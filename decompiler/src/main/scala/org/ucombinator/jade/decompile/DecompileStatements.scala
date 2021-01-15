package org.ucombinator.jade.decompile

import scala.collection.immutable._
import scala.jdk.CollectionConverters._

import com.github.javaparser.ast.NodeList
import com.github.javaparser.ast.expr._
import com.github.javaparser.ast.stmt._
import org.jgrapht.graph._
import org.ucombinator.jade.asm.Insn
import org.ucombinator.jade.asm.Insn.ordering
import org.ucombinator.jade.decompile._
import org.ucombinator.jade.analysis.Structure
import org.ucombinator.jade.analysis.StaticSingleAssignment
import org.ucombinator.jade.util.Errors
import org.ucombinator.jade.util.Log
import org.ucombinator.jade.util.MyersList
import com.github.javaparser.ast.body.VariableDeclarator
import com.github.javaparser.ast.`type`.PrimitiveType
import com.github.javaparser.ast.`type`.{Type => JavaParserType}
import org.objectweb.asm.{Type => AsmType}
import org.objectweb.asm.tree.LabelNode
import org.ucombinator.jade.analysis.Var
import org.ucombinator.jade.analysis.ControlFlowGraph
import org.ucombinator.jade.classfile.Descriptor
import org.ucombinator.jade.util.JavaParser
import com.github.javaparser.ast.comments.BlockComment

/*
Non-Linear Stmt Types
  -Break
  -Continue
  -Return/Throw
  +Try-Catch-Finally
  +Synchronized
  +Do/While/For/For-each
  +If/Switch
Non-linear expressions
  Boolean(&&/||/!/==/!=/</>/<=/>=)
  Trinary Operator/Switch Expression
 */

// TODO: rename to Statement
object DecompileStatements extends Log {
  /*
  As long as one is jumping forwards, we can always encode as a sequence of breaks
  Use topo-sort with a sort order that groups loop heads with their body

  use stack (recursion) of zero-in-degree vertices to group loops
  is it a loop head, which loop head is this part of
   */

  def apply(cfg: ControlFlowGraph, ssa: StaticSingleAssignment, structure: Structure): BlockStmt = {
    // TODO: check for SCCs with multiple entry points
    // TODO: LocalClassDeclarationStmt
    val jumpTargets = cfg.graph // TODO: rename to insnOfLabel
      .vertexSet()
      .asScala
      .flatMap({ insn =>
        insn.insn match {
          case e: LabelNode => Set(e.getLabel -> insn)
          case _ => Set()
        }
      })
      .toMap

    // TODO: remove back edges
    val graph = new AsSubgraph(
      new MaskSubgraph(cfg.graph, (_: Insn) => false, (e: ControlFlowGraph.Edge) => structure.backEdges(e))
    )

    def labelString(label: LabelNode): String = { "JADE_" + jumpTargets(label.getLabel()).index }
    def insnLabelString(insn: Insn): String = { "JADE_" + insn.index } // TODO: overload with labelString

    def structuredBlock(head: Insn): (Statement, Set[Insn] /* pendingOutside */ ) = {
      // do statements in instruction order if possible
      // constraints (loops *must* be together):
      // 1. Respect edges
      // 2. Loop instructions must be together (thus avoid exiting loop)
      // 3. Pick following instruction if possible and not goto.
      //    Otherwise, pick the smallest instruction.
      //
      // Any instruction could require a "break" or "continue" attached to it.
      // Only loops are allowed to be continue targets.

      val headStructure = structure.nesting(head)

      // worklist of vertexes with no more incoming edges that are inside the current loop (back edges do not count)
      // NOTE: We use TreeSet so we have `minOption()`
      var pendingInside = TreeSet[Insn]()

      // worklist of vertexes with no more incoming edges that are outside the current loop (back edges do not count)
      var pendingOutside = Set[Insn]()

      def addPending(insns: Set[Insn]): Unit = {
        for (insn <- insns) {
          assert(graph.inDegreeOf(insn) == 0)
          if (MyersList.partialOrdering.gteq(structure.nesting(insn), headStructure)) {
            pendingInside += insn
          } else {
            pendingOutside += insn
          }
        }
      }

      def removeOutEdges(insn: Insn): Set[Insn] = {
        val outEdges = Set.from(graph.outgoingEdgesOf(insn).asScala)
        val targets = outEdges.map(e => graph.getEdgeTarget(e))
        outEdges.foreach(e => graph.removeEdge(e))
        addPending(targets.filter(e => graph.inDegreeOf(e) == 0))
        outEdges.map(e => graph.getEdgeTarget(e))
      }

      // ASSUMPTION: structured statements have a single entry point
      def structuredStmt(insn: Insn): Statement = {
        val block = structure.nesting(insn).head
        if (block.headInsn eq insn) { // insn is the head of a structured statement
          // TODO: multiple nested structures starting at same place (for now assume everything is a loop)
          val (stmt, newPending) = structuredBlock(insn)
          addPending(newPending)
          block.kind match {
            case Structure.Loop() =>
              val label = labelString(insn.insn.asInstanceOf[LabelNode]) // TODO: do better
              new LabeledStmt(label, new WhileStmt(new BooleanLiteralExpr(true), stmt))
            case Structure.Exception() => ???
            case Structure.Synchronized() => ???
          }
        } else {
          return simpleStmt(insn)
        }
      }

      def simpleStmt(insn: Insn): Statement = {
        // ASSUMPTION: we ignore allocs but implement the constructors
        val (retVal, decompiled) = DecompileInsn.decompileInsn(insn.insn, ssa)
        decompiled match {
          case DecompiledIf(labelNode, condition) =>
            this.log.debug("IF: " + labelNode + "///" + labelNode.getLabel)
            new IfStmt(condition, new BreakStmt(labelString(labelNode)), null)
          case DecompiledGoto(labelNode: LabelNode) =>
            new BreakStmt(labelString(labelNode)) // TODO: use instruction number?
          case _ => return DecompileInsn.decompileInsn(retVal, decompiled)
        }
        // TODO: break vs continue
        // TODO: labels in break or continue
      }
      // TODO: explicitly labeled instructions

      var currentInsn: Insn = head
      var currentStmt: Statement = simpleStmt(currentInsn)
      // If the next instruction is an outgoing edge via normal control,
      //   if it is available, use it
      //   otherwise, insert a 'break' ('continue' is impossible since we are looking at the next instruction), then do part two
      // If the next instruction is not an outgoing edge,
      //   use the smallest available
      def getNextInsn(): Insn = {
        // TODO: switch?
        // TODO: constructor?
        val outEdges = removeOutEdges(currentInsn)
        val (_, decompiled) = DecompileInsn.decompileInsn(currentInsn.insn, ssa)
        val nextInsn =
          if (!decompiled.usesNextInsn) {
            // Use the smallest available instruction
            pendingInside.minOption
          } else {
            // ASSUMPTION: last Insn in method does not use next
            val next = currentInsn.next
            assert(outEdges.contains(next))
            if (pendingInside(next)) {
              // Use the next instruction
              Some(next)
            } else {
              // NOTE [Branch Targets]:
              // We can't go to the sequencially next instruction (probably due to a CFG dependency)
              // so we insert a `break` and pick the next instruction that we can go to.
              // This line is why every statement is a potential break target.
              currentStmt = new BlockStmt(new NodeList[Statement](currentStmt, new BreakStmt(insnLabelString(next))))
              // Use the smallest available instruction
              pendingInside.minOption
            }
          }
        nextInsn.orNull
      }

      while ({ currentInsn = getNextInsn(); currentInsn != null }) {
        pendingInside -= currentInsn
        currentStmt = new LabeledStmt(
          insnLabelString(currentInsn),
          new BlockStmt(new NodeList[Statement](currentStmt, structuredStmt(currentInsn)))
        )
      }

      return (currentStmt, pendingOutside)
    }

    val (stmt, pendingOutside) = structuredBlock(cfg.entry)
    if (!pendingOutside.isEmpty) { Errors.fatal(f"Non-empty pending ${pendingOutside}") }
    val variables = ssa.insnVars.values.map(_._1) ++ ssa.phiInputs.keys
    def decompileVarDecl(v: Var): Statement = {
      // TODO: modifiers
      if (v.basicValue == null || v.basicValue.getType() == null) {
        JavaParser.noop(f"${v}")
      } else {
        val t = Descriptor.fieldDescriptor(v.basicValue.getType().getDescriptor())
        new ExpressionStmt(new VariableDeclarationExpr(t, v.name))
      }
    }
    val declarations = variables.map(decompileVarDecl)

    val statements = new NodeList[Statement](declarations.toList.asJava)
    statements.add(stmt)
    val stmt2 = new BlockStmt(statements)
    return stmt2
  }
}
