package org.ucombinator.jade.decompile.methodbody

import scala.collection.immutable._
import com.github.javaparser.ast.NodeList
import com.github.javaparser.ast.expr._
import com.github.javaparser.ast.stmt._

import org.jgrapht.graph._

import org.ucombinator.jade.asm.Insn
import org.ucombinator.jade.decompile.DecompileInsn
import org.ucombinator.jade.decompile.methodbody.ssa.SSA
import org.ucombinator.jade.asm.Insn.InsnOrdering
import org.ucombinator.jade.util.Errors

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

object Stmt {
  /*
  As long as one is jumping forwards, we can always encode as a sequence of breaks
  Use topo-sort with a sort order that groups loop heads with their body

  use stack (recursion) of zero-in-degree vertices to group loops
  is it a loop head, which loop head is this part of
  */

  def run(cfg: ControlFlowGraph, ssa: SSA, structures: Map[Insn, Structure]): Statement = {
    // TODO: check for SCCs with multiple entry points
    // TODO: LocalClassDeclarationStmt

    // TODO: remove back edges
    val graph = new AsSubgraph(new MaskSubgraph(cfg.graph, (v: Insn) => true, (e: ControlFlowGraph.Edge) => e.isForwardEdge))

    def structuredBlock(head: Insn): (Statement, Set[Insn]/* pendingOutside */) = {
      // do statements in instruction order if possible
      // constraints (loops *must* be together):
      // 1. Respect edges
      // 2. Loop instructions must be together (thus avoid exiting loop)
      // 3. Pick following instruction if possible and not goto.
      //    Otherwise, pick the smallest instruction.
      //
      // Any instruction could require a "break" or "continue" attached to it.
      // Only loops are allowed to be continue targets.

      val headStructure = structures(head)

      // worklist of vertexes with no more incoming edges that are inside the current loop (back edges do not count)
      // NOTE: We use TreeSet so we have `minOption()`
      var pendingInside = TreeSet[Insn]()

      // worklist of vertexes with no more incoming edges that are outside the current loop (back edges do not count)
      var pendingOutside = Set[Insn]()

      def addPending(insns: Set[Insn]): Unit = {
        for (insn <- insns) {
          assert(graph.inDegreeOf(insn) == 0)
          if (structures(insn) >= headStructure) {
            pendingInside += insn
          } else {
            pendingOutside += insn
          }
        }
      }

      def removeOutEdges(insn: Insn): Set[Insn] = {
        val outEdges = graph.outEdges(insn).asScala
        val targets = outEdges.map(e => graph.getEdgeTarget(e))
        outEdges.foreach(e => graph.removeEdge(e))
        addPending(targets.filter(e => graph.inDegreeOf(e) == 0))
        outEdges
      }

      // ASSUMPTION: structured statements have a single entry point
      def structuredStmt(insn: Insn): Statement = {
        if (structures(insn).head eq insn) { // insn is the head of a structured statement
          // TODO: multiple nested structures starting at same place (for now assume everything is a loop)
          val (block, newPending) = structuredBlock(insn)
          addPending(newPending)
          return new LabeledStmt("LOOP" + insn.index, new WhileStmt(new BooleanLiteralExpr(true), block))
        // } else if (tryCatchFinally) {
        //   // TODO
        // } else if (synchronized) {
        //   // TODO
        } else {
          return simpleStmt(insn)
        }
      }

      def simpleStmt(insn: Insn): Statement = {
        // ASSUMPTION: we ignore allocs but implement the constructors
        val (retVal, decompiled) = DecompileInsn.decompileInsn(insn.insn, ssa)
        // TODO: break vs continue
        // TODO: labels in break or continue
        return DecompileInsn.decompileInsn(retVal, decompiled)
      }
      // TODO: explicitly labeled instructions

      var currentInsn: Insn = head
      // If the next instruction is an outgoing edge via normal control,
      //   if it is available, use it
      //   otherwise, insert a 'break' ('continue' is impossible since we are looking at the next instruction), then do part two
      // If the next instruction is not an outgoing edge,
      //   use the smallest available
      def getNextInsn(): Insn = {
        // TODO: switch?
        // TODO: constructor?
        val outEdges = removeOutEdges(currentInsn)
        val nextInsn =
          if (!currentInsn.usesNextInsn) {
            // Use the smallest available instruction
            pendingInside.minOption
          } else {
            // ASSUMPTION: last Insn in method does not use next
            val next = currentInsn.getNext
            assert(outEdges.contains(next))
            if (pendingInside(next)) {
              // Use the next instruction
              Some(next)
            } else {
              currentStmt = new BlockStmt(new NodeList[Statement](currentStmt, new BreakStmt(next)))
              // Use the smallest available instruction
              pendingInside.minOption
            }
          }
        nextInsn match {
          case Some(insn) => insn
          case None => null
        }
      }

      var currentStmt: Statement = simpleStmt(currentInsn)
      while ({ currentInsn = getNextInsn(); currentInsn != null }) {
        pendingInside -= currentInsn

        currentStmt = new BlockStmt(new NodeList[Statement](currentStmt, structuredStmt(currentInsn)))

        if (currentInsn.jumpTarget) {
          currentStmt = new LabeledStmt("L" + currentInsn.index, currentStmt)
        }
      }

      return (currentStmt, pendingOutside)
    }

    var (stmt, pendingOutside) = structuredBlock(graph.entry)
    if (!pendingOutside.isEmpty) { Errors.fatal(f"Non-empty pending ${pendingOutside}") }
    return stmt
  }
}
