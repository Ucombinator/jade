package org.ucombinator.jade.decompile.method

import com.github.javaparser.ast.expr._
import com.github.javaparser.ast.stmt._

object AST {
  def run(graph): BlockStmt = {
    // Requirement, loops must have a single entry point
    // TODO: check for SCCs with multiple entry points
    // TODO: LocalClassDeclarationStmt
    val graph = graph.clone() // TODO: does JGraphT support "delete" adaptor?


    def loop(loopHead: Vertex): (BlockStmt, TreeSet[Insn]/* outOfLoopPending */) = {
      var stmt: BlockStmt

      // worklist of vertexes with no more incoming edges that are inside the current loop (back edges do not count)
      var inLoopPending = TreeSet[Insn](loopHead)

      // worklist of vertexes with no more incoming edges that are outside the current loop (back edges do not count)
      var outOfLoopPending = TreeSet[Insn]()
    // do statements in instruction order if possible
    //   constraints (loops *must* be together)
    // 1. Respect edges
    // 2. Avoid exiting loop
    // 3. Pick following instruction if possible and not goto.
    //    Otherwise, pick the smallest instruction.
    //
    // Any instruction could require a "break" or "continue" attached to it.
    // Only loops are allowed to be continue targets.
    // On loop entry, gather all the instructions for that loop
    //
    // If there is one outgoing edge,
    //   if it is available, use that one.
    //   if it is not available, insert a 'break' and use the smallest available.
    // If there are no outgoing edges, use smallest that is available
    // If there are multiple outgoing edges,
    //   use the next insn if it is an outgoing edge and is available

    // If the next instruction is an outgoing edge via normal control,
    //   if it is available, use it
    //   otherwise, insert a 'break' ('continue' is impossible since we are looking at the next instruction), then do part two
    // If the next instruction is not an outgoing edge,
    //   use the smallest available
      getFirstInsn
      while (Some(insn) <- inLoopPending.minOption) {

        while (insn != null && inLoopPending(insn)) {
          inLoopPending -= insn

          // TODO: monitor enter and exit
          // TODO: try
          val newStmt =
            if (insn.loopHeads.contains(insn)) {
              val (loopBody, newPending) = run(vertex, vertex.loopHeads)
              // TODO: sort newPending by whether in loop
              new Labeled("LOOP"+vertex, new WhileStmt(new TrueExpr(), loopBody))
            } else {
              val (retVal, decompiledInsn) = DecompileInsn.decompileInsn(method, insn, ids)
              DecompileInsn.decompileInsn(retVal, decompileInsn)
            }

          stmt = new BlockStmt(stmt, newStmt)
          val edges = removeOutEdges(insn)

          // TODO: switch
          // TODO: constructors
          insn =
            if (!decompiledInsn.usesNextInsn) {
              null
            } else {
              val next = insns(insn+1)
              assert(edges.contains(next))
              if (inLoopPending(next)) {
                next
              } else {
                stmt = new BlockStmt(stmt, DecompileInsn.decompileInsn(null, DecompiledGoto(next)))
                null
              }
            } 
        }
      }

      return (stmt, outOfLoopPending)
    }

    def stmt(vertex: Vertex): Stmt = {
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

      // TODO: break if falling out of loop
      if (vertex.loopHeads.contains(vertex)) { // loop heads
        val loopBody = go(vertex, vertex.loopHeads)
        val loop = new Labeled("LOOP"+vertex, new WhileStmt(new TrueExpr(), loopBody))
        return loop
      // } else if (tryCatchFinally) {
      //   // TODO
      // } else if (synchronized) {
      //   // TODO
      // } else if (switch) {
      //   // TODO
      } else {
        // TODO: break vs continue: fix via post processing pass?
        val (retVal, decompiled) = DecompileInsn.decompileInsn(method, insn, ids)
        return DecompileInsn.decompileInsn(retVal, decompiled)
      }
    }

    /*
          As long as one is jumping forwards, we can alwyas encode as a sequence of breaks
      Use topo-sort with a sort order that groups loop heads with their body

      use stack (recursion) of zero-in-degree vertices to group loops
      is it a loop head, which loophead is this part of
      */

    def go(var vertex: Vertex, loopHeads): BlockStmt = {
      val pending = Set.empty[Vertex]
      var stmt: BlockStmt

      // TODO: loop end
      while ({ // "Loop and a half" pattern
        val next = stmt(vertex)
        stmt = new BlockStmt(new NodeList(stmt, next))

        vertex = 
        vertex != null // loop condition
      }) {
        graph.update
        pending.update
        stmt = new LabeledStmt("L"+vertex, stmt)
      }
      return stmt
    }

    return go(graph.entry)
  }
}
