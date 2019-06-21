package org.ucombinator.jade.decompile.method

import org.jgrapht.graph.DirectedPseudograph
import org.objectweb.asm.tree.analysis.{Analyzer, BasicInterpreter, BasicValue, Frame}
import org.objectweb.asm.tree.MethodNode
import org.ucombinator.jade.util.asm.Insn

case class ControlFlowGraph(
  method: MethodNode,
  graph: DirectedPseudograph[Insn, ControlFlowGraph.Edge],
  frames: Array[Frame[BasicValue]]) {
  val entry = Insn(method, method.instructions.getFirst)
}

case object ControlFlowGraph {
  def apply(owner: String, method: MethodNode): ControlFlowGraph = {
    val graph = new DirectedPseudograph[Insn, Edge](classOf[Edge])
    for (i <- method.instructions.toArray) {
      graph.addVertex(Insn(method, i))
    }
    val analyzer = new ControlFlowGraphAnalyzer(method, graph)
    val frames = analyzer.analyze(owner, method)
    ControlFlowGraph(method, graph, frames)
  }

  final case class Edge(source: Insn, target: Insn)

  class ControlFlowGraphAnalyzer(
    method: MethodNode,
    graph: DirectedPseudograph[Insn, Edge])
    extends Analyzer[BasicValue](new BasicInterpreter) {

    override protected def newControlFlowEdge(insn: Int, successor: Int): Unit = {
      val source = Insn(method, this.method.instructions.get(insn))
      val target = Insn(method, this.method.instructions.get(successor))
      this.graph.addEdge(source, target, Edge(source, target))
    }

    // Default handling (which returns true) is sufficient
    //override protected def newControlFlowExceptionEdge(insn: Int, successor: Int): Boolean = ??? // Should never be called

    //override protected def newControlFlowExceptionEdge(insn: Int, successor: TryCatchBlockNode): Boolean = {
    //  this.handlers += successor
    //  true // the edge must always be considered by the analyzer
    //}
  }
}
