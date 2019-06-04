package org.ucombinator.jade.decompile.method

import org.jgrapht.graph.DirectedPseudograph
import org.objectweb.asm.tree.analysis.{Analyzer, BasicInterpreter, BasicValue, Frame}
import org.objectweb.asm.tree.{MethodNode, TryCatchBlockNode}
import org.ucombinator.jade.util.asm.Insn

import scala.collection.mutable

case class ControlFlowGraph(
  method: MethodNode,
  graph: DirectedPseudograph[Insn, ControlFlowGraph.Edge],
  handlers: Set[TryCatchBlockNode],
  frames: Array[Frame[BasicValue]])

case object ControlFlowGraph {
  def apply(owner: String, method: MethodNode): ControlFlowGraph = {
    val edges = new DirectedPseudograph[Insn, Edge](classOf[Edge])
    for (i <- method.instructions.toArray) {
      edges.addVertex(Insn(method, i))
    }
    val handlers = mutable.Set[TryCatchBlockNode]()
    val analyzer = new ControlFlowGraphAnalyzer(method, edges, handlers)
    val frames = analyzer.analyze(owner, method)
    ControlFlowGraph(method, edges, handlers.toSet, frames)
  }

  final case class Edge(source: Insn, target: Insn)

  class ControlFlowGraphAnalyzer(
    method: MethodNode,
    edges: DirectedPseudograph[Insn, Edge],
    handlers: mutable.Set[TryCatchBlockNode])
    extends Analyzer[BasicValue](new BasicInterpreter) {

    override protected def newControlFlowEdge(insn: Int, successor: Int): Unit = {
      val source = Insn(method, this.method.instructions.get(insn))
      val target = Insn(method, this.method.instructions.get(successor))
      this.edges.addEdge(source, target, Edge(source, target))
    }

    override protected def newControlFlowExceptionEdge(insn: Int, successor: Int): Boolean = ??? // Should never be called

    override protected def newControlFlowExceptionEdge(insn: Int, successor: TryCatchBlockNode): Boolean = {
      this.handlers += successor
      true // the edge must always be considered by the analyzer
    }
  }
}
