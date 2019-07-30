package org.ucombinator.jade.decompile.method

import org.jgrapht.Graph
import org.jgrapht.graph.{AsGraphUnion, DirectedPseudograph}
import org.objectweb.asm.tree.MethodNode
import org.objectweb.asm.tree.analysis.{Analyzer, BasicInterpreter, BasicValue, Frame}
import org.ucombinator.jade.decompile.method.ControlFlowGraph.Edge
import org.ucombinator.jade.asm.Insn

import scala.collection.JavaConverters._

case class ControlFlowGraph(
  method: MethodNode,
  graph: DirectedPseudograph[Insn, ControlFlowGraph.Edge],
  frames: Array[Frame[BasicValue]]) {
  val entry = Insn(method, method.instructions.getFirst)
  val graphWithExceptions: Graph[Insn, Edge] = {
    val g = new DirectedPseudograph[Insn, Edge](classOf[Edge])
    for (handler <- method.tryCatchBlocks.asScala) {
      val s = Insn(method, handler.start)
      val h = Insn(method, handler.handler)
      g.addVertex(s)
      g.addVertex(h)
      g.addEdge(s, h, Edge(s, h))
    }
    new AsGraphUnion(graph, g)
  }
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
  }
}
