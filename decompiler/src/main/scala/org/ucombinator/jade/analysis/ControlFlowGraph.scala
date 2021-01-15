package org.ucombinator.jade.analysis

import scala.jdk.CollectionConverters._

import org.jgrapht.Graph
import org.jgrapht.graph.AsGraphUnion
import org.jgrapht.graph.DirectedPseudograph
import org.objectweb.asm.tree.MethodNode
import org.objectweb.asm.tree.analysis.Analyzer
import org.objectweb.asm.tree.analysis.BasicValue
import org.objectweb.asm.tree.analysis.Frame
import org.ucombinator.jade.asm.Insn
import org.ucombinator.jade.asm.TypedBasicInterpreter

case class ControlFlowGraph(
    entry: Insn,
    graph: DirectedPseudograph[Insn, ControlFlowGraph.Edge],
    graphWithExceptions: Graph[Insn, ControlFlowGraph.Edge],
    frames: Array[Frame[BasicValue]])

case object ControlFlowGraph {
  def apply(owner: String, method: MethodNode): ControlFlowGraph = {
    val graph = new DirectedPseudograph[Insn, Edge](classOf[Edge])
    for (i <- method.instructions.toArray) {
      graph.addVertex(Insn(method, i))
    }
    val analyzer = new ControlFlowGraphAnalyzer(method, graph)
    val frames = analyzer.analyze(owner, method)
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
    ControlFlowGraph(entry, graph, graphWithExceptions, frames)
  }

  final case class Edge(source: Insn, target: Insn)

  class ControlFlowGraphAnalyzer(method: MethodNode, graph: DirectedPseudograph[Insn, Edge])
      extends Analyzer[BasicValue](TypedBasicInterpreter) {

    override protected def newControlFlowEdge(insn: Int, successor: Int): Unit = {
      val source = Insn(method, this.method.instructions.get(insn))
      val target = Insn(method, this.method.instructions.get(successor))
      this.graph.addEdge(source, target, Edge(source, target))
      ()
    }
  }
}
