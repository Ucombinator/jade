package org.ucombinator.jade.util.jgrapht

import java.io.{StringWriter, Writer}

import org.jgrapht.Graph
import org.jgrapht.io.{ComponentNameProvider, DOTExporter, IntegerComponentNameProvider, StringComponentNameProvider}
import org.objectweb.asm.tree.MethodNode
import org.ucombinator.jade.decompile.method.ControlFlowGraph
import org.ucombinator.jade.util.asm.Insn

object GraphViz {
  def escape(string: String): String = {
    string
      .replaceAll("\\\\", "\\\\\\\\")
      .replaceAll("\"", "\\\\\"")
  }

  def toString[N, E](graph: Graph[N, E]): String = {
    val writer = new StringWriter()
    print(writer, graph)
    writer.toString
  }

  def print[N, E](writer: Writer, graph: Graph[N, E]): Unit = {
    val dotExporter = new DOTExporter[N, E](
      new IntegerComponentNameProvider(),
      new StringComponentNameProvider(),
      null
    )
    dotExporter.exportGraph(graph, writer)
  }

  def toString[E](graph: ControlFlowGraph): String = {
    val writer = new StringWriter()
    print(writer, graph)
    writer.toString
  }

  def print[E](writer: Writer, graph: ControlFlowGraph): Unit = {
    val dotExporter = new DOTExporter[Insn, ControlFlowGraph.Edge](
      new IntegerComponentNameProvider(),
      new AbstractInsnComponentNameProvider(graph.method),
      null
    )
    dotExporter.exportGraph(graph.graph, writer)
  }

  private class AbstractInsnComponentNameProvider(method: MethodNode) extends ComponentNameProvider[Insn] {
    override def getName(component: Insn): String = { component.longString }
  }
}
