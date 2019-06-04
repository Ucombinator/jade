package org.ucombinator.jade.util.jgrapht

import java.io.{StringWriter, Writer}

import org.jgrapht.Graph
import org.jgrapht.io.{ComponentNameProvider, DOTExporter, StringComponentNameProvider}
import org.objectweb.asm.tree.{AbstractInsnNode, InsnList}
import org.ucombinator.jade.decompile.method.ControlFlowGraph
import org.ucombinator.jade.util.asm.Instructions

object GraphViz {
  private class EscapedStringComponentNameProvider[N](quotes: Boolean) extends StringComponentNameProvider[N] {
    override def getName(component: N): String = {
      val s = (component.toString + " " + component.hashCode)
        .replaceAll("\\\\", "\\\\\\\\")
        .replaceAll("\"", "\\\\\"")
      if (quotes) { "\"" + s + "\"" }
      else { s }
    }
  }

  def print[N, E](graph: Graph[N, E]): String = {
    val writer = new StringWriter()
    print(writer, graph)
    writer.toString
  }

  def print[N, E](writer: Writer, graph: Graph[N, E]): Unit = {
    val dotExporter = new DOTExporter[N, E](
      new EscapedStringComponentNameProvider[N](true),
      null,
      null
    )
    dotExporter.exportGraph(graph, writer)
  }

  private class AbstractInsnComponentNameProvider(insnList: InsnList) extends ComponentNameProvider[AbstractInsnNode] {
    override def getName(component: AbstractInsnNode): String = {
      Instructions.longInsnString(insnList, component)
    }
  }

  def print[E](graph: ControlFlowGraph): String = {
    val writer = new StringWriter()
    print(writer, graph)
    writer.toString
  }

  def print[E](writer: Writer, graph: ControlFlowGraph): Unit = {
    val dotExporter = new DOTExporter[AbstractInsnNode, ControlFlowGraph.Edge](
      new EscapedStringComponentNameProvider[AbstractInsnNode](true),
      new AbstractInsnComponentNameProvider(graph.method.instructions),
      null
    )
    dotExporter.exportGraph(graph.graph, writer)
  }
}
