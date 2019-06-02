package org.ucombinator.jade.util.jgrapht

import org.jgrapht.Graph
import org.jgrapht.io.{DOTExporter, StringComponentNameProvider}

import java.io.{StringWriter, Writer}

object GraphViz {
  private class EscapedStringComponentNameProvider[N](quotes: Boolean) extends StringComponentNameProvider[N] {
    override def getName(component: N): String = {
      val s = (component.toString + " " + component.hashCode)
        .replaceAll("\\\\", "\\\\\\\\")
        .replaceAll("\"", "\\\\\"")
      if (quotes) { "\"" + s + "\""}
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
}
