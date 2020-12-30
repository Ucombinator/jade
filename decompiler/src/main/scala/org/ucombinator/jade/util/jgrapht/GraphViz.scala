package org.ucombinator.jade.util.jgrapht

import java.io.{StringWriter, Writer}

import org.jgrapht.Graph
import org.jgrapht.nio.DefaultAttribute
import org.jgrapht.nio.dot.DOTExporter
import org.objectweb.asm.tree.MethodNode
import org.ucombinator.jade.asm.Insn
import org.ucombinator.jade.decompile.method.ControlFlowGraph

import scala.collection.JavaConverters._
import scala.collection.mutable

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
    val dotExporter = new DOTExporter[N, E]()
    dotExporter.exportGraph(graph, writer)
  }

  def toString[E](graph: ControlFlowGraph): String = {
    val writer = new StringWriter()
    print(writer, graph)
    writer.toString
  }

  def print[E](writer: Writer, graph: ControlFlowGraph): Unit = {
    val dotExporter = new DOTExporter[Insn, ControlFlowGraph.Edge]()
    dotExporter.setVertexAttributeProvider((v: Insn) => Map("label" -> DefaultAttribute.createAttribute(v.longString)).asJava)
    dotExporter.exportGraph(graph.graph, writer)
  }

  def nestingTree[V,GE,TE](graph: Graph[V,GE], tree: Graph[V,TE], root: V): String = {
    val writer = new StringWriter()
    nestingTree(writer, graph, tree, root)
    writer.toString
  }

  def nestingTree[V,GE,TE](out: Writer, graph: Graph[V,GE], tree: Graph[V,TE], root: V, alternateBackgroundColor: Boolean = true, flatten: Boolean = true): Unit = {
    out.write("digraph {\n")
    var cluster = 0
    val ids = mutable.Map[V,String]()
    def id(v: V): String = { ids.getOrElseUpdate(v, "n" + ids.size) }
    def go(indent: String, v: V, backgroundColor: Boolean, soleChild: Boolean): Unit = {
      cluster += 1
      // NOTE: subgraph must have a name starting with "cluster" to get GraphViz to draw a box around it
      if (!flatten || !soleChild) {
        out.write(indent + s"subgraph cluster$cluster {\n")
        if (alternateBackgroundColor) {
          out.write(indent + f"  bgcolor=${if (backgroundColor) {"\"#eeeeee\""} else {"\"#ffffff\""}};\n")
        }
      }
      val label = "\"" + GraphViz.escape(v.toString) + "\""
      out.write(indent + f"  ${id(v)} [ label=$label ];\n")
      val edges = tree.incomingEdgesOf(v).asScala
      // TODO: edges in trees should always go down
      val sole = edges.toList match {
        case List(x) =>
          val y = tree.getEdgeSource(x)
          graph.outgoingEdgesOf(v).asScala.map(graph.getEdgeTarget) == Set(y) &&
          graph.incomingEdgesOf(y).asScala.map(graph.getEdgeSource) == Set(v)
        case _ =>
          false
      }
      for (child <- edges.map(tree.getEdgeSource)) {
        go(indent + (if (!flatten || !sole) { "  " } else { "" }), child, (flatten && sole) == backgroundColor, sole)
      }
      if (!flatten || !soleChild) {
        out.write(indent + "}\n")
      }
    }
    go("  ", root, false, false)
    for (edge <- graph.edgeSet().asScala) {
      val source = graph.getEdgeSource(edge)
      val target = graph.getEdgeTarget(edge)
      val constraint = !Dominator.isDominator(tree, target, source)
      out.write(f"  ${id(source)} -> ${id(target)} [ constraint=$constraint ];\n")
    }
    out.write("}\n")
  }
}
