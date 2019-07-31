package org.ucombinator.jade.util.jgrapht

import java.io.{StringWriter, Writer}

import org.jgrapht.Graph
import org.jgrapht.io.{ComponentNameProvider, DOTExporter, IntegerComponentNameProvider, StringComponentNameProvider}
import org.objectweb.asm.tree.MethodNode
import org.ucombinator.jade.decompile.method.ControlFlowGraph
import org.ucombinator.jade.asm.Insn

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

  def nestingTree[V,GE,TE](graph: Graph[V,GE], tree: Graph[V,TE], root: V): String = {
    val writer = new StringWriter()
    nestingTree(writer, graph, tree, root)
    writer.toString
  }

  // TODO: flag for flattening chains
  // TODO: alternate background colors
  def nestingTree[V,GE,TE](out: Writer, graph: Graph[V,GE], tree: Graph[V,TE], root: V): Unit = {
    out.write("digraph {\n")
    var cluster = 0
    val ids = mutable.Map[V,String]()
    def id(v: V): String = { ids.getOrElseUpdate(v, "n" + ids.size) }
    def go(indent: String, v: V): Unit = {
      cluster += 1
      // NOTE: subgraph must have a name starting with "cluster" to get GraphViz to draw a box around it
      out.write(indent + s"subgraph cluster${cluster} {\n")
      val label = "\"" + GraphViz.escape(v.toString) + "\""
      out.write(indent + f"  ${id(v)} [ label=$label ];\n")
      for (child <- tree.incomingEdgesOf(v).asScala.map(tree.getEdgeSource)) {
        go(indent + "  ", child)
      }
      out.write(indent + "}\n")
    }
    go("  ", root)
    for (edge <- graph.edgeSet().asScala) {
      // TODO: layout-ignore edges that go to own dominator
      out.write(f"  ${id(graph.getEdgeSource(edge))} -> ${id(graph.getEdgeTarget(edge))};\n")
    }
    out.write("}\n")
  }

}
