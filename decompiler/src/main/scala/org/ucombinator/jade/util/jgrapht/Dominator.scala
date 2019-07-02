package org.ucombinator.jade.util.jgrapht

import java.io.{StringWriter, Writer}

import org.jgrapht.graph.SimpleDirectedGraph
import org.jgrapht.{Graph, Graphs}

import scala.collection.JavaConverters._
import scala.collection.{immutable, mutable}

object Dominator {
  // Returns a mapping from nodes to the set of nodes that dominate them
  def dominators[V,E](graph: Graph[V,E], start: V): immutable.Map[V, immutable.Set[V]] = {
    val vs = graph.vertexSet.asScala.toSet
    var dom: immutable.Map[V, immutable.Set[V]] = Map.empty

    // Initial assignment
    dom = dom + (start -> immutable.Set(start))
    for (v <- vs if v != start) {
      dom = dom + (v -> vs)
    }

    // Iteration until fixed point
    var old_dom = dom
    do {
      old_dom = dom
      for (v <- vs if v != start) {
        dom = dom +
          (v -> (
            immutable.Set(v) ++
              Graphs.predecessorListOf(graph, v).
                asScala.map(dom).
                fold(vs)(_ & _)))
      }
    } while (old_dom != dom)

    dom
  }

  final case class Edge[V](source: V, target: V)

  type DominatorTree[V] = Graph[V, Edge[V]]
  // Returns a graph with notes pointing to their immediate dominator
  def dominatorTree[V <: AnyRef, E](graph: Graph[V,E], start: V): DominatorTree[V] = {
    val dom = dominators(graph, start)
    val tree = new SimpleDirectedGraph[V, Edge[V]](classOf[Edge[V]])

    object O extends Ordering[V] {
      override def compare(x: V, y: V): Int = {
        if (dom(x)(y)) { -1 }
        else if (dom(y)(x)) { 1 }
        else { 0 }
      }
    }

    for (v <- graph.vertexSet().asScala) {
      tree.addVertex(v)
    }

    for ((k, vs) <- dom) {
      val vs2 = vs - k
      if (vs2.nonEmpty) {
        val v = vs2.min(O)
        tree.addEdge(k, v, Edge(k, v))
      }
    }

    tree
  }
  // TODO: if missing node

  def dominatorNesting[V,E](graph: Graph[V,E], tree: DominatorTree[V], root: V): String = {
    val writer = new StringWriter()
    dominatorNesting(writer, graph, tree, root)
    writer.toString
  }

// TODO: unique edge type so types are distinct
// TODO: alternate background colors
  def dominatorNesting[V,E](out: Writer, graph: Graph[V,E], tree: DominatorTree[V], root: V): Unit = {
    out.write("digraph {\n")
    var cluster = 0
    val ids = mutable.Map[V,String]()
    def id(v: V): String = { ids.getOrElseUpdate(v, "n" + ids.size) }
    def go(indent: String, v: V): Unit = {
      cluster += 1
      out.write(indent + s"subgraph cluster${cluster} {\n") // NOTE: subgraph must have a name starting with "cluster" to get GraphViz to draw a box around it
      val label = "\"" + GraphViz.escape(v.toString) + "\""
      out.write(indent + f"  ${id(v)} [ label=$label ];\n")
      for (child <- tree.incomingEdgesOf(v).asScala.map(tree.getEdgeSource)) {
        go(indent + "  ", child)
      }
      out.write(indent + "}\n")
    }
    go("", root)
    for (edge <- graph.edgeSet().asScala) {
      // TODO: layout-ignore edges that go to own dominator
      out.write(f"${id(graph.getEdgeSource(edge))} -> ${id(graph.getEdgeTarget(edge))};\n")
    }
    out.write("}\n")
  }
}
