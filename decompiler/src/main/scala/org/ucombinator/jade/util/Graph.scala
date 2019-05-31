package org.ucombinator.jade.util

import java.io.Writer

import org.jgrapht.io.{DOTExporter, StringComponentNameProvider}
import org.jgrapht.{Graphs, Graph => JGraph}

import scala.collection.JavaConverters._
import scala.collection.immutable

object Graph {
  class EscapedStringComponentNameProvider[N](quotes: Boolean) extends StringComponentNameProvider[N] {
    override def getName(component: N): String = {
      val s = (component.toString + " " + component.hashCode)
        .replaceAll("\\\\", "\\\\\\\\")
        .replaceAll("\"", "\\\\\"")
      if (quotes) { "\"" + s + "\""}
      else { s }
    }
  }

  def print[N, E](writer: Writer, graph: JGraph[N, E]): Unit = {
    val dotExporter = new DOTExporter[N, E](
      new EscapedStringComponentNameProvider[N](true),
      null,
      null
    )
    dotExporter.exportGraph(graph, writer)
  }

  // Returns a mapping from nodes to the set of nodes that dominate them
  def dominators[V,E](graph: JGraph[V,E], start: V): immutable.Map[V, immutable.Set[V]] = {
    val vs = graph.vertexSet.asScala.toSet
    var dom: immutable.Map[V, immutable.Set[V]]  = Map.empty

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

  // Returns a mapping from nodes to the node that is its immediate dominator
  def immediateDominators[V,E](graph: JGraph[V,E], start: V): immutable.Map[V, V] = {
    val dom = dominators(graph, start)

    object O extends Ordering[V] {
      override def compare(x: V, y: V): Int = {
        if (dom(x)(y)) -1
        else if (dom(y)(x)) 1
        else 0
      }
    }

    dom.mapValues(_.min(O))
  }

  //structure of region graph?
  // Type region:
  // | Single of Vertex
  // | Graph [regions, edge]

  // Definition subregion: All nodes that share a pre and post dominator?

}
