package org.ucombinator.jade.util.jgrapht

import org.jgrapht.Graph
import org.jgrapht.graph.SimpleDirectedGraph

import scala.collection.JavaConverters._
import scala.collection.mutable

object Dominator {
  final case class Edge[V](source: V, target: V)

  type DominatorTree[V] = Graph[V, Edge[V]]

  // TODO: if missing node
  // drawn from https://gist.github.com/yuzeh/a5e6602dfdb0db3c2130c10537db54d7
  // paper: A Fast Algorithm for Finding Dominators in a Flowgraph, THOMAS LENGAUER and ROBERT ENDRE TARJAN
  // ACM Transactions on Programming Languages and Systems, Vol. 1, No. 1, July 1979, Pages 121-141.
  // https://eden.dei.uc.pt/~amilcar/pdf/CompilerInJava.pdf
  def dominatorTree[V >:Null <: AnyRef, E](graph: Graph[V, E], start: V): DominatorTree[V] = {

    // Original dealt in Ints, not Vs.
    def successors(v: V): Iterable[V] = graph.outgoingEdgesOf(v).asScala.map(graph.getEdgeTarget)
    def predecessors(v: V): Iterable[V] = graph.incomingEdgesOf(v).asScala.map(graph.getEdgeSource)
    def numNodes: Int = graph.vertexSet().size()

    var N = 0

    val bucket = mutable.Map.empty[V, Set[V]] //buckets of nodes with the same sdom
    for (vertex <- graph.vertexSet().asScala){
      bucket(vertex) = Set.empty[V]
    }

    val dfnum = mutable.Map.empty[V, Int] //order nodes reached in DFS
    val vertex: mutable.ArraySeq[V] = new mutable.ArraySeq[V](numNodes) //vertex assigned a given number
    val parent = mutable.Map.empty[V, V] //parent of node in DFS tree
    val semi = mutable.Map.empty[V, V] //semidominaor of each V
    val ancestor = mutable.Map.empty[V, V] //used for ancestorWithLowestSemi. Mutable, path compressed
    val idom = mutable.Map.empty[V, V] //idom (once known)
    val samedom = mutable.Map.empty[V, V] //node determined to have same idom
    val best = mutable.Map.empty[V, V]//ancestor of V with lowest semidominator

    //Performs simple DFS, assign numbers and parents
    def dfs(): Unit = {
      var stack: List[(V, V)] = List((null: V, start))
      while (stack.nonEmpty) {
        val (p, n) = stack.head
        stack = stack.tail
        if (!dfnum.contains(n)) {
          dfnum(n) = N
          vertex(N) = n
          parent(n) = p
          N += 1
          for (w <- successors(n)) {
            stack = (n, w) :: stack
          }
        }
      }
    }

    //Finds the ancestor of v with the lowest semidominator. Uses path compression to keep runtime down
    def ancestorWithLowestSemi(v: V): V = {
      val a = ancestor(v) //ancestor initially means parent; only modified here
      if (ancestor.contains(a)) { //if defined
        val b = ancestorWithLowestSemi(a)
        ancestor(v) = ancestor(a)
        if (dfnum(semi(b)) < dfnum(semi(best(v)))) {
          best(v) = b
        }
      }
      best(v)
    }

    //helper function: p is parent of n
    def link(p: V, n: V): Unit = {
      ancestor(n) = p
      best(n) = n
    }

    //Start of execution
    dfs() //setup DFS tree

    for (i <- (N - 1) until 0 by -1) {
      //Iterate over nodes from bottom of DFS tree to top.
      val n = vertex(i)
      val p = parent(n)
      var s = p

      //find the semidominator of v
      for (v <- predecessors(n)) {
        val sPrime = if (dfnum(v) <= dfnum(n)) { //determines if pred is ancestor in DFS tree.
          v
        } else {
          semi(ancestorWithLowestSemi(v))
        }
        if (dfnum(sPrime) < dfnum(s)) { //picks lowest
          s = sPrime
        }
      }

      semi(n) = s
      bucket(s) = bucket(s) + n

      link(p, n)

      //for each bucket, find ancestor with lowest semi. If it has the same semi, that semi is the idom. If not, it has the same semidominator.
      for (v <- bucket(p)) {
        val y = ancestorWithLowestSemi(v)
        if (semi(y) == semi(v)) {
          idom(v) = p
        } else {
          samedom(v) = y
        }
      }
      bucket(p) = Set.empty
    }

    //iterate and assign idom based on samedom. Order guarantees idom will be defined in time.
    for (i <- 0 until N) {
      val n = vertex(i)
      if (samedom.contains(n)) {
        idom(n) = idom(samedom(n))
      }
    }

    //algorithm complete; remaining code is just for translation to expected result structure
    val tree = new SimpleDirectedGraph[V, Edge[V]](classOf[Edge[V]])
    tree.addVertex(start) //suspicious: may or may not be key in idom
    for ((key, value) <- idom) {
      tree.addVertex(key)
    }
    for ((key, value) <- idom) {
      tree.addEdge(key, value, Edge(key, value))
    }
    tree
  }
}
