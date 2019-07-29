package org.ucombinator.jade.util.jgrapht

import java.util

import org.jgrapht.graph.{AsSubgraph, SimpleDirectedGraph}
import org.jgrapht.{Graph, Graphs}

import scala.collection.JavaConverters._
import scala.collection.immutable
import scala.collection.mutable

object Dominator {
  // Returns a mapping from nodes to the set of nodes that dominate them
  def dominators[V, E](graph: Graph[V, E], start: V): immutable.Map[V, immutable.Set[V]] = {
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


  //Produces an AsSubgraph masking away all non-reachable nodes. Performance of AsSubgraph == ??
  def reachableSubgraph[V <: AnyRef, E](graph: Graph[V, E], start: V) : Graph[V, E] = {
    val reached = new util.HashSet[V]()


    var stack: List[V] = List(start)
    while (stack.nonEmpty) {
      val n = stack.head
      stack = stack.tail
      if (!reached.contains(n)) {
        reached.add(n)

        for (w <- graph.outgoingEdgesOf(n).asScala.map(graph.getEdgeTarget(_))) {
          stack = w :: stack
        }
      }

    }
    new AsSubgraph(graph, reached)
  }

  // Returns a graph with notes pointing to their immediate dominator
  def slowDominatorTree[V <: AnyRef, E](graph: Graph[V, E], start: V): DominatorTree[V] = {
    val dom = dominators(graph, start)
    val tree = new SimpleDirectedGraph[V, Edge[V]](classOf[Edge[V]])

    object O extends Ordering[V] {
      override def compare(x: V, y: V): Int = {
        if (dom(x)(y)) {
          -1
        }
        else if (dom(y)(x)) {
          1
        }
        else {
          0
        }
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
  // drawn from https://gist.github.com/yuzeh/a5e6602dfdb0db3c2130c10537db54d7
  // paper: A Fast Algorithm for Finding Dominators in a Flowgraph, THOMAS LENGAUER and ROBERT ENDRE TARJAN
  // ACM Transactions on Programming Languages and Systems, Vol. 1, No. 1, July 1979, Pages 121-141.
  // https://eden.dei.uc.pt/~amilcar/pdf/CompilerInJava.pdf
  def dominatorTree[V >:Null <: AnyRef, E](graph: Graph[V, E], start: V): DominatorTree[V] = {

    // Original dealt in Ints, not Vs.
    def successors(v: V): Iterable[V] = graph.outgoingEdgesOf(v).asScala.map(graph.getEdgeTarget(_))
    def predecessors(v: V): Iterable[V] = graph.incomingEdgesOf(v).asScala.map(graph.getEdgeSource(_))
    def numNodes: Int = graph.vertexSet().size()

    var N = 0

    val bucket = mutable.Map.empty[V, Set[V]] //buckets of nodes with the same sdom
    for (vertex <- graph.vertexSet().asScala){
      bucket(vertex) = Set.empty[V]
    }


    val dfnum =  mutable.Map.empty[V, Int] //order nodes reached in DFS
    val vertex: mutable.ArraySeq[V] = new mutable.ArraySeq[V](numNodes) //vertex assigned a given number
    val parent =  mutable.Map.empty[V, V] //parent of node in DFS tree
    val semi =  mutable.Map.empty[V, V] //semidominaor of each V
    val ancestor =  mutable.Map.empty[V, V] //used for ancestorWithLowestSemi. Mutable, path compressed
    val idom =  mutable.Map.empty[V, V] //idom (once known)
    val samedom =  mutable.Map.empty[V, V] //node determined to have same idom
    val best =  mutable.Map.empty[V, V]//ancestor of V with lowest semidominator


    //Performs simple DFS, assign numbers and parents
    def dfs(): Unit = {
      var stack: List[(V, V)] = ((null:V), start) :: Nil
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
      val n = vertex(i); val p = parent(n); var s = p //Iterate over nodes from bottom of DFS tree to top.
      //n is vertex, p is parent, s is also parent

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

    //iterate and asign idom based on samedom. Order guarantees idom will be defined in time.
    for (i <- 0 until N) {
      val n = vertex(i)
      if (samedom.contains(n)) {
        idom(n) = idom(samedom(n))
      }
    }

    //algorithm complete; remaining code is just for translatin to expected result structure
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

  //find ancestor with lowest semidenominator
  //If same: Mark idom
  //If not: mark samedom

  //In order, asign idom

}
