package org.ucombinator.jade.util.jgrapht

import org.jgrapht.graph.{DefaultEdge, SimpleDirectedGraph}
import org.jgrapht.{Graph, Graphs}

import scala.collection.JavaConverters._
import scala.collection.immutable

/*

http://hg.openjdk.java.net/jdk9/jdk9/raw-file/tip/common/doc/testing.html
https://hg.openjdk.java.net/jdk/jdk/raw-file/tip/doc/testing.html
https://en.wikipedia.org/wiki/JavaTest_harness
https://en.wikipedia.org/wiki/Technology_Compatibility_Kit

Recovering clear, natural identifiers from obfuscated JS names
  https://www.semanticscholar.org/paper/Recovering-clear%2C-natural-identifiers-from-JS-names-Vasilescu-Casalnuovo/53da3749987b3a4d166449e32af7b474ce776096
Helping Johnny to Analyze Malware: A Usability-Optimized Decompiler and Malware Analysis User Study
  https://www.semanticscholar.org/paper/Helping-Johnny-to-Analyze-Malware%3A-A-Decompiler-and-Yakdan-Dechand/12945913b496ff349f65565d8b65208147ed3794
Native x86 Decompilation Using Semantics-Preserving Structural Analysis and Iterative Control-Flow Structuring
  https://www.semanticscholar.org/paper/Native-x86-Decompilation-Using-Semantics-Preserving-Brumley-Lee/16981623b83e5d8b927a9e1f240ce568f2587862
Static Single Assignment for Decompilation
  https://www.semanticscholar.org/paper/Static-Single-Assignment-for-Decompilation-Emmerik/6181cec77dc6fc26973f8d0386d587cf6978609a
Structural analysis: A new approach to flow analysis in optimizing compilers
  https://www.sciencedirect.com/science/article/pii/0096055180900077

The Isomorphism Problem For Directed Path Graphs and For Rooted Directed Path Graphs
  https://www.sciencedirect.com/science/article/pii/S0196677496900589
https://stackoverflow.com/questions/11338746/directed-graphs-with-a-given-root-node-match-another-directed-graph-for-equali

https://en.wikipedia.org/wiki/Dominator_(graph_theory)
https://tanujkhattar.wordpress.com/2016/01/11/dominator-tree-of-a-directed-graph/

A fast algorithm for finding dominators in a flowgraph
  https://dl.acm.org/citation.cfm?id=357062.357071
  https://www.wikidata.org/wiki/Q55934256
  https://scholar.google.com/scholar?q=A+Fast+Algorithm+for+Finding+Dominators+in+a+Flowgraph.
Finding Dominators in Practice
  http://jgaa.info/getPaper?id=119
  https://scholar.google.com/scholar?q=Finding+Dominators+in+Practice.
  https://link.springer.com/chapter/10.1007/978-3-540-30140-0_60
Finding dominators via disjoint set union
  https://www.sciencedirect.com/science/article/pii/S1570866713001007
Loop Nesting Forests, Dominators, and Applications
  https://link.springer.com/chapter/10.1007/978-3-319-07959-2_15
Dominators in directed graphs: a survey of recent results, applications, and open problems
Immediate predominators in a directed graph [H]
  https://dl.acm.org/citation.cfm?doid=361532.361566
a poset approach to dominator computation
lengauer-tarjan
  https://gist.github.com/yuzeh/a5e6602dfdb0db3c2130c10537db54d7
  https://www.yuzeh.com/code/scala-pseudocode.html
  https://github.com/google/binnavi/blob/c3f9058/src/main/java/com/google/security/zynamics/zylib/types/graphs/algorithms/LengauerTarjan.java#L37-L38
  https://www.boost.org/doc/libs/1_43_0/libs/graph/doc/lengauer_tarjan_dominator.htm
  https://help.eclipse.org/oxygen/index.jsp?topic=%2Forg.eclipse.mat.ui.help%2Fconcepts%2Fdominatortree.html
Edge-disjoint spanning trees and depth-first search
  https://link.springer.com/article/10.1007/BF00268499
Identifying loops in almost linear time
  https://dl.acm.org/citation.cfm?id=316687

No More Gotos: Decompilation Using Pattern-Independent Control-Flow Structuring and Semantics-Preserving Transformations
  https://www.ndss-symposium.org/ndss2015/ndss-2015-programme/no-more-gotos-decompilation-using-pattern-independent-control-flow-structuring-and-semantics/
  https://scholar.google.com/scholar?cites=1213140627167738221&as_sdt=5,45&sciodt=0,45&hl=en
  https://www.semanticscholar.org/paper/No-More-Gotos%3A-Decompilation-Using-Control-Flow-and-Yakdan-Eschweiler/a545e5214086d048624b6176c0017db57652163e?tab=abstract&citingPapersSort=is-influential&citingPapersLimit=10&citingPapersOffset=10&year%5B0%5D=&year%5B1%5D=&citedPapersSort=is-influential&citedPapersLimit=10&citedPapersOffset=0

Meaningful variable names for decompiled code: a machine translation approach
  https://www.semanticscholar.org/paper/Meaningful-variable-names-for-decompiled-code%3A-a-Jaffe-Lacomis/44673a5ec512a81e0c366226135d1ce363d782e6?tab=abstract&citingPapersSort=is-influential&citingPapersLimit=10&citingPapersOffset=0&citedPapersSort=is-influential&citedPapersLimit=10&citedPapersOffset=10
Suggesting meaningful variable names for decompiled code: a machine translation approach
  https://dl.acm.org/citation.cfm?id=3121274
Emscripten: an LLVM-to-JavaScript compiler
  https://dl.acm.org/citation.cfm?doid=2048147.2048224
The Region Problem
  https://zneak.github.io/fcd/2016/02/17/structuring.html

https://en.wikipedia.org/wiki/Strongly_connected_component
  https://en.wikipedia.org/wiki/Tarjan%27s_strongly_connected_components_algorithm
  https://en.wikipedia.org/wiki/Kosaraju%27s_algorithm
  https://en.wikipedia.org/wiki/Path-based_strong_component_algorithm
 */

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

  // Returns a graph with notes pointing to their immediate dominator
  def dominatorTree[V,E](graph: Graph[V,E], start: V): Graph[V, DefaultEdge] = {
    val dom = dominators(graph, start)
    val tree = new SimpleDirectedGraph[V, DefaultEdge](classOf[DefaultEdge])

    object O extends Ordering[V] {
      override def compare(x: V, y: V): Int = {
        if (dom(x)(y)) { -1 }
        else if (dom(y)(x)) { 1 }
        else { 0 }
      }
    }

    for ((k, vs) <- dom) {
      tree.addEdge(k, vs.min(O))
    }

    tree
  }
}
