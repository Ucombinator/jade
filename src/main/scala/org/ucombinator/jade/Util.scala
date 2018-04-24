package org.ucombinator.jade

import org.jgrapht.graph.DefaultDirectedGraph
import org.jgrapht.{Graph, Graphs}
import org.objectweb.asm.tree._

import scala.collection.immutable
import scala.collection.JavaConverters._

object Util {
  // Returns a mapping from nodes to the set of nodes that dominate them
  def dominators[V,E](graph: Graph[V,E], start: V): immutable.Map[V, immutable.Set[V]] = {
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

    return dom
  }

  // Returns a mapping from nodes to the node that is its immediate dominator
  def immediateDominators[V,E](graph: Graph[V,E], start: V): immutable.Map[V, V] = {
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


  /** Debug usage */
  def printInsnNode(i: AbstractInsnNode): Unit = {
    val op = org.ucombinator.jade.util.Util.translator(i)

    val message =
      i match {
        case fld:  FieldInsnNode         => s"$op  --  $i -- name: ${fld.name}"
        case iinc: IincInsnNode          => s"$op  --  $i -- var: ${iinc.`var`} -- incr: ${iinc.incr}"
        case _:    InsnNode              => s"$op  --  $i -- NO OPERAND"
        case int:  IntInsnNode           => s"$op  --  $i -- ${int.operand}"
        case ivk:  InvokeDynamicInsnNode => s"$op  --  $i -- bsm: ${ivk.bsm} -- bsmArgs: ${ivk.bsmArgs}"
        case jmp:  JumpInsnNode          => s"$op  --  $i -- label: ${jmp.label}"
        case ldc:  LdcInsnNode           => s"$op  --  $i -- cst: ${ldc.cst}"
        case ls:   LookupSwitchInsnNode  => s"$op  --  $i -- dlft: ${ls.dflt} -- keys: ${ls.keys} -- lables: ${ls.labels}"
        case m:    MethodInsnNode        => s"$op  --  $i -- desc: ${m.desc} -- itf: ${m.itf} -- name: ${m.name}"
        case ts:   TableSwitchInsnNode   => s"$op  --  $i -- dflt: ${ts.dflt} -- labels: ${ts.labels} -- max: ${ts.max} -- min: ${ts.min}"
        case t:    TypeInsnNode          => s"$op  --  $i -- desc: ${t.desc}"
        case v:    VarInsnNode           => s"$op  --  $i -- var: ${v.`var`}"
        case _                           => s"$op  --  $i"
      }

    println(message)
  }


}
