package org.ucombinator.jade.method

import com.google.common.collect.ImmutableBiMap
import org.jgrapht.{Graph, Graphs}
import org.jgrapht.alg.cycle.TarjanSimpleCycles
import org.jgrapht.graph.DirectedPseudograph
import org.objectweb.asm.tree.{AbstractInsnNode, JumpInsnNode, MethodNode}
import org.ucombinator.jade.method.controlFlowGraph.{ControlFlowGraph, Edge}

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.collection.mutable.ListBuffer


abstract class AbstractBlock {
  val start: Int
  val end: Int

  def contains(b: AbstractBlock): Boolean =
    b.start >= this.start && b.end <= this.end
}

case class NoJumpBlock(start: Int, end: Int) extends AbstractBlock

trait JumpBlock extends AbstractBlock

case class BranchBlock(start: Int, end: Int, jmp: JumpInsnNode)
  extends AbstractBlock with JumpBlock

case class CycleBlock(start: Int, end: Int, forwardJumpInsn: JumpInsnNode, backwardJumpInsn: JumpInsnNode)
  extends AbstractBlock with JumpBlock


final class InsnBlockTree(className: String, method: MethodNode) {
//  private val instructions: Array[AbstractInsnNode] = method.instructions.toArray

  private val insnIndexPairs: List[(AbstractInsnNode, Int)] = method.instructions.toArray.zipWithIndex.toList

  private val controlFlowGraph: DirectedPseudograph[AbstractInsnNode, Edge] =
    ControlFlowGraph.create(className, method).graph

  private val reverseControlFlowGraph: DirectedPseudograph[AbstractInsnNode, Edge] = {
    val result = new DirectedPseudograph[AbstractInsnNode, Edge](classOf[Edge])
    controlFlowGraph.vertexSet.asScala.foreach(result.addVertex)
    controlFlowGraph.edgeSet.asScala.map {
      case Edge(s, t) => Edge(t, s)
    } foreach {
      e => result.addEdge(e.source, e.target, e)
    }

    result
  }

  def dominators[V,E](graph: Graph[V,E], start: V): Map[V, Set[V]] = {
    val vs = graph.vertexSet.asScala.toSet
    var dom: Map[V, Set[V]]  = Map.empty

    // Initial assignment
    dom = dom + (start -> Set(start))
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
            Set(v) ++
              Graphs.predecessorListOf(graph, v).
                asScala.map(dom).
                fold(vs)(_ & _)))
      }
    } while (old_dom != dom)

    return dom
  }

  // Returns a mapping from nodes to the node that is its immediate dominator
  def immediateDominators[V,E](graph: Graph[V,E], start: V): Map[V, V] = {
    val dom = dominators(graph, start)

    object O extends Ordering[V] {
      override def compare(x: V, y: V): Int = {
        if (dom(x)(y)) -1
        else if (dom(y)(x)) 1
        else 0
      }
    }

    dom map {
      case (v, vs) if vs.size == 1 => v -> vs
      case (v, vs)                 => v -> (vs - v)
    } mapValues(_.min(O))
  }

  private val postDominators: Map[AbstractInsnNode, AbstractInsnNode] =
    immediateDominators(reverseControlFlowGraph, method.instructions.getLast)

  val insnIndexBiMap: ImmutableBiMap[AbstractInsnNode, Int] = {
    val builder: ImmutableBiMap.Builder[AbstractInsnNode, Int] = ImmutableBiMap.builder()

    for ((insn, idx) <- insnIndexPairs) {
      builder.put(insn, idx)
    }

    builder.build()
  }

  val jumpInsnIndexPair: List[(JumpInsnNode, Int)] =
    for {(j, idx) <- insnIndexPairs if j.isInstanceOf[JumpInsnNode]
         pair = (j.asInstanceOf[JumpInsnNode], idx)}
      yield pair

  @tailrec
  private def findFirstJumpAfter(n: AbstractInsnNode): JumpInsnNode =
    n match {
      case j: JumpInsnNode => j
      case _               => findFirstJumpAfter(n.getNext)
    }

  @tailrec
  private def findFirstJumpBefore(n: AbstractInsnNode): JumpInsnNode =
    n match {
      case j: JumpInsnNode => j
      case _               => findFirstJumpBefore(n.getPrevious)
    }

  private val cycleFinder = new TarjanSimpleCycles(controlFlowGraph)
  private val cycles: List[List[AbstractInsnNode]] = cycleFinder.findSimpleCycles.asScala.map(_.asScala.toList).toList

  private val cycleBlocks  =
    cycles map { l =>
      val start = insnIndexBiMap.get(l.head)
      val end = insnIndexBiMap.get(l.last)
      val forwardJumpInsn = findFirstJumpAfter(l.head)
      val backwardJumpInsn = findFirstJumpBefore(l.last)
      CycleBlock(start, end, forwardJumpInsn, backwardJumpInsn)
    }

  private val cycleJumpInsnNodes: Set[JumpInsnNode] = cycles.toSet flatMap { (l: List[AbstractInsnNode]) =>
    val forwardJumpInsn = findFirstJumpAfter(l.head)
    val backwardJumpInsn = findFirstJumpBefore(l.last)
    Seq(forwardJumpInsn, backwardJumpInsn)
  }

  private val branchBlocks: List[JumpBlock] =
    for {(jmp, start) <- jumpInsnIndexPair if !cycleJumpInsnNodes(jmp)
         end = insnIndexBiMap.get(postDominators(jmp))}
      yield BranchBlock(start, end, jmp)

  private val jumpBlocks: List[JumpBlock] = (cycleBlocks ++ branchBlocks).sortBy(_.start)

  /* There may be gaps between */
  def fillGaps(bs: List[AbstractBlock]): List[AbstractBlock] = bs match {
    case Nil => Nil
    case List(e) => List(e)
    case h :: (t @ hh :: _) if h.end - hh.start == 1 => h :: fillGaps(t)
    case h :: (t @ hh :: _) => h :: NoJumpBlock(h.end + 1, hh.start - 1) :: fillGaps(t)
  }

  def getRootBlocks(jbs: List[JumpBlock]): List[AbstractBlock] = {
    val result = new ListBuffer[JumpBlock]

    def helper(j: JumpBlock, js: List[JumpBlock]): Unit = {
      if (result.isEmpty || result.last != j) result += j

      js match {
        case Nil => ()
        case h :: t if j.contains(h) => helper(j, t)
        case h :: t =>
          helper(h, t)
      }
    }

    jbs match {
      case Nil => ()
      case h :: t   => helper(h, t)
    }

    fillGaps(result.toList)
  }

  val subJumpBlocksMap: mutable.LinkedHashMap[JumpBlock, List[JumpBlock]] = {
    def subJumpBlocks(j: JumpBlock): List[JumpBlock] = {
      jumpBlocks.filter(jj => jj.start > j.start && jj.end <= j.end)
    }

    val result = new mutable.LinkedHashMap[JumpBlock, List[JumpBlock]]

    jumpBlocks.map(j => j -> subJumpBlocks(j)).foreach {
      case (j, subj) => result.put(j, subj)
    }

    result
  }

  //  These two variables are effective a tree
  // TODO: Convert them to a real tree structure later
  val root = getRootBlocks(jumpBlocks)
  val tree = subJumpBlocksMap.mapValues(fillGaps)

}
