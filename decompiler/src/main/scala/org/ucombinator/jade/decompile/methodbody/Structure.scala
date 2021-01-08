package org.ucombinator.jade.decompile.methodbody

import org.jgrapht.graph.AsGraphUnion
import org.jgrapht.traverse.DepthFirstIterator
import org.ucombinator.jade.asm.Insn
import org.ucombinator.jade.util.jgrapht.Dominator

import scala.jdk.CollectionConverters._

sealed trait StructureKind
case class Loop() extends StructureKind

case class Structure(kind: StructureKind, head: Insn, tail: Structure)

object Structure {
  def get(): Map[Insn,List[Insn]] = { ???
    // maps from vertex to list of vertices that are loop heads

          // for each loop
      // what is the query that loops need
      /*
      Loop heads dominate a predicesor
      Whole loop = all vertecies backwards from predicestor until loop head
      */
  }
}


object Exceptions {
  def apply(cfg: ControlFlowGraph): Unit = {
    // TODO: check that handlers properly nest
    val union = new AsGraphUnion(cfg.graph, ???) // Edge from entry of try to each handler?
    val doms = Dominator.dominatorTree(union, cfg.entry)
    for (handler <- cfg.method.tryCatchBlocks.asScala) yield {
      val insns = new DepthFirstIterator[Insn, Dominator.Edge[Insn]](doms, Insn(cfg.method, handler.handler)).asScala.toList.sortBy(_.index)
      val indexes = insns.map(_.index)
      assert(indexes == (indexes.min to indexes.max).toList)
      handler -> insns
    }
  }
}
