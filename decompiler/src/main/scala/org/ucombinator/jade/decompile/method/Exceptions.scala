package org.ucombinator.jade.decompile.method

import org.jgrapht.graph.AsGraphUnion
import org.jgrapht.traverse.DepthFirstIterator
import org.ucombinator.jade.asm.Insn
import org.ucombinator.jade.util.jgrapht.Dominator

import scala.jdk.CollectionConverters._

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
