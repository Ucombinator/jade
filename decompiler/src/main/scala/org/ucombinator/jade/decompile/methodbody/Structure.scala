package org.ucombinator.jade.decompile.methodbody

import org.ucombinator.jade.util.MyersList
import org.ucombinator.jade.asm.Insn

case class Structure(nesting: Map[Insn, Structure.Nesting], backEdges: Set[ControlFlowGraph.Edge])

object Structure {
  type Nesting = MyersList[{val kind: Kind; val headInsn: Insn}]

  sealed trait Kind // TODO: structural type
  case class Loop() extends Kind
  case class Exception() extends Kind
  case class Synchronized() extends Kind

  def apply(cfg: ControlFlowGraph): Structure = { 
        /*
        Loop heads dominate a predicesor
        Loop tree based on Dominator tree?



        Whole loop = all vertecies backwards from predicestor until loop head
        */
        ???
  }
}

  // object Exceptions {
  //   def apply(cfg: ControlFlowGraph): Unit = {
  //     // TODO: check that handlers properly nest
  //     val union = new AsGraphUnion(cfg.graph, ???) // Edge from entry of try to each handler?
  //     val doms = Dominator.dominatorTree(union, cfg.entry)
  //     for (handler <- cfg.method.tryCatchBlocks.asScala) yield {
  //       val insns = new DepthFirstIterator[Insn, Dominator.Edge[Insn]](doms, Insn(cfg.method, handler.handler)).asScala.toList.sortBy(_.index)
  //       val indexes = insns.map(_.index)
  //       assert(indexes == (indexes.min to indexes.max).toList)
  //       handler -> insns
  //     }
  //   }
  // }
