package org.ucombinator.jade.decompile.methodbody

import scala.jdk.CollectionConverters._

import org.ucombinator.jade.asm.Insn
import org.ucombinator.jade.util.MyersList

case class Structure(nesting: Map[Insn, Structure.Nesting], backEdges: Set[ControlFlowGraph.Edge])

// TODO: rename to CodeStructure or CodeNesting or BlockNesting
object Structure {
  // TODO: name to NestingPath or StructurePath or BlockPath
  type Nesting = MyersList[Block]

  case class Block(kind: Kind, headInsn: Insn)

  sealed trait Kind // TODO: structural type
  case class Loop() extends Kind
  case class Exception() extends Kind
  case class Synchronized() extends Kind

  def apply(cfg: ControlFlowGraph): Structure = {
    // This dummy works only on code with no loops, try/catches, or synchronized blocks
    val backEdges = Set[ControlFlowGraph.Edge]()
    // TODO: note that head block is present so we can always safely call .head, but its headInsn is null so it doesn't match the first instruction of the method
    val nestingRoot: Nesting = MyersList.Cons(Block(kind = null, headInsn = null), MyersList.Nil)
    val nestingMap = cfg.graph.vertexSet().asScala.map(_ -> nestingRoot).toMap
    return Structure(nestingMap, backEdges)
    /*
        Loop heads dominate a predicesor
        Loop tree based on Dominator tree?
        Whole loop = all vertecies backwards from predicestor until loop head
     */
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
