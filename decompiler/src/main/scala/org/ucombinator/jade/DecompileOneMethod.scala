package org.ucombinator.jade

import org.jgrapht.alg.cycle.TarjanSimpleCycles
import org.objectweb.asm.tree.analysis.Frame
import org.objectweb.asm.tree.{AbstractInsnNode, MethodNode, _}
import org.ucombinator.jade.util.DebugUtil
import org.ucombinator.jade.util.DebugUtil.translator

import scala.collection.JavaConverters._


class DecompileOneMethod(className: String, method: MethodNode) {
  val IndexedInstructions: Map[AbstractInsnNode, Int] = method.instructions.toArray.zipWithIndex.toMap
  val analyser = new IdentifierAnalyzer(className, method)
  val frames: Array[Frame[Identifier]] = analyser.frames
  val controlFlowGraph = analyser.edges
  val cycleFinder = new TarjanSimpleCycles(controlFlowGraph)
  val cycles: List[List[AbstractInsnNode]] = cycleFinder.findSimpleCycles.asScala.map(_.asScala.toList).toList

  for (c <- cycles) {
    println(s"cycle $c")
    c.foreach(i => println(DebugUtil.translator(i)))
  }

  val cycleStartPositions: List[Int] = cycles.map(c => IndexedInstructions(c.head)).sorted


  def helper(src: List[String],
             info: Option[AnyRef],
             leftInstructions: List[(AbstractInsnNode, Int)]): List[String] =
    leftInstructions match {
      case Nil => src
      case h :: t =>
        // TODO:
        // process `h`
        // new info
        // new src
        helper(src, info, t)
    }


  val result: List[String] =
    helper(List("modifier TODO", "signature TODO", method.name, "{"),
      None,
      method.instructions.toArray.toList.zipWithIndex)

  for {(insn, idx) <- method.instructions.toArray.zipWithIndex
       f = frames(idx)
  } {
    if (insn.getOpcode == 16) {
      println(translator(insn))
      println(insn.asInstanceOf[IntInsnNode].operand)
      println("-----------")
    }

    if (DebugUtil.translator(insn) == "ISTORE") {
      println(f.getStackSize)
      println(f.getStack(0))
      println("===========")
    }

    println(s"($idx) ==== ${translator(insn)} ==== ${f}")
  }


}

