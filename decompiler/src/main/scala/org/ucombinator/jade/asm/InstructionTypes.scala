/* Automatically built by generateAsmInstructionTypes.scala to reflect values in AbstractInsnNode. */
/* Last checked against ASM 7.1. */

package org.ucombinator.jade.asm

import org.objectweb.asm.tree.AbstractInsnNode

object InstructionTypes{
  val fromString: Map[String, Int] = Map(
    "INSN" -> AbstractInsnNode.INSN,
    "INT_INSN" -> AbstractInsnNode.INT_INSN,
    "VAR_INSN" -> AbstractInsnNode.VAR_INSN,
    "TYPE_INSN" -> AbstractInsnNode.TYPE_INSN,
    "FIELD_INSN" -> AbstractInsnNode.FIELD_INSN,
    "METHOD_INSN" -> AbstractInsnNode.METHOD_INSN,
    "INVOKE_DYNAMIC_INSN" -> AbstractInsnNode.INVOKE_DYNAMIC_INSN,
    "JUMP_INSN" -> AbstractInsnNode.JUMP_INSN,
    "LABEL" -> AbstractInsnNode.LABEL,
    "LDC_INSN" -> AbstractInsnNode.LDC_INSN,
    "IINC_INSN" -> AbstractInsnNode.IINC_INSN,
    "TABLESWITCH_INSN" -> AbstractInsnNode.TABLESWITCH_INSN,
    "LOOKUPSWITCH_INSN" -> AbstractInsnNode.LOOKUPSWITCH_INSN,
    "MULTIANEWARRAY_INSN" -> AbstractInsnNode.MULTIANEWARRAY_INSN,
    "FRAME" -> AbstractInsnNode.FRAME,
    "LINE" -> AbstractInsnNode.LINE,
  )

  val fromInt: Map[Int, String] = fromString map {_.swap}
}
