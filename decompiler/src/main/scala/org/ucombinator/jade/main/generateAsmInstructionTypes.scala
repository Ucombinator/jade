/**
 * Reflection pass to build maps from opcode names to integers and vice versa.
 * Reads from AbstractInsnNode (see import).
 * Writes to stdout, but output created org.ucombinator.jade.asm.InstructionTypes. */

package org.ucombinator.jade.main.generateAsmInstructionTypes

import org.objectweb.asm.tree.AbstractInsnNode

import java.lang.reflect.Modifier

object Main {
  def main () : Unit = {
    println("/* Automatically built by generateAsmInstructionTypes.scala to reflect values in AbstractInsnNode. */")
    println("/* Last checked against ASM 7.1. */")
    println()
    println("package org.ucombinator.jade.asm")
    println()
    println("import org.objectweb.asm.tree.AbstractInsnNode")
    println()
    println("object InstructionTypes{")
    println("  val fromString: Map[String, Int] = Map(")

    for (field <- classOf[AbstractInsnNode].getDeclaredFields) {
      // As of ASM 7.1, all final public static int members of AbstractInsNode were ones we want. Updates beware.
      if (field.getType == classOf[Int] && field.getModifiers == (Modifier.FINAL |  Modifier.PUBLIC | Modifier.STATIC)) {
        println(f"""    "${field.getName}" -> AbstractInsnNode.${field.getName},""")
      }
    }

    println("  )")
    println()
    println("  val fromInt: Map[Int, String] = fromString map {_.swap}")
    println("}")
  }
}
