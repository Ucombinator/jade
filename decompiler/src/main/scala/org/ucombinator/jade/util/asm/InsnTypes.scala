package org.ucombinator.jade.util.asm

import java.lang.reflect.Modifier

import org.objectweb.asm.tree.AbstractInsnNode

// TODO: Move to Insn.scala
object InsnTypes {
  val fromString: Map[String, Int] =
    (for (field <- classOf[AbstractInsnNode].getDeclaredFields) yield {
      // As of ASM 7.1, all final public static int members of AbstractInsNode are ones we want. Updates beware.
      if (field.getType == classOf[Int] && field.getModifiers == (Modifier.FINAL |  Modifier.PUBLIC | Modifier.STATIC)) {
        Some(field.getName -> field.get(null).asInstanceOf[Integer].intValue())
      } else {
        None
      }
    }).toList.flatten.toMap

  val fromInt: Map[Int, String] = fromString map {_.swap}
}
