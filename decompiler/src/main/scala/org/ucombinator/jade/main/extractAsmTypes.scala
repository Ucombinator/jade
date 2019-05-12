/*Reflection pass to build maps from opcode names to integers and vice versa
* Reads from AbstractInsnNode (see import)
* Writes to stdout, but output created org.ucombinator.jade.asm.InstructionTypes */

package org.ucombinator.jade.main.extractAsmTypes


import java.lang.reflect.Modifier

import org.objectweb.asm.tree.AbstractInsnNode

object Main {
  def main () : Unit = {
    println("/*Automatically built by extractAsmTypes.scala to reflect values in AbstractInsnNode. Last checked against ASM 7.0*/")
    println("package org.ucombinator.jade.asm")
    println("import org.objectweb.asm.tree.AbstractInsnNode")
    println("object InstructionTypes{")
    println("  val fromString = Map(")

    for (field <- classOf[AbstractInsnNode].getDeclaredFields()){
      //as of ASM 7.0, all final public static int members of AbstractInsNode were ones we want. Updates beware.
      if(field.getModifiers == (Modifier.FINAL |  Modifier.PUBLIC | Modifier.STATIC) && field.getType == classOf[Int]){
        println(f"""    "${field.getName}" -> AbstractInsnNode.${field.getName},""")
      }
    }
    println("  )");
    println("  val fromInt = fromString map {_.swap}")
    println("}")
  }


}
