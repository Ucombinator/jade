package org.ucombinator.jade.main.extractAsmTypes

import java.lang.reflect
import java.lang.reflect.Modifier

import org.objectweb.asm.tree.AbstractInsnNode

object Main {
  def main () : Unit = {


    println("package org.ucombinator.jade.asm")
    println("import org.objectweb.asm.tree.AbstractInsnNode")
    println("object InstructionTypes{")
    println("  val fromString = Map(")

    for (field <- classOf[AbstractInsnNode].getDeclaredFields()){
      if(field.getModifiers == (Modifier.FINAL |  Modifier.PUBLIC | Modifier.STATIC) && field.getType == classOf[Int]){
        //println("\n\"" + f"${field.getName}" + "\"->AbstractInsnNode." + f"${field.getName}")
        println(f"""    "${field.getName}" -> AbstractInsnNode.${field.getName},""")
      }
    }
    println("  )");
    println("  val fromInt = fromString map {_.swap}")
    println("}")
  }


}
