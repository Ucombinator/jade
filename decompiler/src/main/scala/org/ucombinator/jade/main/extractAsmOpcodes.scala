package org.ucombinator.jade.main.extractAsmOpcodes

import java.lang.reflect
import java.lang.reflect.Modifier

import org.objectweb.asm.Opcodes



object Main {
  def main () : Unit = {


    println("package org.ucombinator.jade.asm")
    println("import org.objectweb.asm.{Opcodes=>AsmOpcodes}")
    println("object Opcodes{")
    println("  val fromString = Map(")
    var count = 0


    for (field <- classOf[Opcodes].getDeclaredFields()){
      if(field.getType == classOf[Int]) {
        val name = field.getName
        if (!(name.startsWith("ACC_") ||
          name.startsWith("F_") ||
          name.startsWith("T_") ||
          name.startsWith("ASM") ||
          name.startsWith("V") ||
          name.startsWith("H_"))) {
          count = count + 1;
          println(f"""    "${field.getName}" -> AsmOpcodes.${field.getName},""")
        }
        //
      }
    }
    println("  )");
    println("  val fromInt = fromString map {_.swap}")
    println("}")
  }


}
