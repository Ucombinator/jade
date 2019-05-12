/*Reflection pass to build maps from opcode names to integers and vice versa
* Reads from Opcodes.scala (see import)
* Writes to stdout, but output created \org.ucombinator.jade.asm.Opcodes*/

package org.ucombinator.jade.main.extractAsmOpcodes


import org.objectweb.asm.Opcodes



object Main {
  def main () : Unit = {

    println("/*Automatically built by extractAsmOpcodes.scala to reflect values in Opcodes. Last checked against ASM 7.0*/")
    println("package org.ucombinator.jade.asm")
    println("import org.objectweb.asm.{Opcodes=>AsmOpcodes}")
    println("object Opcodes{")
    println("  val fromString = Map(")


    for (field <- classOf[Opcodes].getDeclaredFields()){
      if(field.getType == classOf[Int]) {
        val name = field.getName
        //Opcodes lists a number of other integers that we want to exclude.
        //These conditions were manually checked to be valid in ASM 7.0
        //Future versions beware
        if (!(name.startsWith("ACC_") ||
          name.startsWith("F_") ||
          name.startsWith("T_") ||
          name.startsWith("ASM") ||
          name.startsWith("V") ||
          name.startsWith("H_"))) {
          println(f"""    "${field.getName}" -> AsmOpcodes.${field.getName},""") //triple qoutes necessary due to bug with formatted strings and escape characters
        }
      }
    }
    println("  )");
    println("  val fromInt = fromString map {_.swap}") //Creates reverse map
    println("}")
  }


}
