package org.ucombinator.jade

import org.objectweb.asm._

object DecompileOneClass {
  def main(): Unit = {
    val cp = new ClassPrinter
//    val cr = new ClassReader("java.lang.Runnable");
    val cr = new ClassReader("java.util.HashMap")
    cr.accept(cp, 0)
    println("\n\n\n\n\n")
    println(cp.sourceFileName)
  }
}

class ClassPrinter() extends ClassVisitor(Opcodes.ASM6) {
  private var sourceFile = Option.empty[String]
  private var visitSourceIsCalled = false

  private def extractModifiers(access: Int, candidateFlags: Map[Int, TAccessFlag])
  : Set[String] = candidateFlags
    .keySet
    .withFilter(f => (f & access) != 0)
    .map(candidateFlags)
    .flatMap(_.keyword)

  override def visit(version: Int, access: Int, name: String, signature: String, superName: String,
                     interfaces: Array[String])
  : Unit = {
    val extendsSuperName =
      if (superName.isEmpty) { "" }
      else                   { s"extends $superName" }

    val implementsInterfaces =
      if (interfaces.isEmpty) { "" }
      else                    { s"implements ${interfaces.mkString(", ")}" }

    val modifiers = extractModifiers(access, AccessFlag.classFlags)
      .mkString(" ")

    val header = s"$modifiers $name $extendsSuperName $implementsInterfaces"
    println(header + " {")
  }

  override def visitSource(source: String, debug: String) : Unit = {
    this.visitSourceIsCalled = true
    if (this.sourceFile.isEmpty) this.sourceFile = Some(source)
    // xxx
  }

  override def visitOuterClass(owner: String, name: String, desc: String)
  : Unit = {
  }

  override def visitAnnotation(desc: String, visible: Boolean): AnnotationVisitor = null

  override def visitAttribute(attr: Attribute) : Unit = {
    println(attr)
  }

  override def visitInnerClass(name: String, outerName: String, innerName: String, access: Int) : Unit = {}

  override def visitField(access: Int, name: String, desc: String, signature: String, value: Any)
  : FieldVisitor = {
    val modifiers = extractModifiers(access, AccessFlag.fieldFlags).mkString(" ")
    println(s"$modifiers $desc $name")
    null
  }

  override def visitMethod(access: Int, name: String, desc: String, signature: String, exceptions: Array[String])
  : MethodVisitor = {
    System.out.println(" " + name + desc)
    null
  }

  override def visitEnd()
  : Unit = {
    System.out.println("}")
  }

  def sourceFileName: Option[String] =
    if (visitSourceIsCalled)
      sourceFile
    else
      throw new RuntimeException("You cannot get the source file name before calling the" + "`visitSource` method")
}

