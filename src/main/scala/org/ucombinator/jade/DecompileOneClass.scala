package org.ucombinator.jade

import org.objectweb.asm._
import org.objectweb.asm.tree._
import org.objectweb.asm.tree.analysis._

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.collection.immutable

object DecompileOneClass {

  def decompileOne(className: String): Unit = {
    require(className != null, "the given class file name is actually `null`!")
    val cn = new ClassNode
    // TODO: It seems in my test case the given names are only the class in Java standard library like
    // TODO (CONTINUE): "java.lang.Runnable" and "java.util.HashMap"
    // TODO (CONTINUE): try other cases later
    val cr = new ClassReader(className)
    cr.accept(cn, 0)

    // cn.version

    /* -- annotations -- */
    val visibleAnnotationsString: String = annotationText(cn.visibleAnnotations.asScala)
    val invisibleAnnotationsString: String = annotationText(cn.invisibleAnnotations.asScala)

    println(visibleAnnotationsString +"\n" + invisibleAnnotationsString)

    val classHeader: String = constructClassHeader(
      cn.access, cn.name,
      Option(cn.superName),
      cn.interfaces.asScala.toList, cn.signature, isInner = cn.outerClass != null)

    // TODO: cn.sourceFile, cn.sourceDebug

    println(classHeader + " {")

    // TODO: cn.outerClass, cn.outerMethod, cn.outerMethodDesc

    // TODO: Inner classes
    val inners: List[InnerClassNode] = cn.innerClasses.asScala.toList
    inners.foreach { c =>
      println(c.name)
    }

    // This is non-standard attributes, I'm not sure if I need them.
    // TODO: cn.attrs

    val fields: List[FieldNode] = cn.fields.asScala.toList
    val filedsCode: List[String] = fields.map(fieldText)

    println(filedsCode.mkString("\n"))

    val methods: List[MethodNode] = cn.methods.asScala.toList
    val methodsCode: List[String] = methods.map(methodText)

    println(methodsCode.mkString("\n"))

    for (method <- methods) {
      println(method.name)
      new IdentifierAnalyzer(className, method)
    }

    println("\n}")
  }


  private def constructClassHeader(access: Int, name: String, superName: Option[String],
                                   interfaces: List[String], signature: String, isInner: Boolean): String = {

    /* -- modifiers, which may include `interface` -- */

    val modifiers = if (isInner) AccessFlag.extractNestedClassAccessFlags(access)
                    else         AccessFlag.extractClassAccessFlags(access)
    val isInterface = modifiers.contains("interface")

    val modifiersAndTypeString: String =
      if (isInterface) modifiers.mkString(" ")
      else             modifiers.mkString(" ") + " " + "class"


    /* -- superclass and interfaces -- */
    val inheritanceString: String = inheritanceRelations(superName, interfaces)

    // TODO: signature

      modifiersAndTypeString + " " + name + " " + inheritanceString  // TODO: use triple quotes
  }

//  private def allAnnotationText(obj: {val visibleAnnotations: java.util.List[AnnotationNode]
//                                      val invisibleAnnotations: java.util.List[AnnotationNode]})
//  : String = {
//
//    val visibleAnnotationsString = annotationText(obj.visibleAnnotations.asScala.toList)
//    val invisibleAnnotationsString = annotationText(obj.invisibleAnnotations.asScala.toList)
//    List(visibleAnnotationsString, invisibleAnnotationsString).mkString("\n")
//  }

  private def annotationText(annotations: mutable.Buffer[AnnotationNode])
  : String =
  // TODO: eliminate `null`
    if (annotations != null && annotations.nonEmpty)
      annotations.map {
        // TODO: Annoation can only be class ????!!!?? I don't know!!!
        "@" + _.desc.stripPrefix("L").stripSuffix(";")
      }.mkString("\n")
    else ""

  private def inheritanceRelations(superName: Option[String], interfaces: List[String])
  : String = {
    val extendsSuperName = superName match {
      case None | Some("java/lang/Object") => ""
      case Some(s)                         => s"extends $s"
    }

    val implementsInterfaces =
      if (interfaces.isEmpty) { "" }
      else                    { s"implements ${interfaces.mkString(", ")}" }

    List(extendsSuperName, implementsInterfaces).
      mkString("\n")
  }


  /** Get field Text */
  private def fieldText(field: FieldNode): String = {
    val visibleAnnotationsString = annotationText(field.visibleAnnotations.asScala)
    val invisibleAnnotationsString = annotationText(field.invisibleAnnotations.asScala)

    /* -- modifiers, which may include `interface` -- */
    val modifiers = AccessFlag.extractFieldAccessFlags(field.access)
    val isAbstract = modifiers.contains("abstract")

    visibleAnnotationsString + "\n" +
      invisibleAnnotationsString + "\n" +
      // TODO: signature
      modifiers + " " + /* signatureString +*/ " " + field.name + (
      if (isAbstract || field.value == null) ""
      else                                   " = " + field.value
      )
  }

  /** Get method Text */
  private def methodText(method: MethodNode): String = {
    val visibleAnnotationsString = annotationText(method.visibleAnnotations.asScala)
    val invisibleAnnotationsString = annotationText(method.invisibleAnnotations.asScala)

    /* -- modifiers, which may include `interface` -- */
    val modifiers = AccessFlag.extractMethodAccessFlags(method.access)
    val isAbstract = modifiers.contains("abstract")

    val checkedExceptions = method.exceptions.asScala.mkString(", ")

    val parameterList = Option(method.parameters.asScala).
      getOrElse(mutable.Buffer.empty[ParameterNode]).
      map { p =>
        AccessFlag.extractParameterAccessFlags(p.access).mkString(" ") + p.name
      }.mkString(" ")
//    val parameterList =
//      method.parameters.asScala.map { p: ParameterNode =>
//        AccessFlag.extractParameterAccessFlags(p.access).mkString(" ") + p.name
//      }.mkString(", ")

    visibleAnnotationsString + "\n" +
      invisibleAnnotationsString + "\n" +
      // TODO: signature
      modifiers + " " + /* signatureString +*/ " " + method.name + "(" + parameterList + ")" +
      " " + (if (checkedExceptions.isEmpty) "" else "throws" + " " + checkedExceptions) +
      " " + (if (isAbstract) ";" else "{ /* TODO */ }")
  }

  def printInsnNode(insnNode: AbstractInsnNode): Unit = insnNode match {
    case f     : FieldInsnNode          => println(f.getOpcode)
    case fr    : FrameNode              => println(fr.getOpcode)
    case iinc  : IincInsnNode           => println(iinc.getOpcode)
    case i     : InsnNode               => println(i.getOpcode)
    case int   : IntInsnNode            => println(int.getOpcode)
    case invDyn: InvokeDynamicInsnNode  => println(invDyn.getOpcode)
    case jmp   : JumpInsnNode           => println(jmp.getOpcode)
    case lb    : LabelNode              => println(lb.getOpcode)
    case ldc   : LdcInsnNode            => println(ldc.getOpcode)
    case ln    : LineNumberNode         => println(ln.getOpcode)
    case lkpS  : LookupSwitchInsnNode   => println(lkpS.getOpcode)
    case m     : MethodInsnNode         => println(m.getOpcode)
    case mulArr: MultiANewArrayInsnNode => println(mulArr.getOpcode)
    case tabS  : TableSwitchInsnNode    => println(tabS.getOpcode)
    case t     : TypeInsnNode           => println(t.getOpcode)
    case v     : VarInsnNode            => println(v.getOpcode)
  }
}
