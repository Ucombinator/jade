package org.ucombinator.jade.main.decompile

import java.io.PrintWriter
import java.nio.file.{Files, Paths}

import com.github.javaparser.{StaticJavaParser, TokenRange}
import com.github.javaparser.ast.`type`.{ClassOrInterfaceType, Type, TypeParameter}
import com.github.javaparser.ast.{CompilationUnit, ImportDeclaration, Modifier, NodeList, PackageDeclaration}
import com.github.javaparser.ast.body.{BodyDeclaration, ClassOrInterfaceDeclaration, FieldDeclaration, TypeDeclaration, VariableDeclarator}
import com.github.javaparser.ast.expr.{AnnotationExpr, DoubleLiteralExpr, Expression, IntegerLiteralExpr, LiteralExpr, LongLiteralExpr, MarkerAnnotationExpr, Name, NormalAnnotationExpr, SimpleName, SingleMemberAnnotationExpr, StringLiteralExpr}
import com.github.javaparser.ast.modules.ModuleDeclaration
import org.objectweb.asm.{ClassReader, Opcodes}
import org.objectweb.asm.tree._
import org.objectweb.asm.util.{Textifier, TraceClassVisitor}
import org.ucombinator.jade.asm.Instructions
import org.ucombinator.jade.classfile.{AccessFlag, Descriptor, Signature}
import org.ucombinator.jade.method.controlFlowGraph.ControlFlowGraph
import org.ucombinator.jade.method.ssa.SSA

import scala.collection.mutable
import scala.collection.JavaConverters._

object Main {
  def main(fileName: String): Unit = {
    require(fileName != null, "the given class file name is actually `null`!")

    val byteArray = Files.readAllBytes(Paths.get(fileName)) //Full path class name

    val cn = new ClassNode
    val cr = new ClassReader(byteArray)

    cr.accept(cn, 0)

    val traceClassVisitor = new TraceClassVisitor(null, new Textifier(), new PrintWriter(System.out))
    cn.accept(traceClassVisitor)

    /* -- annotations -- */
    val visibleAnnotationsString: String = annotationText(cn.visibleAnnotations.asScala)
    val invisibleAnnotationsString: String = annotationText(cn.invisibleAnnotations.asScala)

    println(visibleAnnotationsString +"\n" + invisibleAnnotationsString)

    val cu = astToJavaparser(cn)
    println(cu)

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
    val fieldsCode: List[String] = fields.map(fieldText)

    println(fieldsCode.mkString("\n"))

    val methods: List[MethodNode] = cn.methods.asScala.toList
    val methodsCode: List[String] = methods.map(methodText)

    println(methodsCode.mkString("\n"))

    for (method <- methods) {
      println("!!!!!!!!!!!!")
      println(f"method: ${method.name} ${method.signature} ${method.desc}")
      println("**** ControlFlowGraph ****")
      val cfg = ControlFlowGraph(fileName, method)
      for (v <- cfg.graph.vertexSet().asScala) {
        println(f"v: ${method.instructions.indexOf(v)} ${cfg.graph.incomingEdgesOf(v).size()}: $v")
      }
      println("**** SSA ****")
      val ids = SSA(fileName, method, cfg)

      println("frames: " + ids.frames.length)
      for (i <- 0 until method.instructions.size) {
        println(f"frame($i): ${ids.frames(i)}")
      }

      println("results and arguments")
      for (i <- 0 until method.instructions.size) {
        val insn = method.instructions.get(i)
        println(f"args(${i}): ${Instructions.toString(method.instructions, insn)} ${ids.instructionArguments.get(insn)}")
      }

      println("ssa")
      for ((key, value) <- ids.ssaMap) {
        println(s"ssa: $key -> $value")
      }
    }

    println("\n}")
  }

  private def typeToJavaparser(desc: String, signature: String): Type = {
    println(f"desc: $desc sig: $signature")
    if (signature == null) { Descriptor.fieldDescriptor(desc) }
    else { Signature.javaTypeSignature(signature) }
  }

  private def literalToJavaparser(node: Object): LiteralExpr = node match {
    // TODO: improve formatting of literals?
    case null => null
    case node: java.lang.Integer => new IntegerLiteralExpr(String.valueOf(node))
    case node: java.lang.Float => ???
    case node: java.lang.Long => new LongLiteralExpr(String.valueOf(node))
    case node: java.lang.Double => new DoubleLiteralExpr(String.valueOf(node))
    case node: java.lang.String => new StringLiteralExpr(node)
  }

  private def astToJavaparser(node: AnnotationNode): AnnotationExpr = {
    val name = StaticJavaParser.parseName(node.desc)
    node.values.asScala match {
      case List() => new MarkerAnnotationExpr(name)
      case List(v: Object) => new SingleMemberAnnotationExpr(name, literalToJavaparser(v))
      case vs => new NormalAnnotationExpr(name, ???)
    }
  }

  private def astToJavaparser(node: FieldNode): FieldDeclaration = {
    val tokenRange: TokenRange = null // TODO
    val modifiers: NodeList[Modifier] = new NodeList() // TODO
    val annotations: NodeList[AnnotationExpr] = new NodeList()
    // TODO
    //annotations.addAll(node.visibleAnnotations.asScala.map(astToJavaparser).asJava)
    //annotations.addAll(node.invisibleAnnotations.asScala.map(astToJavaparser).asJava)
    //annotations.addAll(node.visibleTypeAnnotations.asScala.map(astToJavaparser).asJava)
    //annotations.addAll(node.invisibleTypeAnnotations.asScala.map(astToJavaparser).asJava)
    val variables: NodeList[VariableDeclarator] = new NodeList({
      val `type`: Type = typeToJavaparser(node.desc, node.signature)
      val name: SimpleName = new SimpleName(node.name)
      val initializer: Expression = literalToJavaparser(node.value)
      new VariableDeclarator(tokenRange: TokenRange, `type`: Type, name: SimpleName, initializer: Expression)})

    new FieldDeclaration(tokenRange, modifiers, annotations, variables)
  }

  private def astToJavaparser(node: ClassNode): CompilationUnit = {
    val fullClassName = StaticJavaParser.parseName(node.name.replace('/', '.'))

    val packageDeclaration: PackageDeclaration = new PackageDeclaration(new NodeList[AnnotationExpr]() /*TODO*/, fullClassName.getQualifier.orElse(new Name()))
    val imports: NodeList[ImportDeclaration] = new NodeList() // TODO

    val classOrInterfaceDeclaration = {
      val modifiers: NodeList[Modifier] = new NodeList() // TODO
      val annotations: NodeList[AnnotationExpr] = new NodeList() // TODO
      val isInterface: Boolean = (node.access & Opcodes.ACC_INTERFACE) != 0
      val simpleName: SimpleName = new SimpleName(fullClassName.getIdentifier)
      val typeParameters: NodeList[TypeParameter] = new NodeList() // TODO
      val extendedTypes: NodeList[ClassOrInterfaceType] = new NodeList() // TODO
      val implementedTypes: NodeList[ClassOrInterfaceType] = new NodeList() // TODO
      val members: NodeList[BodyDeclaration[_ <: BodyDeclaration[_]]] = new NodeList[BodyDeclaration[_ <: BodyDeclaration[_]]]()

      members.addAll(node.fields.asScala.map(astToJavaparser).asJava)

      new ClassOrInterfaceDeclaration(modifiers, annotations, isInterface, simpleName, typeParameters, extendedTypes, implementedTypes, members)}

    val types: NodeList[TypeDeclaration[_ <: TypeDeclaration[_]]] = new NodeList[TypeDeclaration[_ <: TypeDeclaration[_]]]()
    types.add(classOrInterfaceDeclaration)

    val module: ModuleDeclaration = null // TODO

    new CompilationUnit(packageDeclaration, imports, types, module)
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
        // TODO: Annotation can only be class ????!!!?? I don't know!!!
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
