package org.ucombinator.jade.main.decompile

import java.io.PrintWriter
import java.nio.file.{Files, Paths}

import com.github.javaparser.StaticJavaParser
import com.github.javaparser.ast.`type`.{ClassOrInterfaceType, Type, TypeParameter}
import com.github.javaparser.ast.{CompilationUnit, ImportDeclaration, Modifier, NodeList, PackageDeclaration}
import com.github.javaparser.ast.body.{BodyDeclaration, ClassOrInterfaceDeclaration, FieldDeclaration, TypeDeclaration, VariableDeclarator}
import com.github.javaparser.ast.expr.{AnnotationExpr, DoubleLiteralExpr, Expression, IntegerLiteralExpr, LiteralExpr, LongLiteralExpr, MarkerAnnotationExpr, Name, NormalAnnotationExpr, SimpleName, SingleMemberAnnotationExpr, StringLiteralExpr}
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
  def main(fileName: String, printAsm: Boolean, printJavaparser: Boolean, printMethods: Boolean): Unit = {
    require(fileName != null, "the given class file name is actually `null`!")

    val byteArray = Files.readAllBytes(Paths.get(fileName)) //Full path class name

    val cn = new ClassNode
    val cr = new ClassReader(byteArray)

    cr.accept(cn, 0)

    if (printAsm) {
      val traceClassVisitor = new TraceClassVisitor(null, new Textifier(), new PrintWriter(System.out))
      cn.accept(traceClassVisitor)
    }

    if (printJavaparser) {
      val cu = asmToJavaparser(cn)
      println(cu)
    }

    if (printMethods) {
      // TODO: cn.sourceFile, cn.sourceDebug

      println("class {")

      // TODO: cn.outerClass, cn.outerMethod, cn.outerMethodDesc

      // TODO: Inner classes
      val inners: List[InnerClassNode] = cn.innerClasses.asScala.toList
      inners.foreach { c =>
        println(c.name)
      }

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
  }

  private def accessToJavaparser(access: Int): NodeList[Modifier] = {
    new NodeList() // TODO
  }

  private def typeToJavaparser(desc: String, signature: String): Type = {
    if (signature == null) { Descriptor.fieldDescriptor(desc) }
    else { Signature.javaTypeSignature(signature) }
  }

  private def literalToJavaparser(node: Object): LiteralExpr = node match {
    // TODO: improve formatting of literals?
    case null => null
    case node: java.lang.Integer => new IntegerLiteralExpr(String.valueOf(node))
    case node: java.lang.Float => new DoubleLiteralExpr(String.valueOf(node)) // Note that `javaparser` uses Double for Floats
    case node: java.lang.Long => new LongLiteralExpr(String.valueOf(node))
    case node: java.lang.Double => new DoubleLiteralExpr(String.valueOf(node))
    case node: java.lang.String => new StringLiteralExpr(node)
  }

  private def asmToJavaparser(node: AnnotationNode): AnnotationExpr = {
    val name = StaticJavaParser.parseName(node.desc)
    node.values.asScala match {
      case List() => new MarkerAnnotationExpr(name)
      case List(v: Object) => new SingleMemberAnnotationExpr(name, literalToJavaparser(v))
      case vs => new NormalAnnotationExpr(name, ???) // TODO
    }
  }

  private def asmToJavaparser(node: FieldNode): FieldDeclaration = {
    val modifiers = accessToJavaparser(node.access)
    val annotations: NodeList[AnnotationExpr] = {
      val list = new NodeList[AnnotationExpr]()
      val as = List(
        node.visibleAnnotations,
        node.invisibleAnnotations,
        node.visibleTypeAnnotations,
        node.invisibleTypeAnnotations)
      for (a <- as) {
        if (a != null) { list.addAll(a.asScala.map(asmToJavaparser).asJava) }
      }
      list
    }
    val variables = new NodeList[VariableDeclarator]({
      val `type`: Type = typeToJavaparser(node.desc, node.signature)
      val name = new SimpleName(node.name)
      val initializer: Expression = literalToJavaparser(node.value)
      new VariableDeclarator(`type`, name, initializer)})

    new FieldDeclaration(modifiers, annotations, variables)
  }

  private def asmToJavaparser(node: ClassNode): CompilationUnit = {
    val fullClassName: Name = StaticJavaParser.parseName(node.name.replace('/', '.'))

    val packageDeclaration = new PackageDeclaration(
      new NodeList[AnnotationExpr]() /*TODO*/, fullClassName.getQualifier.orElse(new Name()))
    val imports = new NodeList[ImportDeclaration]() // TODO

    val classOrInterfaceDeclaration = {
      val modifiers = accessToJavaparser(node.access)
      val annotations: NodeList[AnnotationExpr] = {
        val list = new NodeList[AnnotationExpr]()
        val as = List(
          node.visibleAnnotations,
          node.invisibleAnnotations,
          node.visibleTypeAnnotations,
          node.invisibleTypeAnnotations)
        for (a <- as) {
          if (a != null) { list.addAll(a.asScala.map(asmToJavaparser).asJava) }
        }
        list
      }
      val isInterface: Boolean = (node.access & Opcodes.ACC_INTERFACE) != 0
      val simpleName = new SimpleName(fullClassName.getIdentifier)
      // `extendedTypes` may be multiple if on an interface
      // TODO: test if should be Descriptor.className
      val (typeParameters, extendedTypes, implementedTypes):
        (NodeList[TypeParameter], NodeList[ClassOrInterfaceType], NodeList[ClassOrInterfaceType]) = {
        if (node.signature != null) {
          val s = Signature.classSignature(node.signature)
          (new NodeList(s._1.asJava), new NodeList(s._2), new NodeList(s._3.asJava))
        } else {
          (new NodeList(),
           new NodeList(Descriptor.className(node.superName)),
           new NodeList[ClassOrInterfaceType](node.interfaces.asScala.map(Descriptor.className).asJava))
        }
      }
      val members: NodeList[BodyDeclaration[_ <: BodyDeclaration[_]]] = {
        val list = new NodeList[BodyDeclaration[_ <: BodyDeclaration[_]]]()
        list.addAll(node.fields.asScala.map(asmToJavaparser).asJava)
        // TODO
        list
      }

      new ClassOrInterfaceDeclaration(
        modifiers, annotations, isInterface, simpleName, typeParameters, extendedTypes, implementedTypes, members) }

    if (classOrInterfaceDeclaration.isInterface) {
      classOrInterfaceDeclaration.setExtendedTypes(classOrInterfaceDeclaration.getImplementedTypes)
      classOrInterfaceDeclaration.setImplementedTypes(new NodeList())
    }

    val types = new NodeList[TypeDeclaration[_ <: TypeDeclaration[_]]]()
    types.add(classOrInterfaceDeclaration)

    val module = null // TODO

    new CompilationUnit(packageDeclaration, imports, types, module)
  }

  /** Get method Text */
  private def methodText(method: MethodNode): String = {
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
