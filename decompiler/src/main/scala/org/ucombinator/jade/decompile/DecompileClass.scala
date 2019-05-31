package org.ucombinator.jade.decompile

import com.github.javaparser.ast.{CompilationUnit, ImportDeclaration, NodeList, PackageDeclaration}
import com.github.javaparser.ast.`type`.{ClassOrInterfaceType, ReferenceType, Type, TypeParameter}
import com.github.javaparser.ast.body.{BodyDeclaration, ClassOrInterfaceDeclaration, FieldDeclaration, MethodDeclaration, Parameter, ReceiverParameter, TypeDeclaration, VariableDeclarator}
import com.github.javaparser.ast.expr.{AnnotationExpr, DoubleLiteralExpr, Expression, IntegerLiteralExpr, LiteralExpr, LongLiteralExpr, MarkerAnnotationExpr, MemberValuePair, Name, NormalAnnotationExpr, SimpleName, SingleMemberAnnotationExpr, StringLiteralExpr}
import com.github.javaparser.ast.stmt.BlockStmt
import org.objectweb.asm.Opcodes
import org.objectweb.asm.tree.{AnnotationNode, ClassNode, FieldNode, MethodNode, ParameterNode}
import org.ucombinator.jade.util.classfile.{Descriptor, Modifier, Signature}

import scala.collection.JavaConverters._

object DecompileClass {

  private def literalToJavaParser(node: Object): LiteralExpr = node match {
    // TODO: improve formatting of literals?
    case null => null
    case node: java.lang.Integer => new IntegerLiteralExpr(String.valueOf(node))
    case node: java.lang.Float => new DoubleLiteralExpr(String.valueOf(node)) // Note that `JavaParser` uses Double for Floats
    case node: java.lang.Long => new LongLiteralExpr(String.valueOf(node))
    case node: java.lang.Double => new DoubleLiteralExpr(String.valueOf(node))
    case node: java.lang.String => new StringLiteralExpr(node)
    case _ => throw new Exception(f"unimplemented literal '$node'")
  }

  private def typeToName(t: Type): Name = t match {
    case null => null
    case t: ClassOrInterfaceType => new Name(typeToName(t.getScope.orElse(null)), t.getName.getIdentifier)
    case _ => throw new Exception(f"failed to convert type $t to a name")
  }

  private def asmToJavaParser(node: AnnotationNode): AnnotationExpr = {
    val name = typeToName(Descriptor.fieldDescriptor(node.desc))
    node.values.asScala match {
      case null => new MarkerAnnotationExpr(name)
      case List(v: Object) => new SingleMemberAnnotationExpr(name, literalToJavaParser(v))
      case vs =>
        new NormalAnnotationExpr(
          name,
          new NodeList[MemberValuePair](
            (for (List(k, v) <- vs.grouped(2)) yield {
              new MemberValuePair(
                k.asInstanceOf[String],
                literalToJavaParser(v.asInstanceOf[Object]))}).toList.asJava) )
    }
  }

  private def asmToJavaParser(node: FieldNode): FieldDeclaration = {
    // attrs (ignore?)
    val modifiers = Modifier.modifiersToNodeList(Modifier.intToField(node.access))
    val annotations: NodeList[AnnotationExpr] = asmToJavaParsers(
      node.visibleAnnotations,
      node.invisibleAnnotations,
      node.visibleTypeAnnotations,
      node.invisibleTypeAnnotations)
    val variables = new NodeList[VariableDeclarator]({
      val `type`: Type =
        if (node.signature == null) { Descriptor.fieldDescriptor(node.desc) }
        else { Signature.typeSignature(node.signature) }
      val name = new SimpleName(node.name)
      val initializer: Expression = literalToJavaParser(node.value)
      new VariableDeclarator(`type`, name, initializer)})

    new FieldDeclaration(modifiers, annotations, variables)
  }

  private def decomParameter(method: MethodNode, paramCount: Int):
    (((((Type, Int), ParameterNode), java.util.List[AnnotationNode]), java.util.List[AnnotationNode])) => Parameter = {
    case ((((typ, index), node), a1), a2) =>
      val flags = if (node == null) { List() } else { Modifier.intToParameter(node.access) }
      val modifiers = Modifier.modifiersToNodeList(flags)
      val annotations: NodeList[AnnotationExpr] = asmToJavaParsers(a1, a2, null, null)
      val `type`: Type = typ
      val isVarArgs: Boolean =
        Modifier.intToMethod(method.access).contains(Modifier.ACC_VARARGS) &&
        index == paramCount - 1
      val varArgsAnnotations= new NodeList[AnnotationExpr]() // TODO?
      val name: SimpleName = new SimpleName(if (node == null) { f"parameter${index + 1}" } else { node.name })
    new Parameter(modifiers, annotations, `type`, isVarArgs, varArgsAnnotations, name)
  }

  private def asmToJavaParser(node: MethodNode): MethodDeclaration = {
    // attr (ignore?)
    // instructions
    // tryCatchBlocks
    // maxStack
    // maxLocals
    // localVariables
    // visibleLocalVariableAnnotations
    // invisibleLocalVariableAnnotations
    // TODO: JPModifier.Keyword.DEFAULT
    val modifiers = Modifier.modifiersToNodeList(Modifier.intToMethod(node.access))
    val annotations: NodeList[AnnotationExpr] = asmToJavaParsers(
      node.visibleAnnotations,
      node.invisibleAnnotations,
      node.visibleTypeAnnotations,
      node.invisibleTypeAnnotations)
    val sig: (Array[TypeParameter], Array[Type], Type, Array[ReferenceType]) = {
      if (node.signature != null) { Signature.methodSignature(node.signature) }
      else {
        val d = Descriptor.methodDescriptor(node.desc)
        (Array(), d._1, d._2, node.exceptions.asScala.toArray.map(x => Descriptor.classNameType(x)))
      }
    }
    val typeParameters: NodeList[TypeParameter] = new NodeList(sig._1:_*)
    def nullToList[A](x: Seq[A]): Seq[A] = { if (x == null) { List() } else { x } }
    val parameters: NodeList[Parameter] = {
      val ps = sig._2
        .zipWithIndex
        .zipAll(node.parameters match { case null => List(); case p => p.asScala }, null, null)
        .zipAll(nullToList(node.visibleParameterAnnotations), null, null)
        .zipAll(nullToList(node.invisibleParameterAnnotations), null, null)
      new NodeList(ps.map(decomParameter(node, sig._2.length)):_*)
    }
    val `type`: Type = sig._3
    val thrownExceptions: NodeList[ReferenceType] = new NodeList(sig._4:_*)
    val name: SimpleName = new SimpleName(node.name)
    val body: BlockStmt = null // TODO
    val receiverParameter: ReceiverParameter = null // TODO
    new MethodDeclaration(modifiers, annotations, typeParameters, `type`, name, parameters, thrownExceptions, body, receiverParameter)
  }

  private def asmToJavaParsers(nodes: java.util.List[_]*): NodeList[AnnotationExpr] = {
    val list = new NodeList[AnnotationExpr]()
    for (node <- nodes) {
      if (node != null) { list.addAll(node.asScala.map(x => asmToJavaParser(x.asInstanceOf[AnnotationNode])).asJava) }
    }
    list
  }

  def asmToJavaParser(node: ClassNode): CompilationUnit = {
    println(f"version: ${node.version}") // TODO
    println(f"sourceFile: ${node.sourceFile}") // TODO
    println(f"sourceDebug: ${node.sourceDebug}") // TODO
    // outerClass
    // outerMethod
    // outerMethodDesc
    // attr (ignore?)
    // innerClasses
    // nestHostClass
    // nestMember
    val fullClassName: Name = Descriptor.className(node.name)

    val packageDeclaration = new PackageDeclaration(
      new NodeList[AnnotationExpr]() /*TODO*/, fullClassName.getQualifier.orElse(new Name()))
    val imports = new NodeList[ImportDeclaration]() // TODO

    val classOrInterfaceDeclaration = {
      val modifiers = Modifier.modifiersToNodeList(Modifier.intToClass(node.access))
      val annotations: NodeList[AnnotationExpr] = asmToJavaParsers(
        node.visibleAnnotations,
        node.invisibleAnnotations,
        node.visibleTypeAnnotations,
        node.invisibleTypeAnnotations)
      val isInterface: Boolean = (node.access & Opcodes.ACC_INTERFACE) != 0
      val simpleName = new SimpleName(fullClassName.getIdentifier)
      // `extendedTypes` may be multiple if on an interface
      // TODO: test if should be Descriptor.className
      val (typeParameters, extendedTypes, implementedTypes):
        (NodeList[TypeParameter], NodeList[ClassOrInterfaceType], NodeList[ClassOrInterfaceType]) = {
        if (node.signature != null) {
          val s = Signature.classSignature(node.signature)
          (new NodeList(s._1:_*), new NodeList(s._2), new NodeList(s._3:_*))
        } else {
          (new NodeList(),
           if (node.superName == null) { new NodeList() }
           else { new NodeList(Descriptor.classNameType(node.superName)) },
           new NodeList(node.interfaces.asScala.map(x => Descriptor.classNameType(x)).asJava))
        }
      }
      val members: NodeList[BodyDeclaration[_ <: BodyDeclaration[_]]] = {
        val list = new NodeList[BodyDeclaration[_ <: BodyDeclaration[_]]]()
        list.addAll(node.fields.asScala.map(asmToJavaParser).asJava)
        list.addAll(node.methods.asScala.map(asmToJavaParser).asJava)
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

    val module = null // TODO node.module

    new CompilationUnit(packageDeclaration, imports, types, module)
  }
}
