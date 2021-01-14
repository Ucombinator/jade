package org.ucombinator.jade.decompile

import scala.jdk.CollectionConverters._

import com.github.javaparser.ast.CompilationUnit
import com.github.javaparser.ast.ImportDeclaration
import com.github.javaparser.ast.NodeList
import com.github.javaparser.ast.PackageDeclaration
import com.github.javaparser.ast.`type`.ClassOrInterfaceType
import com.github.javaparser.ast.`type`.ReferenceType
import com.github.javaparser.ast.`type`.Type
import com.github.javaparser.ast.`type`.TypeParameter
import com.github.javaparser.ast.body._
import com.github.javaparser.ast.comments.BlockComment
import com.github.javaparser.ast.expr._
import com.github.javaparser.ast.stmt.BlockStmt
import org.objectweb.asm.Opcodes
import org.objectweb.asm.tree._
import org.ucombinator.jade.classfile.Descriptor
import org.ucombinator.jade.classfile.Flags
import org.ucombinator.jade.classfile.Signature
import org.ucombinator.jade.util.JavaParser
import org.ucombinator.jade.util.Log

object DecompileClass extends Log {

  def decompileLiteral(node: Object): Expression = {
    node match {
      // TODO: improve formatting of literals?
      case null => null
      case node: java.lang.Integer => new IntegerLiteralExpr(node.toString)
      case node: java.lang.Long => new LongLiteralExpr(node.toString)
      case node: java.lang.Float => new DoubleLiteralExpr(node.toString + "F") // `JavaParser` uses Doubles for Floats
      case node: java.lang.Double => new DoubleLiteralExpr(node.toString + "D")
      case node: java.lang.String => new StringLiteralExpr(node.toString)
      case node: org.objectweb.asm.Type => new ClassExpr(Descriptor.fieldDescriptor(node.getDescriptor))
      case _ => throw new Exception(f"unimplemented literal '${node}'")
    }
  }

  private def typeToName(t: Type): Name = t match {
    case null => null
    case t: ClassOrInterfaceType => new Name(typeToName(t.getScope.orElse(null)), t.getName.getIdentifier)
    case _ => throw new Exception(f"failed to convert type ${t} to a name")
  }

  private def decompileAnnotation(node: AnnotationNode): AnnotationExpr = {
    val name = typeToName(Descriptor.fieldDescriptor(node.desc))
    node.values.asScala match {
      case null => new MarkerAnnotationExpr(name)
      case List(v: Object) => new SingleMemberAnnotationExpr(name, decompileLiteral(v))
      case vs =>
        new NormalAnnotationExpr(
          name,
          new NodeList[MemberValuePair](
            (for (List(k, v) <- vs.grouped(2)) yield {
              new MemberValuePair(k.asInstanceOf[String], decompileLiteral(v.asInstanceOf[Object]))
            }).toList.asJava
          )
        )
    }
  }

  private def decompileAnnotations(nodes: java.util.List[_ <: AnnotationNode]*): NodeList[AnnotationExpr] = {
    val list = new NodeList[AnnotationExpr]()
    for (node <- nodes) {
      if (node != null) { list.addAll(node.asScala.map(x => decompileAnnotation(x)).asJava) }
    }
    list
  }

  private def decompileField(node: FieldNode): FieldDeclaration = {
    // attrs (ignore?)
    val modifiers = Flags.toModifiers(Flags.fieldFlags(node.access))
    val annotations: NodeList[AnnotationExpr] = decompileAnnotations(
      node.visibleAnnotations,
      node.invisibleAnnotations,
      node.visibleTypeAnnotations,
      node.invisibleTypeAnnotations
    )
    val variables = new NodeList[VariableDeclarator]({
      val `type`: Type =
        if (node.signature == null) { Descriptor.fieldDescriptor(node.desc) }
        else { Signature.typeSignature(node.signature) }
      val name = new SimpleName(node.name)
      val initializer: Expression = decompileLiteral(node.value)
      new VariableDeclarator(`type`, name, initializer)
    })

    new FieldDeclaration(modifiers, annotations, variables)
  }

  private def decompileParameter(
      method: MethodNode,
      paramCount: Int,
      parameter: ((((Type, Int), ParameterNode), java.util.List[AnnotationNode]), java.util.List[AnnotationNode])
  ): Parameter = {
    val ((((typ, index), node), a1), a2) = parameter
    val flags =
      if (node == null) { List() }
      else { Flags.parameterFlags(node.access) }
    val modifiers = Flags.toModifiers(flags)
    val annotations: NodeList[AnnotationExpr] = decompileAnnotations(a1, a2, null, null)
    val `type`: Type = typ
    val isVarArgs: Boolean =
      Flags.methodFlags(method.access).contains(Flags.ACC_VARARGS) &&
        index == paramCount - 1
    val varArgsAnnotations = new NodeList[AnnotationExpr]() // TODO?
    val name: SimpleName =
      new SimpleName(
        if (node == null) { f"parameter${index + 1}" }
        else { node.name }
      )
    new Parameter(modifiers, annotations, `type`, isVarArgs, varArgsAnnotations, name)
  }

  def parameterTypes(desc: List[Type], sig: List[Type], params: List[ParameterNode]): List[Type] = {
    (desc, sig, params) match {
      case (d :: ds, ss, p :: ps)
          if Flags.parameterFlags(p.access).contains(Flags.ACC_SYNTHETIC)
            || Flags.parameterFlags(p.access).contains(Flags.ACC_MANDATED) =>
        // TODO: Flags.checkParameter(access, Modifier)
        d :: parameterTypes(ds, ss, ps)
      case (_ :: ds, s :: ss, _ :: ps) =>
        s :: parameterTypes(ds, ss, ps)
      case (_, ss, List()) =>
        ss
      case _ =>
        throw new Exception(f"failed to construct parameter types: ${desc}, ${sig}, ${params}")
    }
  }

  def nullToSeq[A](x: java.util.List[A]): Seq[A] = {
    if (x eq null) { Seq() }
    else { Seq.from(x.asScala) }
  }
  def nullToSeq[A](x: Array[A]): Seq[A] = {
    if (x eq null) { Seq() }
    else { x.toSeq }
  }

  def decompileMethod(classNode: ClassNode, node: MethodNode): BodyDeclaration[_ <: BodyDeclaration[_]] = {
    // attr (ignore?)
    // instructions
    // tryCatchBlocks
    // localVariables
    // visibleLocalVariableAnnotations
    // invisibleLocalVariableAnnotations
    // TODO: JPModifier.Keyword.DEFAULT
    val modifiers = Flags.toModifiers(Flags.methodFlags(node.access))
    val annotations: NodeList[AnnotationExpr] = decompileAnnotations(
      node.visibleAnnotations,
      node.invisibleAnnotations,
      node.visibleTypeAnnotations,
      node.invisibleTypeAnnotations
    )
    val descriptor: (Array[Type], Type) = Descriptor.methodDescriptor(node.desc)
    val sig: (Array[TypeParameter], Array[Type], Type, Array[ReferenceType]) = {
      if (node.signature != null) { Signature.methodSignature(node.signature) }
      else {
        (Array(), descriptor._1, descriptor._2, node.exceptions.asScala.toArray.map(x => Descriptor.classNameType(x)))
      }
    }
    val parameterNodes = nullToSeq(node.parameters)
    if (node.parameters != null && sig._2.length != node.parameters.size()) {
      // TODO: check if always in an enum
    }
    val typeParameters: NodeList[TypeParameter] = new NodeList(sig._1: _*)
    val parameters: NodeList[Parameter] = {
      val ps = parameterTypes(descriptor._1.toList, sig._2.toList, parameterNodes.toList).zipWithIndex
        .zipAll(parameterNodes, null, null)
        .zipAll(nullToSeq(node.visibleParameterAnnotations), null, null)
        .zipAll(nullToSeq(node.invisibleParameterAnnotations), null, null)
      new NodeList(ps.map(decompileParameter(node, sig._2.length, _)): _*)
    }
    val `type`: Type = sig._3
    val thrownExceptions: NodeList[ReferenceType] = new NodeList(sig._4: _*)
    val name: SimpleName = new SimpleName(node.name)
    val body: BlockStmt = DecompileMethodBody.decompileBodyStub(node)
    val receiverParameter: ReceiverParameter = null // TODO
    val bodyDeclaration = node.name match {
      case "<clinit>" =>
        new InitializerDeclaration(true, body)
      case "<init>" =>
        new ConstructorDeclaration(
          modifiers,
          annotations,
          typeParameters,
          new SimpleName(Descriptor.className(classNode.name).getIdentifier) /*TODO*/,
          parameters,
          thrownExceptions,
          body,
          receiverParameter
        )
      case _ =>
        new MethodDeclaration(
          modifiers,
          annotations,
          typeParameters,
          `type`,
          name,
          parameters,
          thrownExceptions,
          body,
          receiverParameter
        )
    }
    Decompile.methods += bodyDeclaration -> ((classNode, node))
    bodyDeclaration
  }

  def decompileClass(node: ClassNode): CompilationUnit = {
    val comment = new BlockComment(
      f"""
         |* Source File: ${node.sourceFile}
         |* Class-file Format Version: ${node.version}
         |* Source Debug Extension: ${node.sourceDebug} // See JSR-45 https://www.jcp.org/en/jsr/detail?id=045
         |""".stripMargin
    )
    // outerClass
    // outerMethod
    // outerMethodDesc
    // attr (ignore?)
    // innerClasses
    // nestHostClass
    // nestMember

    val fullClassName: Name = Descriptor.className(node.name)

    val packageDeclaration =
      new PackageDeclaration(new NodeList[AnnotationExpr]() /*TODO*/, fullClassName.getQualifier.orElse(new Name()))
    val imports = new NodeList[ImportDeclaration]() // TODO

    val classOrInterfaceDeclaration = {
      // TODO: assert ACC_SUPER
      val modifiers = Flags.toModifiers(Flags.classFlags(node.access))
      val annotations: NodeList[AnnotationExpr] = decompileAnnotations(
        node.visibleAnnotations,
        node.invisibleAnnotations,
        node.visibleTypeAnnotations,
        node.invisibleTypeAnnotations
      )
      val isInterface: Boolean = (node.access & Opcodes.ACC_INTERFACE) != 0
      val simpleName = new SimpleName(fullClassName.getIdentifier)
      // `extendedTypes` may be multiple if on an interface
      // TODO: test if should be Descriptor.className
      val (typeParameters, extendedTypes, implementedTypes): (
          NodeList[TypeParameter],
          NodeList[ClassOrInterfaceType],
          NodeList[ClassOrInterfaceType]
      ) = {
        if (node.signature != null) {
          val s = Signature.classSignature(node.signature)
          (new NodeList(s._1: _*), new NodeList(s._2), new NodeList(s._3: _*))
        } else {
          (
            new NodeList(),
            if (node.superName == null) {
              new NodeList()
            } else {
              new NodeList(Descriptor.classNameType(node.superName))
            },
            new NodeList(node.interfaces.asScala.map(x => Descriptor.classNameType(x)).asJava)
          )
        }
      }
      val members: NodeList[BodyDeclaration[_ <: BodyDeclaration[_]]] = {
        val list = new NodeList[BodyDeclaration[_ <: BodyDeclaration[_]]]()
        list.addAll(node.fields.asScala.map(decompileField).asJava)
        list.addAll(node.methods.asScala.map(decompileMethod(node, _)).asJava)
        // TODO
        list
      }

      new ClassOrInterfaceDeclaration(
        modifiers,
        annotations,
        isInterface,
        simpleName,
        typeParameters,
        extendedTypes,
        implementedTypes,
        members
      )
    }

    if (classOrInterfaceDeclaration.isInterface) {
      classOrInterfaceDeclaration.setExtendedTypes(classOrInterfaceDeclaration.getImplementedTypes)
      classOrInterfaceDeclaration.setImplementedTypes(new NodeList())
    }

    val types = new NodeList[TypeDeclaration[_ <: TypeDeclaration[_]]]()
    types.add(classOrInterfaceDeclaration)

    // TODO: ModuleExportNode
    // TODO: ModuleNode
    // TODO: ModuleOpenNode
    // TODO: ModuleProvideNode
    // TODO: ModuleRequireNode
    val module = null // TODO node.module

    val compilationUnit = new CompilationUnit(packageDeclaration, imports, types, module)
    JavaParser.setComment(compilationUnit, comment)
    Decompile.classes += compilationUnit -> node
    this.log.debug("++++ decompile class ++++\n" + compilationUnit.toString)
    compilationUnit
  }
}
