package org.ucombinator.jade.classfile

import com.github.javaparser.ast.NodeList
import com.github.javaparser.ast.`type`.{ArrayType, ClassOrInterfaceType, PrimitiveType, ReferenceType, Type, VoidType, WildcardType, TypeParameter}
import com.github.javaparser.ast.expr.{AnnotationExpr, SimpleName}

import scala.collection.JavaConverters._
import scala.util.parsing.combinator.RegexParsers
import sun.reflect.generics.parser.SignatureParser
import sun.reflect.generics.tree.{ArrayTypeSignature, BaseType, BooleanSignature, ByteSignature, CharSignature, ClassSignature, ClassTypeSignature, DoubleSignature, FieldTypeSignature, FloatSignature, FormalTypeParameter, IntSignature, LongSignature, MethodTypeSignature, ReturnType, ShortSignature, SimpleClassTypeSignature, TypeArgument, TypeSignature, TypeVariableSignature, VoidDescriptor, Wildcard}

// Grammar defined in JVMS 4.7.9.1
object Signature extends RegexParsers {
  def classSignature(string: String): (Array[TypeParameter], ClassOrInterfaceType, Array[ClassOrInterfaceType]) = {
    translate(SignatureParser.make().parseClassSig(string))
  }
  def methodSignature(string: String): (Array[TypeParameter], Array[Type], Type, Array[ReferenceType]) = {
    translate(SignatureParser.make().parseMethodSig(string))
  }
  def typeSignature(string: String): Type = {
    translate(SignatureParser.make().parseTypeSig(string))
  }
  private def translate(t: MethodTypeSignature): (Array[TypeParameter], Array[Type], Type, Array[ReferenceType]) = {
    (t.getFormalTypeParameters.map(translate),
    t.getParameterTypes.map(translate),
    translate(t.getReturnType),
    t.getExceptionTypes.map(translate).map(_.asInstanceOf[ReferenceType]))
  }
  private def translate(t: ReturnType): Type = {
    t match {
      case returnType: VoidDescriptor => new VoidType
      case returnType: TypeSignature => translate(returnType)
    }
  }
  private def translate(t: ClassSignature): (Array[TypeParameter], ClassOrInterfaceType, Array[ClassOrInterfaceType]) = {
    (t.getFormalTypeParameters.map(translate),
    translate(t.getSuperclass),
    t.getSuperInterfaces.map(translate))
  }
  private def translate(t: FormalTypeParameter): TypeParameter = {
    new TypeParameter(
      t.getName,
      new NodeList(t.getBounds.map(translate).map(_.asInstanceOf[ClassOrInterfaceType]):_*))
  }
  private def translate(t: FieldTypeSignature): Type = {
    t match {
      case fieldTypeSignature: ArrayTypeSignature => translate(fieldTypeSignature)
      // The following case should never happen:
      //case fieldTypeSignature: BottomSignature => translate(fieldTypeSignature)
      case fieldTypeSignature: ClassTypeSignature => translate(fieldTypeSignature)
      case fieldTypeSignature: SimpleClassTypeSignature => translate(fieldTypeSignature)
      case fieldTypeSignature: TypeVariableSignature => translate(fieldTypeSignature)
    }
  }
  private def translate(t: TypeVariableSignature): Type = {
    new TypeParameter(t.getIdentifier)
  }
  private def translate(t: ArrayTypeSignature): Type = {
    new ArrayType(translate(t.getComponentType))
  }
  private def translate(t: ClassTypeSignature): ClassOrInterfaceType = {
    t.getPath.asScala
      .foldLeft(null: ClassOrInterfaceType)
      { (scope, simpleClassTypeSignature) => translate(scope, simpleClassTypeSignature) }
  }
  private def translate(initialScope: ClassOrInterfaceType = null, simpleClassTypeSignature: SimpleClassTypeSignature): ClassOrInterfaceType = {
    val name :: names = simpleClassTypeSignature.getName.split('.').toList.reverse
    val scope = names.foldRight(initialScope){ (name, scope) => new ClassOrInterfaceType(scope, name) }
    // TODO: ignored: simpleClassTypeSignature.getDollar
    new ClassOrInterfaceType(
      scope,
      new SimpleName(name),
      if (simpleClassTypeSignature.getTypeArguments.isEmpty) { null }
      else { new NodeList(simpleClassTypeSignature.getTypeArguments.map(translate):_*) })
  }
  private def translate(t: TypeArgument): Type = {
    t match {
      case typeArgument: FieldTypeSignature => translate(typeArgument)
      case typeArgument: Wildcard => translate(typeArgument)
    }
  }
  private def translate(t: Wildcard): WildcardType = {
    (t.getLowerBounds, t.getUpperBounds) match {
      case (Array(), Array(o: SimpleClassTypeSignature)) if o.getName == "java.lang.Object" =>
        // *
        new WildcardType()
      case (Array(), Array(t)) =>
        // +
        // TODO: consider using Type.asReferenceType
        new WildcardType(translate(t).asInstanceOf[ReferenceType])
      case (Array(t), Array(o: SimpleClassTypeSignature)) if o.getName == "java.lang.Object" =>
        // -
        new WildcardType(null, translate(t).asInstanceOf[ReferenceType], new NodeList[AnnotationExpr]())
    }
  }
  private def translate(t: TypeSignature): Type = {
    t match {
      case typeSignature: FieldTypeSignature => translate(typeSignature)
      case typeSignature: BaseType => translate(typeSignature)
    }
  }
  private def translate(t: BaseType): Type = {
    t match {
      case baseType: BooleanSignature => PrimitiveType.booleanType()
      case baseType: ByteSignature => PrimitiveType.byteType()
      case baseType: CharSignature => PrimitiveType.charType()
      case baseType: DoubleSignature => PrimitiveType.doubleType()
      case baseType: FloatSignature => PrimitiveType.floatType()
      case baseType: IntSignature => PrimitiveType.intType()
      case baseType: LongSignature => PrimitiveType.longType()
      case baseType: ShortSignature => PrimitiveType.shortType()
      // The following case should never happen:
      //case baseType: FieldTypeSignature => ???
    }
  }
}
