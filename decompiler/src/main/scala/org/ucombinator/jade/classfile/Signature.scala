package org.ucombinator.jade.classfile

import com.github.javaparser.ast.NodeList
import com.github.javaparser.ast.`type`.{ArrayType, ClassOrInterfaceType, PrimitiveType, ReferenceType, Type, VoidType, WildcardType, TypeParameter}
import com.github.javaparser.ast.expr.{AnnotationExpr, SimpleName}

import sun.reflect.generics.parser.SignatureParser
import sun.reflect.generics.tree.{ArrayTypeSignature, BaseType, BooleanSignature, ByteSignature, CharSignature, ClassSignature, ClassTypeSignature, DoubleSignature, FieldTypeSignature, FloatSignature, FormalTypeParameter, IntSignature, LongSignature, MethodTypeSignature, ReturnType, ShortSignature, SimpleClassTypeSignature, TypeArgument, TypeSignature, TypeVariableSignature, VoidDescriptor, Wildcard}

import scala.collection.JavaConverters._

// NOTE: The structure of this class follows that of the Java Virtual Machine Specification section 4.7.9.1 "Signatures"
object Signature {
  def typeSignature(string: String): Type = {
    translate(SignatureParser.make().parseTypeSig(string))
  }
  def classSignature(string: String): (Array[TypeParameter], ClassOrInterfaceType, Array[ClassOrInterfaceType]) = {
    translate(SignatureParser.make().parseClassSig(string))
  }
  def methodSignature(string: String): (Array[TypeParameter], Array[Type], Type, Array[ReferenceType]) = {
    translate(SignatureParser.make().parseMethodSig(string))
  }

  // Used by `typeSignature`
  private def translate(t: TypeSignature): Type = {
    t match {
      case t: FieldTypeSignature => translate(t)
      case t: BaseType => translate(t)
    }
  }
  private def translate(t: BaseType): PrimitiveType = {
    t match {
      case _: ByteSignature => PrimitiveType.byteType()
      case _: CharSignature => PrimitiveType.charType()
      case _: DoubleSignature => PrimitiveType.doubleType()
      case _: FloatSignature => PrimitiveType.floatType()
      case _: IntSignature => PrimitiveType.intType()
      case _: LongSignature => PrimitiveType.longType()
      case _: ShortSignature => PrimitiveType.shortType()
      case _: BooleanSignature => PrimitiveType.booleanType()
      // The following case should never happen:
      //case baseType: FieldTypeSignature => ???
    }
  }
  private def translate(t: FieldTypeSignature): ReferenceType = {
    t match {
      case t: ClassTypeSignature => translate(t)
      case t: SimpleClassTypeSignature => translate(t)
      case t: TypeVariableSignature => translate(t)
      case t: ArrayTypeSignature => translate(t)
      // The following case should never happen:
      //case fieldTypeSignature: BottomSignature => translate(fieldTypeSignature)
    }
  }
  private def translate(t: ClassTypeSignature): ClassOrInterfaceType = {
    t.getPath.asScala
      .foldLeft(null: ClassOrInterfaceType)
      { (scope, simpleClassTypeSignature) => translate(scope, simpleClassTypeSignature) }
  }
  private def translate(t: SimpleClassTypeSignature): ClassOrInterfaceType = {
    translate(null, t)
  }
  private def translate(scope: ClassOrInterfaceType, t: SimpleClassTypeSignature): ClassOrInterfaceType = {
    val name :: names = t.getName.split('.').toList.reverse
    // TODO: ignored: simpleClassTypeSignature.getDollar
    new ClassOrInterfaceType(
      names.foldRight(scope){ (name, scope) => new ClassOrInterfaceType(scope, name) },
      new SimpleName(name),
      if (t.getTypeArguments.isEmpty) { null }
      else { new NodeList(t.getTypeArguments.map(translate):_*) })
  }
  private def translate(t: TypeArgument): Type = {
    t match {
      case t: FieldTypeSignature => translate(t)
      case t: Wildcard => translate(t)
    }
  }
  private def translate(t: Wildcard): WildcardType = {
    (t.getLowerBounds, t.getUpperBounds) match {
      case (Array(), Array(o: SimpleClassTypeSignature)) if o.getName == "java.lang.Object" =>
        // *
        new WildcardType()
      case (Array(), Array(upperBound)) =>
        // +
        // TODO: consider using Type.asReferenceType
        new WildcardType(translate(upperBound))
      case (Array(lowerBound), Array(o: SimpleClassTypeSignature)) if o.getName == "java.lang.Object" =>
        // -
        new WildcardType(null, translate(lowerBound), new NodeList[AnnotationExpr]())
    }
  }
  private def translate(t: TypeVariableSignature): TypeParameter = {
    new TypeParameter(t.getIdentifier)
  }
  private def translate(t: ArrayTypeSignature): ArrayType = {
    new ArrayType(translate(t.getComponentType))
  }

  // Used by `classSignature`
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

  // Used by methodSignature
  private def translate(t: MethodTypeSignature): (Array[TypeParameter], Array[Type], Type, Array[ReferenceType]) = {
    (t.getFormalTypeParameters.map(translate),
     t.getParameterTypes.map(translate),
     translate(t.getReturnType),
     t.getExceptionTypes.map(translate))
  }
  private def translate(t: ReturnType): Type = {
    t match {
      case t: TypeSignature => translate(t)
      case _: VoidDescriptor => new VoidType
    }
  }
}
