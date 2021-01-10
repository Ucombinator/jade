package org.ucombinator.jade.classfile

import org.ucombinator.jade.util.Errors

import com.github.javaparser.ast.NodeList
import com.github.javaparser.ast.`type`._
import com.github.javaparser.ast.expr.{AnnotationExpr, SimpleName}
import sun.reflect.generics.parser.SignatureParser
import sun.reflect.generics.tree._

import scala.jdk.CollectionConverters._

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
      case _ => Errors.unmatchedType(t)
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
      case _: FieldTypeSignature => Errors.impossibleMatch(t)
      case _ => Errors.unmatchedType(t)
    }
  }
  private def translate(t: FieldTypeSignature): ReferenceType = {
    t match {
      case t: ClassTypeSignature => translate(t)
      case t: TypeVariableSignature => translate(t)
      case t: ArrayTypeSignature => translate(t)
      case t: SimpleClassTypeSignature => Errors.impossibleMatch(t)
      case t: BottomSignature => Errors.impossibleMatch(t)
      case _ => Errors.unmatchedType(t)
    }
  }
  private def translate(t: ClassTypeSignature): ClassOrInterfaceType = {
    t.getPath.asScala
      .foldLeft(null: ClassOrInterfaceType) { (scope, simpleClassTypeSignature) =>
        translate(scope, simpleClassTypeSignature)
      }
  }
  private def translate(scope: ClassOrInterfaceType, t: SimpleClassTypeSignature): ClassOrInterfaceType = {
    val name :: names = t.getName.split('.').toList.reverse: @scala.annotation.nowarn("msg=match may not be exhaustive")
    // TODO: ignored: simpleClassTypeSignature.getDollar
    new ClassOrInterfaceType(
      names.foldRight(scope) { (name, scope) => new ClassOrInterfaceType(scope, name) },
      new SimpleName(name),
      if (t.getTypeArguments.isEmpty) { null }
      else { new NodeList(t.getTypeArguments.map(translate): _*) }
    )
  }
  private def translate(t: TypeArgument): Type = {
    t match {
      case t: FieldTypeSignature => translate(t)
      case t: Wildcard => translate(t)
      case _ => Errors.unmatchedType(t)
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
      case x => Errors.impossibleMatch(x)
    }
  }
  private def translate(t: TypeVariableSignature): TypeParameter = {
    new TypeParameter(t.getIdentifier)
  }
  private def translate(t: ArrayTypeSignature): ArrayType = {
    new ArrayType(translate(t.getComponentType))
  }

  // Used by `classSignature`
  private def translate(
      t: ClassSignature
  ): (Array[TypeParameter], ClassOrInterfaceType, Array[ClassOrInterfaceType]) = {
    (t.getFormalTypeParameters.map(translate), translate(t.getSuperclass), t.getSuperInterfaces.map(translate))
  }
  private def referenceTypeToClassOrInterfaceType(t: ReferenceType): ClassOrInterfaceType = {
    t match {
      case t: ClassOrInterfaceType => t
      case t: TypeParameter =>
        assert(t.getTypeBound.isEmpty, f"non-empty type bounds in ${t}")
        // TODO: mark this as a type parameter
        new ClassOrInterfaceType(null, t.getName, null)
      case _ => Errors.unmatchedType(t)
    }
  }
  private def translate(t: FormalTypeParameter): TypeParameter = {
    new TypeParameter(
      t.getName,
      // TODO: distinguish between class and interface bounds?
      new NodeList(t.getBounds.map(translate).map(referenceTypeToClassOrInterfaceType): _*)
    )
  }

  // Used by methodSignature
  private def translate(t: MethodTypeSignature): (Array[TypeParameter], Array[Type], Type, Array[ReferenceType]) = {
    (
      t.getFormalTypeParameters.map(translate),
      t.getParameterTypes.map(translate),
      translate(t.getReturnType),
      t.getExceptionTypes.map(translate)
    )
  }
  private def translate(t: ReturnType): Type = {
    t match {
      case t: TypeSignature => translate(t)
      case _: VoidDescriptor => new VoidType
      case _ => Errors.unmatchedType(t)
    }
  }
}
