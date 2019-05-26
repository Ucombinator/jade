package org.ucombinator.jade.classfile

import com.github.javaparser.ast.NodeList
import com.github.javaparser.ast.`type`.{ArrayType, ClassOrInterfaceType, PrimitiveType, ReferenceType, Type, VoidType, WildcardType, TypeParameter => JPTypeParamter}
import com.github.javaparser.ast.expr.{AnnotationExpr, SimpleName}

import scala.collection.JavaConverters._
import scala.util.parsing.combinator.RegexParsers
import sun.reflect.generics.parser.SignatureParser
import sun.reflect.generics.tree.{ArrayTypeSignature, BaseType, BooleanSignature, BottomSignature, ByteSignature, CharSignature, ClassSignature, ClassTypeSignature, DoubleSignature, FieldTypeSignature, FloatSignature, FormalTypeParameter, IntSignature, LongSignature, MethodTypeSignature, ReturnType, ShortSignature, SimpleClassTypeSignature, TypeArgument, TypeSignature, TypeVariableSignature, VoidDescriptor, Wildcard}

// Grammar defined in JVMS 4.7.9.1
object Signature extends RegexParsers {
  def classSignature2(string: String): (Array[JPTypeParamter], ClassOrInterfaceType, Array[ClassOrInterfaceType]) = {
    parse3(SignatureParser.make().parseClassSig(string))
  }
  def methodSignature2(string: String): (Array[JPTypeParamter], Array[Type], Type, Array[ReferenceType]) = {
    parse3(SignatureParser.make().parseMethodSig(string))
  }
  def typeSignature2(string: String): Type = {
    parse3(SignatureParser.make().parseTypeSig(string))
  }
  private def parse3(methodTypeSignature: MethodTypeSignature): (Array[JPTypeParamter], Array[Type], Type, Array[ReferenceType]) = {
    (methodTypeSignature.getFormalTypeParameters.map(parse3),
    methodTypeSignature.getParameterTypes.map(parse3),
    parse3(methodTypeSignature.getReturnType),
    methodTypeSignature.getExceptionTypes.map(parse3).map(_.asInstanceOf[ReferenceType]))
  }
  private def parse3(returnType: ReturnType): Type = {
    returnType match {
      case returnType: VoidDescriptor => new VoidType
      case returnType: TypeSignature => parse3(returnType)
    }
  }
  private def parse3(classSig: ClassSignature): (Array[JPTypeParamter], ClassOrInterfaceType, Array[ClassOrInterfaceType]) = {
    (classSig.getFormalTypeParameters.map(parse3),
    parse3(classSig.getSuperclass),
    classSig.getSuperInterfaces.map(parse3))
  }
  private def parse3(formalTypeParameter: FormalTypeParameter): JPTypeParamter = {
    new JPTypeParamter(
      formalTypeParameter.getName,
      new NodeList(formalTypeParameter.getBounds.map(parse3).map(_.asInstanceOf[ClassOrInterfaceType]):_*))
  }
  private def parse3(fieldTypeSignature: FieldTypeSignature): Type = {
    fieldTypeSignature match {
      case fieldTypeSignature: ArrayTypeSignature => parse3(fieldTypeSignature)
      case fieldTypeSignature: BottomSignature => parse3(fieldTypeSignature)
      case fieldTypeSignature: ClassTypeSignature => parse3(fieldTypeSignature)
      case fieldTypeSignature: SimpleClassTypeSignature => parse3(fieldTypeSignature)
      case fieldTypeSignature: TypeVariableSignature => parse3(fieldTypeSignature)
    }
  }
  private def parse3(bottomSignature: BottomSignature): Type = {
    ???
  }
  private def parse3(typeVariableSignature: TypeVariableSignature): Type = {
    new JPTypeParamter(typeVariableSignature.getIdentifier)
  }
  private def parse3(arrayTypeSignature: ArrayTypeSignature): Type = {
    new ArrayType(parse3(arrayTypeSignature.getComponentType))
  }
  private def parse3(classTypeSignature: ClassTypeSignature): ClassOrInterfaceType = {
    classTypeSignature.getPath.asScala
      .foldLeft(null: ClassOrInterfaceType)
      { (scope, simpleClassTypeSignature) => parse3(scope, simpleClassTypeSignature) }
  }
  private def parse3(initialScope: ClassOrInterfaceType = null, simpleClassTypeSignature: SimpleClassTypeSignature): ClassOrInterfaceType = {
    val name :: names = simpleClassTypeSignature.getName.split('.').toList.reverse
    val scope = names.foldRight(initialScope){ (name, scope) => new ClassOrInterfaceType(scope, name) }
    // TODO: ignored: simpleClassTypeSignature.getDollar
    new ClassOrInterfaceType(
      scope,
      new SimpleName(name),
      if (simpleClassTypeSignature.getTypeArguments.isEmpty) { null }
      else { new NodeList(simpleClassTypeSignature.getTypeArguments.map(parse3):_*) })
  }
  private def parse3(typeArgument: TypeArgument): Type = {
    typeArgument match {
      case typeArgument: FieldTypeSignature => parse3(typeArgument)
      case typeArgument: Wildcard => parse3(typeArgument)
    }
  }
  private def parse3(wildcard: Wildcard): WildcardType = {
    (wildcard.getLowerBounds, wildcard.getUpperBounds) match {
      case (Array(), Array(o: SimpleClassTypeSignature)) if o.getName == "java.lang.Object" =>
        // *
        new WildcardType()
      case (Array(), Array(t)) =>
        // +
        // TODO: use Type.asReferenceType
        new WildcardType(parse3(t).asInstanceOf[ReferenceType])
      case (Array(t), Array(o: SimpleClassTypeSignature)) if o.getName == "java.lang.Object" =>
        // -
        new WildcardType(null, parse3(t).asInstanceOf[ReferenceType], new NodeList[AnnotationExpr]())
    }
  }
  private def parse3(typeSignature: TypeSignature): Type = {
    typeSignature match {
      case typeSignature: FieldTypeSignature => parse3(typeSignature)
      case typeSignature: BaseType => parse3(typeSignature)
    }
  }
  private def parse3(baseType: BaseType): Type = {
    baseType match {
      case baseType: BooleanSignature => PrimitiveType.booleanType()
      case baseType: ByteSignature => PrimitiveType.byteType()
      case baseType: CharSignature => PrimitiveType.charType()
      case baseType: DoubleSignature => PrimitiveType.doubleType()
      case baseType: FloatSignature => PrimitiveType.floatType()
      case baseType: IntSignature => PrimitiveType.intType()
      case baseType: LongSignature => PrimitiveType.longType()
      case baseType: ShortSignature => PrimitiveType.shortType()
      case baseType: FieldTypeSignature => ??? // TODO: is this possible?
    }
  }
}
