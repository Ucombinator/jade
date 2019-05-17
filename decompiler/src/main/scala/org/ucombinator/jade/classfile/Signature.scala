package org.ucombinator.jade.classfile

import com.github.javaparser.ast.NodeList
import com.github.javaparser.ast.`type`.{ArrayType, ClassOrInterfaceType, PrimitiveType, ReferenceType, Type, VoidType, WildcardType, TypeParameter => JPTypeParamter}
import com.github.javaparser.ast.expr.SimpleName

import scala.collection.JavaConverters._
import scala.util.parsing.combinator.RegexParsers

object Signature extends RegexParsers {
  private def parse[T](p: Parser[T], name: String, string: String): T = {
    parseAll(p, string) match {
      case Error(msg, _) => throw new Exception(f"parse error '$msg' in $name '$string'")
      case Failure(msg, _) => throw new Exception(f"parse failure '$msg' in $name '$string'")
      case Success(typ, _) => typ
    }
  }
  def javaTypeSignature(string: String): Type = { parse(JavaTypeSignature, "Java type signature", string) }
  def classSignature(string: String): Type = { parse(ClassSignature, "class signature", string) }
  def methodSignature(string: String): (List[JPTypeParamter], List[Type], Type, List[Type]) = {
    parse(MethodSignature, "method signature", string)
  }
  def fieldSignature(string: String): Type = { parse(FieldSignature, "field signature", string) }

// See JVMS 4.7.9.1
  private lazy val JavaTypeSignature: Parser[Type] =
    ReferenceTypeSignature |
    BaseType

  private lazy val BaseType: Parser[Type] =
    'B' ^^^ PrimitiveType.byteType() |
    'C' ^^^ PrimitiveType.charType() |
    'D' ^^^ PrimitiveType.doubleType() |
    'F' ^^^ PrimitiveType.floatType() |
    'I' ^^^ PrimitiveType.intType() |
    'J' ^^^ PrimitiveType.longType() |
    'S' ^^^ PrimitiveType.shortType() |
    'Z' ^^^ PrimitiveType.booleanType()

  private lazy val Identifier: Parser[String] = "[^.;\\[/<>:]*".r

  private lazy val ReferenceTypeSignature: Parser[ReferenceType] =
    ClassTypeSignature |
    TypeVariableSignature |
    ArrayTypeSignature

  private lazy val ClassTypeSignature: Parser[ReferenceType] =
    'L' ~> PackageSpecifier.? ~ SimpleClassTypeSignature ~ ClassTypeSignatureSuffix.* <~ ';' ^^ {
      case packageSpecifier ~ simpleClassTypeSignature ~ classTypeSignatureSuffix =>
        val type1 = packageSpecifier.toList.flatten.foldLeft(null: ClassOrInterfaceType){ (scope, name) => new ClassOrInterfaceType(scope, name) }
        val type2 = new ClassOrInterfaceType(type1, simpleClassTypeSignature.getName, simpleClassTypeSignature.getTypeArguments.orElse(null))
        val type3 = classTypeSignatureSuffix.foldLeft(type2){ (scope, suffix) => new ClassOrInterfaceType(scope, suffix.getName, suffix.getTypeArguments.orElse(null)) }
        type3 }
  private lazy val PackageSpecifier: Parser[List[String]] =
    Identifier ~ '/' ~ PackageSpecifier.? ^^ { // Typo in spec has {} when it should be []
      case identifier ~ _ ~ Some(packageSpecifier) => identifier :: packageSpecifier
      case identifier ~ _ ~ None => List(identifier) }
  private lazy val SimpleClassTypeSignature: Parser[ClassOrInterfaceType] =
    Identifier ~ TypeArguments.? ^^
    { case identifier ~ None => new ClassOrInterfaceType(null, identifier)
      case identifier ~ Some(typeArguments) => new ClassOrInterfaceType(null, new SimpleName(identifier), new NodeList[Type](typeArguments.asJava)) }
  private lazy val TypeArguments: Parser[List[Type]] =
    '<' ~> TypeArgument ~ TypeArgument.* <~ '>' ^^ { case arg ~ args => arg :: args }
  private lazy val TypeArgument: Parser[Type] =
    WildcardIndicator.? ~ ReferenceTypeSignature ^^ {
      case None ~ referenceTypeSignature => referenceTypeSignature
      case Some(true) ~ referenceTypeSignature => new WildcardType(referenceTypeSignature) // TODO: these might be backwards
      case Some(false) ~ referenceTypeSignature => new WildcardType(null: ReferenceType, referenceTypeSignature, null) } |
    '*' ^^^ new WildcardType()
  private lazy val WildcardIndicator: Parser[Boolean] =
    '+' ^^^ true |
    '-' ^^^ false
  private lazy val ClassTypeSignatureSuffix: Parser[ClassOrInterfaceType] =
    '.' ~> SimpleClassTypeSignature
  private lazy val TypeVariableSignature: Parser[ReferenceType] =
    'T' ~> Identifier <~ ';' ^^ { new JPTypeParamter(_) }
  private lazy val ArrayTypeSignature: Parser[ReferenceType] =
    '[' ~> JavaTypeSignature ^^ { new ArrayType(_) }

  private lazy val ClassSignature: Parser[Type] =
    TypeParameters.? ~ SuperclassSignature ~ SuperinterfaceSignature.* ^^
    { case _ ~ _ ~ _ => ??? }
  private lazy val TypeParameters: Parser[List[JPTypeParamter]] =
    '<' ~> TypeParameter ~ TypeParameter.* <~ '>' ^^
    { case param ~ params => param :: params }
  private lazy val TypeParameter: Parser[JPTypeParamter] =
    Identifier ~ ClassBound ~ InterfaceBound.* ^^
    { case name ~ classBound ~ interfaceBound =>
      new JPTypeParamter(name, new NodeList[ClassOrInterfaceType]((classBound :: interfaceBound).flatten.asJava)) }
  private lazy val ClassBound: Parser[Option[ClassOrInterfaceType]] =
    ':' ~> ReferenceTypeSignature.? ^^ { _.asInstanceOf[Option[ClassOrInterfaceType]] }
  private lazy val InterfaceBound: Parser[Option[ClassOrInterfaceType]] =
    ':' ~> ReferenceTypeSignature.? ^^ { _.asInstanceOf[Option[ClassOrInterfaceType]] }
  private lazy val SuperclassSignature: Parser[Type] =
    ClassTypeSignature
  private lazy val SuperinterfaceSignature: Parser[Type] =
    ClassTypeSignature

  private lazy val MethodSignature: Parser[(List[JPTypeParamter], List[Type], Type, List[Type])] =
    TypeParameters.? ~ ('(' ~> JavaTypeSignature.* <~ ')') ~ Result ~ ThrowsSignature.* ^^
    { case typeParameters ~ javaTypeSignature ~ result ~ throwsSignature =>
      (typeParameters.orNull, javaTypeSignature, result, throwsSignature) }
  private lazy val Result: Parser[Type] =
    JavaTypeSignature |
    VoidDescriptor
  private lazy val ThrowsSignature: Parser[Type] =
    '^' ~> ClassTypeSignature |
    '^' ~> TypeVariableSignature

  private lazy val VoidDescriptor: Parser[Type] =
    'V' ^^^ new VoidType

  private lazy val FieldSignature: Parser[Type] =
    ReferenceTypeSignature
}
