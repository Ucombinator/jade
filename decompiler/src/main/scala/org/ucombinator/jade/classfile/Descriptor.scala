package org.ucombinator.jade.classfile

import com.github.javaparser.ast.`type`.{ArrayType, ClassOrInterfaceType, PrimitiveType, Type, VoidType}

import scala.util.parsing.combinator.RegexParsers

// Grammar defined by JVMS 4.3
object Descriptor extends RegexParsers {
  private def parse[T](p: Parser[T], name: String): String => T = {
    string: String =>
      parseAll(p, string) match {
        case Error(msg, _) => throw new Exception(f"parse error '$msg' in $name '$string'")
        case Failure(msg, _) => throw new Exception(f"parse failure '$msg' in $name '$string'")
        case Success(typ, _) => typ
      }
  }
  val fieldDescriptor: String => Type = { parse(FieldDescriptor, "field descriptor") }
  val methodDescriptor: String => (List[Type], Type) = { parse(MethodDescriptor, "method descriptor") }
  val className: String => ClassOrInterfaceType = { parse(ClassName, "class name") }

  // Used by `className`
  private lazy val ClassName: Parser[ClassOrInterfaceType] =
    rep1sep("[^;\\[/]+".r, '/') ^^ {
      _.foldLeft(null: ClassOrInterfaceType){ (scope, name) => new ClassOrInterfaceType(scope, name) } }

  // Used by `fieldDescriptor`
  private lazy val FieldDescriptor: Parser[Type] =
    FieldType

  private lazy val FieldType: Parser[Type] =
    BaseType |
    ObjectType |
    ArrayType

  private lazy val BaseType: Parser[Type] =
    'B' ^^^ PrimitiveType.byteType() |
    'C' ^^^ PrimitiveType.charType() |
    'D' ^^^ PrimitiveType.doubleType() |
    'F' ^^^ PrimitiveType.floatType() |
    'I' ^^^ PrimitiveType.intType() |
    'J' ^^^ PrimitiveType.longType() |
    'S' ^^^ PrimitiveType.shortType() |
    'Z' ^^^ PrimitiveType.booleanType()

  private lazy val ObjectType: Parser[Type] =
    'L' ~> ClassName <~ ';'

  private lazy val ArrayType: Parser[Type] =
    '[' ~> ComponentType ^^ { new ArrayType(_) }

  private lazy val ComponentType: Parser[Type] =
    FieldType

  // Used by `methodDescriptor`
  private lazy val MethodDescriptor: Parser[(List[Type], Type)] =
    ('(' ~> ParameterDescriptor.* <~ ')') ~ ReturnDescriptor ^^ {
      case parameterDescriptor ~ returnDescriptor => (parameterDescriptor, returnDescriptor) }

  private lazy val ParameterDescriptor: Parser[Type] =
    FieldType

  private lazy val ReturnDescriptor: Parser[Type] =
    FieldType |
    VoidDescriptor

  private lazy val VoidDescriptor: Parser[Type] =
    'V' ^^^ new VoidType()
}
