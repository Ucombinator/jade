package org.ucombinator.jade.classfile

import com.github.javaparser.ast.`type`.{ArrayType, ClassOrInterfaceType, PrimitiveType, Type, VoidType}
import com.github.javaparser.ast.expr.Name

import scala.util.parsing.combinator.RegexParsers

// Grammar defined by JVMS 4.3
object Descriptor extends RegexParsers {
  private def parse[T](p: Parser[T], name: String): String => T = {
    string: String =>
      parseAll(p, string) match {
        case Error(msg, _) => throw new Exception(f"parse error '$msg' in $name '$string'")
        case Failure(msg, _) => throw new Exception(f"parse failure '$msg' in $name '$string'")
        case Success(typ, i) =>
          if (!i.atEnd) { throw new Exception(f"parse incomplete at position ${i.pos} in $name '$string'") }
          else { typ }
      }
  }
  val fieldDescriptor: String => Type = { parse(FieldDescriptor, "field descriptor") }
  val methodDescriptor: String => (List[Type], Type) = { parse(MethodDescriptor, "method descriptor") }
  val className: String => Name = { parse(ClassName, "class name") }

  def nameToType(t: Name): ClassOrInterfaceType = {
    if (t == null) { null }
    else { new ClassOrInterfaceType(nameToType(t.getQualifier.orElse(null)), t.getIdentifier) }
  }

  // Used by `className`
  private lazy val ClassName: Parser[Name] =
    rep1sep("""[^;\[/]+""".r, '/') ^^ {
      _.foldLeft(null: Name){ (qualifier, identifier) => new Name(qualifier, identifier) } }

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
    'L' ~> ClassName <~ ';' ^^ nameToType

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
