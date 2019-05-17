package org.ucombinator.jade.classfile

import com.github.javaparser.ast.`type`.{ArrayType, ClassOrInterfaceType, PrimitiveType, Type, VoidType}

import scala.util.parsing.combinator.RegexParsers

object Descriptor extends RegexParsers {
  def fieldDescriptor(string: String): Type = {
    println(f"fieldDescriptor: $string")
    parseAll(FieldDescriptor, string) match {
      case Error(msg, _) => throw new Exception(f"parse error '$msg' in field descriptor '$string'")
      case Failure(msg, _) => throw new Exception(f"parse failure '$msg' in field descriptor '$string'")
      case Success(typ, _) => typ
    }
  }

  def methodDescriptor(string: String): (List[Type], Type) = {
    parseAll(MethodDescriptor, string) match {
      case Error(msg, _) => throw new Exception(f"parse error '$msg' in method descriptor '$string'")
      case Failure(msg, _) => throw new Exception(f"parse failure '$msg' in method descriptor '$string'")
      case Success(typ, _) => typ
    }
  }

  // JVMS 4.3
  private lazy val ClassName: Parser[List[String]] =
    rep1sep("[^;\\[/]+".r, '/')

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
    'L' ~> ClassName <~ ';' ^^ { _.foldLeft(null: ClassOrInterfaceType){ (scope, name) => new ClassOrInterfaceType(scope, name) } }

  private lazy val ArrayType: Parser[Type] =
    '[' ~> ComponentType ^^ { new ArrayType(_) }

  private lazy val ComponentType: Parser[Type] =
    FieldType

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

/*
  private val descriptor: Parser[Type] =
    ( 'Z' ^^^ PrimitiveType.booleanType()
    | 'C' ^^^ PrimitiveType.charType()
    | 'B' ^^^ PrimitiveType.byteType()
    | 'S' ^^^ PrimitiveType.shortType()
    | 'I' ^^^ PrimitiveType.intType()
    | 'J' ^^^ PrimitiveType.longType()
    | 'F' ^^^ PrimitiveType.floatType()
    | 'D' ^^^ PrimitiveType.doubleType()
    | 'V' ^^^ new VoidType
    | 'L' ~> "[^;]*".r <~ ";" ^^ { new ClassOrInterfaceType(null, _) }
    | '[' ~> descriptor ^^ { new ArrayType(_) } )
    // TODO: ( {ParameterDescriptor} ) ReturnDescriptor
 */
}
