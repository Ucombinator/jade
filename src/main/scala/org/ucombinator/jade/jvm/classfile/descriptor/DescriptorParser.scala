package org.ucombinator.jade.jvm.classfile.descriptor

import scala.language.implicitConversions
import org.ucombinator.jade.jvm.classfile._
import org.ucombinator.jade.jvm.classfile.TypeCommons._
import org.ucombinator.jade.jvm.classfile.descriptor.Descriptor._


object DescriptorParser extends JavaTokenParsersOpt {
  val parseFieldDescriptor: CharSequence => ParseResult[FieldDescriptor] = parseAll(fieldDescriptor, _)
  val parseMethodDescriptor: CharSequence => ParseResult[MethodDescriptor] = parseAll(methodDescriptor, _)


  val parseArrayDescriptor: CharSequence => ParseResult[ArrayType] = parseAll(arrayType, _)

//  private lazy val identifier: Parser[Identifier] = super.identifier

  /** Field Descriptor */
  private lazy val fieldDescriptor: Parser[FieldDescriptor] =
    fieldType

  private lazy val fieldType: Parser[FieldType] =
    baseType | objectType | arrayType

  private lazy val baseType: Parser[BaseType] =
    ("B" | "C" | "D" | "F" | "I" | "J" | "S" | "Z") ^^
      BaseType.valueOf

  private lazy val objectType: Parser[ObjectType] =
    "L" ~> packageSpecifier ~ identifier <~ ";" ^^ ObjectType

  private lazy val packageSpecifier: Parser[List[JavaIdentifier]] =
    rep(identifier <~ "/")

  private lazy val arrayType: Parser[ArrayType] =
    "[" ~> componentType ^^ ArrayType

  private lazy val componentType: Parser[FieldType] =
    fieldType

  /** Method Descriptor */
  private lazy val methodDescriptor: Parser[MethodDescriptor] =  // TODO: Simplify this part!!!
    "(" ~ ")" ~ returnDescriptor ^^ {
      case "(" ~ ")" ~ v => MethodDescriptor(Nil, v)
    } |
     rep1bra("(", parameterDescriptor, ")") ~ returnDescriptor ^^ MethodDescriptor

  private lazy val parameterDescriptor: Parser[ParameterDescriptor] =
    fieldType

  private lazy val returnDescriptor: Parser[ReturnDescriptor] =
    fieldType | voidDescriptor

  private lazy val voidDescriptor: Parser[ReturnDescriptor] =
    "V" ^^^ VoidDescriptor

}
