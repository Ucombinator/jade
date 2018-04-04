package org.ucombinator.jade.jvm.signature.parser

import org.ucombinator.jade.jvm.signature.Signature._

import scala.util.parsing.combinator._


trait TJavaTypeSignatureParser extends JavaTokenParsers {

  // TODO: shouldn't this be any string not containing: . ; [ / < > :
  protected val identifier: Parser[String] = ident

  def javaTypeSignature: Parser[JavaTypeSignature] =
    baseType | referenceTypeSignature

  protected def baseType: Parser[JavaTypeSignature] =
    ("B" | "C" | "D" | "F" | "I" | "J" | "S" | "Z") ^^
      BaseType.fromString

  protected def referenceTypeSignature: Parser[ReferenceTypeSignature] =
    classTypeSignature | typeVariableSignature | arrayTypeSignature

  protected def classTypeSignature: Parser[ClassTypeSignature] =
    "L" ~> packageSpecifier ~ rep1sep(simpleClassTypeSignature, ".") <~ ";" ^^ {
      case pkgs ~ scts => ClassTypeSignature(pkgs, scts)
    }

  protected def packageSpecifier: Parser[PackageSpecifier] =
    rep(identifier <~ "/")

  protected def simpleClassTypeSignature: Parser[SimpleClassTypeSignature] =
    identifier ~ opt(typeArguments) ^^ {
      case id ~ o => SimpleClassTypeSignature(id, o.getOrElse(Nil))
    }

  protected def typeArguments: Parser[TypeArguments] =
    "<" ~> rep1(typeArgument) <~ ">"

  protected def typeArgument: Parser[TypeArgument] = (
    "*" ^^^ UnBoundedTypeArgument

  | opt(wildcardIndicator) ~ referenceTypeSignature ^^ {
      case wi ~ rts => BoundedTypeArgument(wi, rts)
    }
  )

  protected def wildcardIndicator: Parser[WildcardIndicator] =
    ("+" | "-") ^^ {
      case "+" => Extends
      case "-" => Super
    }

  protected def typeVariableSignature: Parser[TypeVariableSignature] =
    "T" ~> identifier <~ ";" ^^
      TypeVariableSignature

  protected def arrayTypeSignature: Parser[ArrayTypeSignature] =
    "[" ~> javaTypeSignature ^^
      ArrayTypeSignature

}
