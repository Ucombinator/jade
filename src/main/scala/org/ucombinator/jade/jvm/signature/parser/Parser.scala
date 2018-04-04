package org.ucombinator.jade.jvm.signature.parser

import org.ucombinator.jade.jvm.signature.Signature._

import scala.util.parsing.combinator.JavaTokenParsers

// TODO: Modify the traits to guarantee LL(1)
object Parser
  extends JavaTokenParsers {

  def parseJavaTypeSignature(signature: String): JavaTypeSignature = {
    println(s"input (${signature.length}): $signature")
    parseAll(javaTypeSignature, signature).get
  }

  def parseClassSignature(signature: String): ClassSignature = {
    println(s"input (${signature.length}): $signature")
    parseAll(classSignature, signature).get
  }

  def parseMethodSignature(signature: String): MethodSignature  = {
    println(s"input (${signature.length}): $signature")
    parseAll(methodSignature, signature).get
  }

  /////

  // Class signature parser
  final def classSignature: Parser[ClassSignature] =
    opt(typeParameters) ~ superclassSignature ~ rep(superinterfaceSignature) ^^ {
      case tps ~ scs ~ sifss => ClassSignature(tps.getOrElse(Nil), scs, sifss)
    }

  final protected def superclassSignature: Parser[ClassTypeSignature] =
    classTypeSignature

  final protected def superinterfaceSignature: Parser[ClassTypeSignature] =
    classTypeSignature

  // Method signature parser
  final def methodSignature: Parser[MethodSignature] =
    opt(typeParameters) ~ "(" ~ rep(javaTypeSignature) ~ ")" ~ result ~ rep(throwsSignature) ^^ {
      case tps ~ "(" ~ jtss ~ ")" ~ r ~ tss => MethodSignature(tps, jtss, r, tss)
    }

  final protected def result: Parser[Result] =
    javaTypeSignature | voidDescriptor

  final protected def throwsSignature: Parser[ThrowsSignature] =
    "^" ~> (classTypeSignature | typeVariableSignature)

  final protected val voidDescriptor: Parser[Result] =
    "V" ^^^
      VoidDescriptor

  // Type parameter parser
  final protected def typeParameters: Parser[TypeParameters] =
    "<" ~> rep1(typeParameter) <~ ">"

  final protected def typeParameter: Parser[TypeParameter] =
    identifier ~ classBound ~ rep(interfaceBound) ^^ {
      case name ~ scb ~ sib => TypeParameter(name, scb, sib)
    }

  final protected def classBound: Parser[Option[ReferenceTypeSignature]] =
    ":" ~> opt(referenceTypeSignature)

  final protected def interfaceBound: Parser[ReferenceTypeSignature] =
    ":" ~> referenceTypeSignature

  ////


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

