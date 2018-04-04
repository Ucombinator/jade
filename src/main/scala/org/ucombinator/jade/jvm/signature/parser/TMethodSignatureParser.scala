package org.ucombinator.jade.jvm.signature.parser

import org.ucombinator.jade.jvm.signature.Signature._


trait TMethodSignatureParser extends TJavaTypeSignatureParser with TTypeParameterParser {

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
}

