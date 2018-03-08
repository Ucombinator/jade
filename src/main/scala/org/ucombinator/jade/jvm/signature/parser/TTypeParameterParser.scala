package org.ucombinator.jade.jvm.signature.parser

import org.ucombinator.jade.jvm.signature.Signature._


trait TTypeParameterParser extends TJavaTypeSignatureParser {

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

}
