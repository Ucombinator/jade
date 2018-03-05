package org.ucombinator.jade.jvm.signature.parser

import org.ucombinator.jade.jvm.signature.Signature._


trait TClassSignatureParser extends TJavaTypeSignatureParser with TTypeParameterParser {

  final def classSignature: Parser[ClassSignature] =
    opt(typeParameters) ~ superclassSignature ~ rep(superinterfaceSignature) ^^ {
      case tps ~ scs ~ sifss => ClassSignature(tps, scs, sifss)
    }

  final protected def superclassSignature: Parser[ClassTypeSignature] =
    classTypeSignature

  final protected def superinterfaceSignature: Parser[ClassTypeSignature] =
    classTypeSignature

}

