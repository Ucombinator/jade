package org.ucombinator.jade.jvm.signature.parser

import org.ucombinator.jade.jvm.signature.Signature.{ClassSignature, JavaTypeSignature, MethodSignature}

// TODO: Modify the traits to guarantee LL(1)
object Parser
  extends TJavaTypeSignatureParser with TClassSignatureParser with TMethodSignatureParser {

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

}

