package org.ucombinator.jade.jvm.signature.parser

import org.scalatest.FunSuite
import org.ucombinator.jade.jvm.classfile.attribute.signature._

class ParserTest extends FunSuite {
  test("A class signature after parsing should be equal to the result we manually construct") {
    val classSignature = "<K:Ljava/lang/Object;V:Ljava/lang/Object;>Ljava/util/AbstractMap<TK;TV;>;Ljava/util/Map<TK;TV;>;Ljava/lang/Cloneable;Ljava/io/Serializable;"
    val expectedResult = ClassSignature(
      List(
        TypeParameter("K",
          Some(ClassTypeSignature(List("java", "lang"), List(SimpleClassTypeSignature("Object", Nil)))),
          Nil),
        TypeParameter("V",
          Some(ClassTypeSignature(List("java", "lang"), List(SimpleClassTypeSignature("Object", Nil)))),
          Nil)),
      ClassTypeSignature(
        List("java", "util"),
        List(SimpleClassTypeSignature("AbstractMap",
          List(BoundedTypeArgument(None, TypeVariableSignature("K")),
               BoundedTypeArgument(None, TypeVariableSignature("V")))))),
      List(
        ClassTypeSignature(List("java", "util"),
          List(SimpleClassTypeSignature("Map",
            List(BoundedTypeArgument(None, TypeVariableSignature("K")),
                 BoundedTypeArgument(None, TypeVariableSignature("V")))))),
        ClassTypeSignature(List("java", "lang"),
          List(SimpleClassTypeSignature("Cloneable", Nil))),
        ClassTypeSignature(List("java", "io"),
          List(SimpleClassTypeSignature("Serializable", Nil)))))
    val actualResult = SignatureParser.parseClassSignature(classSignature)
    assert(actualResult.successful)
    assertResult(expectedResult)(actualResult.get)
  }
}
