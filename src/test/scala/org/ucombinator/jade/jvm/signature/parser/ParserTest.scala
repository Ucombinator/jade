package org.ucombinator.jade.jvm.signature.parser

import org.scalatest.FunSuite

import org.ucombinator.jade.jvm.signature.Signature._


class ParserTest extends FunSuite {
  test("A class signature after parsing should be equal to the result we manually construct") {
    val classSignature = "<K:Ljava/lang/Object;V:Ljava/lang/Object;>Ljava/util/AbstractMap<TK;TV;>;Ljava/util/Map<TK;TV;>;Ljava/lang/Cloneable;Ljava/io/Serializable;"
    val result = ClassSignature(Some(List(TypeParameter("K",Some(ClassTypeSignature(Some(List("java", "lang")),SimpleClassTypeSignature("Object", Nil),Nil)),Nil), TypeParameter("V",Some(ClassTypeSignature(Some(List("java", "lang")),SimpleClassTypeSignature("Object",Nil),Nil)),Nil))),ClassTypeSignature(Some(List("java", "util")),SimpleClassTypeSignature("AbstractMap",List(BoundedTypeArgument(None,TypeVariableSignature("K")), BoundedTypeArgument(None,TypeVariableSignature("V")))),Nil),List(ClassTypeSignature(Some(List("java", "util")),SimpleClassTypeSignature("Map",List(BoundedTypeArgument(None,TypeVariableSignature("K")), BoundedTypeArgument(None,TypeVariableSignature("V")))),Nil), ClassTypeSignature(Some(List("java", "lang")),SimpleClassTypeSignature("Cloneable",Nil),Nil), ClassTypeSignature(Some(List("java", "io")),SimpleClassTypeSignature("Serializable",Nil),Nil)))
    assert(Parser.parseClassSignature(classSignature) == result)
  }

//  test("Invoking head on an empty Set should produce NoSuchElementException") {
//    assertThrows[NoSuchElementException] {
//      Set.empty.head
//    }
//  }
}
