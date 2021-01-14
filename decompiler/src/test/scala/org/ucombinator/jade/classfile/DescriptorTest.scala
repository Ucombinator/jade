package org.ucombinator.jade.classfile

import com.github.javaparser.ast.`type`.PrimitiveType
import org.scalatest.freespec.AnyFreeSpec

class DescriptorTest extends AnyFreeSpec {
  "fieldDescriptor" - {
    "BaseType" in {
      val types = List(
        PrimitiveType.Primitive.BOOLEAN -> "Z",
        PrimitiveType.Primitive.CHAR -> "C",
        PrimitiveType.Primitive.BYTE -> "B",
        PrimitiveType.Primitive.SHORT -> "S",
        PrimitiveType.Primitive.INT -> "I",
        PrimitiveType.Primitive.LONG -> "J",
        PrimitiveType.Primitive.FLOAT -> "F",
        PrimitiveType.Primitive.DOUBLE -> "D"
      )
      for ((p, s) <- types) {
        assertResult(p) { Descriptor.fieldDescriptor(s).asPrimitiveType.getType }
      }
    }
    "ObjectType" in {
      assertResult("java.lang.Object") {
        Descriptor.fieldDescriptor("Ljava/lang/Object;").asClassOrInterfaceType().toString
      }
    }
    "ArrayType" in {
      assertResult("boolean[][]") {
        Descriptor.fieldDescriptor("[[Z").asArrayType().toString
      }
      assertResult("java.lang.Object[][]") {
        Descriptor.fieldDescriptor("[[Ljava/lang/Object;").asArrayType().toString
      }
    }
  }
  "methodDescriptor" - {
    "Non-void" in {
      val (params, result) = Descriptor.methodDescriptor("(ZJ[[Ljava/lang/Object;)[[Ljava/lang/Object;")
      assertResult(3) { params.length }
      assertResult(PrimitiveType.Primitive.BOOLEAN) { params(0).asPrimitiveType().getType }
      assertResult(PrimitiveType.Primitive.LONG) { params(1).asPrimitiveType().getType }
      assertResult("java.lang.Object[][]") { params(2).asArrayType().toString }
      assertResult("java.lang.Object[][]") { result.asArrayType().toString }
    }
    "Void" in {
      val (params, result) = Descriptor.methodDescriptor("()V")
      assert(params.isEmpty)
      assert(result.isVoidType)
    }
  }
  "className" - {
    "is correct" in {
      assertResult("abc.def.Ghi") { Descriptor.className("abc/def/Ghi").toString }
    }
  }
  "classNameType" - {
    "is correct on a String" in {
      assertResult("abc.def.Ghi") { Descriptor.classNameType("abc/def/Ghi").toString }
    }
    "is correct on a Name" in {
      assertResult("abc.def.Ghi") { Descriptor.classNameType(Descriptor.className("abc/def/Ghi")).toString }
    }
  }
}
