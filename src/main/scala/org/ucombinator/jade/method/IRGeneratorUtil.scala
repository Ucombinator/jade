package org.ucombinator.jade.method

import org.objectweb.asm.{Opcodes, Type}
import org.objectweb.asm.tree._
import org.objectweb.asm.tree.analysis.Frame
import org.ucombinator.jade.jvm.classfile.TypeCommons._
import org.ucombinator.jade.jvm.classfile.descriptor.DescriptorParser
import org.ucombinator.jade.ir._

import scala.collection.mutable.ListBuffer


abstract class IRGenerator {
  // TODO: Should finally be `val code = new ListBuffer[Statement]`
  val code = new ListBuffer[String]
  val localVariableMap: Map[Identifier, Value]
}


object IRGeneratorUtil extends FrameOperations {
  val primitiveArrayElementType = Map(
    4  -> Z, // "boolean"
    5  -> C, // "char"
    6  -> F, // "float"
    7  -> D, // "double"
    8  -> B, // "byte"
    9  -> S, // "short"
    10 -> I, // "int"
    11 -> J, // "long"
  )

  val constantInsnToConst: Map[Int, Value] =
    Map(
      Opcodes.ACONST_NULL -> NullV,
      Opcodes.ICONST_M1   -> IV(-1),
      Opcodes.ICONST_0    -> IV(0),
      Opcodes.ICONST_1    -> IV(1),
      Opcodes.ICONST_2    -> IV(2),
      Opcodes.ICONST_3    -> IV(3),
      Opcodes.ICONST_4    -> IV(4),
      Opcodes.ICONST_5    -> IV(5),
      Opcodes.LCONST_0    -> JV(0L),
      Opcodes.LCONST_1    -> JV(1L),
      Opcodes.FCONST_0    -> FV(0.0F),
      Opcodes.FCONST_1    -> FV(1.0F),
      Opcodes.FCONST_2    -> FV(2.0F),
      Opcodes.DCONST_0    -> DV(0.0),
      Opcodes.DCONST_1    -> DV(1.0)
    )


  final def getConstVal(insn: IntInsnNode): Value =
    (insn.getOpcode: @unchecked) match {
      case Opcodes.BIPUSH => BV(insn.operand.toByte)
      case Opcodes.SIPUSH => SV(insn.operand.toShort)
    }

  final def getConstVal(insn: LdcInsnNode): Value =
    (insn.cst: @unchecked) match {
      case i: java.lang.Integer   => IV(i)
      case f: java.lang.Float     => FV(f)
      case j: java.lang.Long      => JV(j)
      case d: java.lang.Double    => DV(d)
      case str: java.lang.String  => StringLiteralV(str)
      case t: Type /* ASM) */     => Class(DescriptorParser.parseReferenceDescriptor(t.getDescriptor).get)
      // TODO: https://stackoverflow.com/questions/28264012/im-curious-about-what-ldc-short-for-in-jvm/28268671
      // `ldc` can load `java.lang.invoke.MethodType` and `java.lang.invoke.MethodHandle`, but this is NOT for
      // Java code.
      // TODO(continue): If this changes, the actual type of `cst` will
      // TODO(continue): also change -- it might be Methodtype or MethodHandle in the future.
      // TODO: https://stackoverflow.com/questions/28264012/im-curious-about-what-ldc-short-for-in-jvm/28268671
      // `ldc` can load `java.lang.invoke.MethodType` and `java.lang.invoke.MethodHandle`, but this is NOT for
      // Java code.
    }

  final def getInvokeValue(instruction: MethodInsnNode,
                           frame: Frame[Identifier],
                           stackMap: Map[Identifier, Value] = Map.empty[Identifier, Value],
                           localVariableMap: Map[Identifier, Value]): Value = {
    val parameterCount = nParameters(instruction.desc)
    val parameters = topNStackValues(frame, parameterCount, stackMap, localVariableMap).reverse
    val opcode = instruction.getOpcode

    opcode match {
      case Opcodes.INVOKEVIRTUAL | Opcodes.INVOKESPECIAL |
           Opcodes.INVOKEINTERFACE
      =>
        val obj = nthStackValue(frame, parameterCount, stackMap, localVariableMap)
        InvokeValue.of(opcode)(obj, instruction.name, parameters)

      case Opcodes.INVOKESTATIC
      =>
        InvokeValue.of(opcode)(ClassV(instruction.owner), instruction.name, parameters)
    }
  }

  // Example: val desc = "(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;"
  final def nParameters(desc: String): Int =
    DescriptorParser.parseMethodDescriptor(desc).get.parameterDescriptors.length
}
