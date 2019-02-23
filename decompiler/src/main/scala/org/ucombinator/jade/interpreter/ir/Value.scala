package org.ucombinator.jade.interpreter.ir

import org.objectweb.asm.tree.analysis.{BasicValue => AsmBasicValue, Value => AsmValue}
import org.ucombinator.jade.classfile.descriptor.Descriptor.FieldDescriptor


/** Value */
abstract class Value

case class Identifier(id: Int, copyVersion: Int, basicValue: AsmBasicValue)
  extends Value with AsmValue {
  override def getSize: Int = basicValue.getSize
}

/** Used by the opcodes of Loads and Stores - Array Elements */
case class ArrayElementV(array: Value, dimension: Value) extends Value with ArrayOperationT {
  require(isArray(array) || isInt(dimension))
}

/** arraylength */
case class ArrayLengthV(array: Value) extends Value with ArrayOperationT with IntT {
  require(isArray(array))
}

case class CheckCastV(v: Value, typ: FieldDescriptor) extends Value

case class InstanceOfV(v: Value, typ: FieldDescriptor) extends Value with BooleanT {
  require(isReference(v))
}
