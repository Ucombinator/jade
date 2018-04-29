package org.ucombinator.jade.ir

import org.objectweb.asm.tree.analysis.{BasicValue => AsmBasicValue, Value => AsmValue}
import org.ucombinator.jade.jvm.classfile.descriptor.Descriptor.FieldDescriptor
import org.ucombinator.jade.method._


case class Identifier(id: Int, copyVersion: Int, basicValue: AsmBasicValue)
  extends Value with AsmValue with Variable {
  override def getSize: Int = basicValue.getSize
}

/** Used by the opcodes of Loads and Stores - Array Elements */
case class ArrayElementV(array: Value, dimension: Value) extends Value with ArrayOperationT {
  require(isArray(array))
  require(isInt(dimension))
}


/** arraylength */
case class ArrayLengthV(array: Value) extends Value with ArrayOperationT with IntT {
  require(isArray(array))
}

case class CheckCastV(v: Value, typ: FieldDescriptor) extends Value

case class InstanceOfV(v: Value, typ: FieldDescriptor) extends Value with BooleanT {
  require(isReference(v))
  require(isReference(typ))
}
