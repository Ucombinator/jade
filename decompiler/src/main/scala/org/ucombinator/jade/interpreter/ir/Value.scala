package org.ucombinator.jade.interpreter.ir

import org.ucombinator.jade.interpreter.descriptor.Descriptor.FieldDescriptor
import org.ucombinator.jade.method.ssa.Var


/** Value */
abstract class Value

case class Identifier(v: Var) extends Value

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
