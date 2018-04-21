package org.ucombinator.jade.method

import org.ucombinator.jade.jvm.classfile.descriptor.Descriptor
import org.ucombinator.jade.jvm.classfile.descriptor.Descriptor.FieldDescriptor

trait Val

// Identifiers.scala
// case class Identifier(id: Int, copyVersion: Int, basicValue: BasicValue) extends Value with MathOperand

case class NameVal(name: String) extends Val

//trait PrimitiveVal

case class IntVal(v: Int) extends Val
case class LongVal(v: Long) extends Val
case class FloatVal(v: Float) extends Val
case class DoubleVal(v: Double) extends Val

case class ArrayElementVal(typ: Val, dim: Val) extends Val

case class NewArrayVal(typ: String, dim: Val) extends Val  // TODO: String -> Val
case class ArrayLength(v: Val) extends Val
case class InstanceOf(v: Val, typ: FieldDescriptor) extends Val


case class NewObjectArray(typ: FieldDescriptor, length: Val) extends Val
case class NewMultiDimArray(typ: Val, lengths: List[Val]) extends Val


trait ObjectVal extends Val
case class ObjVal(v: Val) extends ObjectVal
case class LDCVal(v: Any) extends ObjectVal
case class CheckCastVal(v: Val, typ: FieldDescriptor) extends ObjectVal
case class InvokeVirtualVal(instance: Val, method: Any, parameters: List[Val]) extends ObjectVal
case class InvokeSpecialVal(instance: Val, method: Any, parameters: List[Val]) extends ObjectVal
case class InvokeStaticVal(instance: Val, method: Any, parameters: List[Val]) extends ObjectVal
case class InvokeInterfaceVal(instance: Val, method: Any, parameters: List[Val]) extends ObjectVal

trait FieldVal extends Val
case class InstanceFieldVal(obj: Val, field: Any, desc: String) extends FieldVal  // obj should be ObjectVal, but it can also be an Identifier.
case class StaticFieldVal(obj: Val, field: Any, desc: String) extends FieldVal
