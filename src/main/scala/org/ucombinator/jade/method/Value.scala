package org.ucombinator.jade.method

import org.objectweb.asm.tree.analysis.{BasicValue => AsmBasicValue, Value => AsmValue}
import org.ucombinator.jade.Variable
import org.ucombinator.jade.jvm.classfile.TypeCommons.BaseType
import org.ucombinator.jade.jvm.classfile.descriptor.Descriptor.{ArrayType, FieldDescriptor, ObjectType}


/** Value */
trait Value

case class Identifier(id: Int, copyVersion: Int, basicValue: AsmBasicValue)
  extends AsmValue with Value with Variable {
  override def getSize: Int = basicValue.getSize
}



// Seems NO NEED to create `CV` for `char` and `ZV` or `boolean`, but I still create them ...
// If they are really useless, delete them later!
abstract class PrimitiveValue(v: Any) extends Value {
  override val toString = v.toString
}

case class BV(v: Byte) extends PrimitiveValue(v) with ByteT      // Opcodes.BIPUSH
case class CV(v: Char) extends PrimitiveValue(v) with CharT      // No specific opcode(s)
case class DV(v: Double) extends PrimitiveValue(v) with DoubleT  // LDC2_W -- not inside the `Opcodes` class
case class FV(v: Float) extends PrimitiveValue(v) with FloatT    // Opcodes.LDC
case class IV(v: Int) extends PrimitiveValue(v) with IntT        // Opcodes.LDC
case class JV(v: Long) extends PrimitiveValue(v) with LongT      // LDC2_W -- not inside the `Opcodes` class
case class SV(v: Short) extends PrimitiveValue(v) with ShortT    // Opcodes.SIPUSH
case class ZV(v: Short) extends PrimitiveValue(v) with BooleanT    // No specific opcode(s)


abstract class ReferenceValue extends Value

abstract class NonArrayReferenceValue extends ReferenceValue

abstract class BoxedValue(v: Any) extends NonArrayReferenceValue {
  override val toString = v.toString
}

case class BoxedBV(v: Byte) extends BoxedValue(v) with ByteT
case class BoxedCV(v: Char) extends BoxedValue(v) with CharT
case class BoxedDV(v: Double) extends BoxedValue(v) with DoubleT
case class BoxedFV(v: Float) extends BoxedValue(v) with FloatT
case class BoxedIV(v: Int) extends BoxedValue(v) with IntT
case class BoxedJV(v: Long) extends BoxedValue(v) with LongT
case class BoxedSV(v: Short) extends BoxedValue(v) with ShortT
case class BoxedZV(v: Short) extends BoxedValue(v) with BooleanT

case object NullV extends ReferenceValue


abstract class ArrayReferenceValue extends ReferenceValue

abstract class NewArrayValue[T <: FieldDescriptor](typ: T, dimension: Value) extends ArrayReferenceValue {
  require(Prerequisite.isInt(dimension))
  override val toString = typ + "[" + dimension + "]"
}

case class NewPrimitiveArrayV(typ: BaseType, dimension: Value) extends NewArrayValue[BaseType](typ, dimension)
case class NewReferenceArrayV(typ: ObjectType, dimension: Value) extends NewArrayValue[ObjectType](typ, dimension)
case class NewMultiDimArray(typ: FieldDescriptor, dimensions: List[Value]) extends ArrayReferenceValue {
  require(dimensions.forall(Prerequisite.isInt))
  override val toString = typ + dimensions.map(l => s"[$l]").mkString
}



case class ArrayElementV(array: Value, dimension: Value) extends Value with ArrayOperationT {
  require(Prerequisite.isArray(array))
  require(Prerequisite.isInt(dimension))
}

case class ArrayLengthV(array: Value) extends Value with ArrayOperationT with IntT {
  require(Prerequisite.isArray(array))
}

case class InstanceOf(v: Value, typ: FieldDescriptor) extends Value with BooleanT {
  require(Prerequisite.isReference(v))
  require(Prerequisite.isReference(typ))
}


object Prerequisite {
  def isInt(v: Value): Boolean =
    v match {
      case (_: IV) | Identifier(_, _, AsmBasicValue.INT_VALUE) => true
      case _ => false
    }

  def isReference(v: Value): Boolean =
    v match {
      case Identifier(_, _, AsmBasicValue.REFERENCE_VALUE) => true
      case _: ArrayReferenceValue => true
      case _ => false
    }

  def isReference(typ: FieldDescriptor): Boolean =
    typ.isInstanceOf[ObjectType] || typ.isInstanceOf[ArrayType]

  // This one is not perfect:
  //   For `Identifier` we can find out if it is a REFERENCE_VALUE, but we can
  // distinguish "Array Reference" and "Non-Array Reference"! This is the
  // limitation of the org.objectweb.asm.tree.analysis.BasicValue
  def isArray(v: Value): Boolean = isReference(v)
}


case class LDCVal(v: Any) extends ReferenceValue  // TODO: can be improved
case class CheckCastV(v: Value, typ: FieldDescriptor) extends Value

/** Invoke */
abstract class InvokeValue[T <: Value](obj: T, method: String, parameters: List[Value])
    extends Value {
  require(Prerequisite.isReference(obj))
}

case class InvokeVirtualV(instance: Value, method: String, parameters: List[Value])
  extends InvokeValue(instance, method, parameters)

case class InvokeSpecialV(instance: Value, method: String, parameters: List[Value])
  extends InvokeValue(instance, method, parameters)

case class InvokeStaticV(cls: ClassV, method: String, parameters: List[Value])
  extends InvokeValue(cls, method, parameters)

// TODO: Add a field for interface info for future type inference
case class InvokeInterfaceV(instance: Value, method: String, parameters: List[Value])
  extends  InvokeValue(instance, method, parameters)

// TODO: `case class InvokeDynamicV`


/** Field */
abstract class FieldValue[T](obj: T, field: Any, desc: FieldDescriptor)
  extends Value

case class InstanceFieldV(obj: Value, field: Any, desc: FieldDescriptor)
  extends FieldValue[Value](obj, field, desc)  // TODO: obj should be ObjectVal, but it can also be an Identifier.  `require`

case class StaticFieldV(cls: String, field: String, desc: FieldDescriptor)
  extends FieldValue[String](cls, field, desc)


case class ClassV(name: String) extends ReferenceValue  // TODO: ???
//case class FieldV(name: String) extends Value  // TODO: NameT ???
//case class InstanceV(name: String) extends Value  // TODO: ???


//case class EqualTest(lhs: Value, rhs: Value) extends Value
//case class SsaPHI(v1: Value, v2: Value) extends Value









//trait Value
//
//case class Identifier(id: Int, copyVersion: Int, basicValue: AsmBasicValue)
//  extends AsmValue with Value with Variable {
//  override def getSize: Int = basicValue.getSize
//}
//
//
//case class NameV(name: String) extends Value
//
//// B -> `byte`
//// C -> `char`
//// D -> `double`
//// F -> `float`
//// I -> `int`
//// J -> `long`
//// S -> `short`
//// Z -> `boolean`
//
//// Seems NO NEED to create `CV` for `char` and `ZV` or `boolean`
//trait PrimitiveValue extends Value
//case class BV(v: Byte) extends PrimitiveValue    // Opcodes.BIPUSH
//case class SV(v: Short) extends PrimitiveValue   // Opcodes.SIPUSH
//case class IV(v: Int) extends PrimitiveValue     // Opcodes.LDC
//case class JV(v: Long) extends PrimitiveValue    // LDC2_W -- not inside the `Opcodes` class
//case class FV(v: Float) extends PrimitiveValue   // Opcodes.LDC
//case class DV(v: Double) extends PrimitiveValue  // LDC2_W -- not inside the `Opcodes` class
//
//trait ReferenceValue extends Value
//case object NullV extends ReferenceValue
//case class NewArrayV(typ: String, dim: Value) extends ReferenceValue  // TODO: String -> Val
//
//
//case class ArrayElementV(typ: Value, dim: Value) extends Value {
//
//}
//
//case class ArrayLength(v: Value) extends Value
//case class InstanceOf(v: Value, typ: FieldDescriptor) extends Value
//
//
//case class NewObjectArray(typ: FieldDescriptor, length: Value) extends Value
//case class NewMultiDimArray(typ: Value, lengths: List[Value]) extends Value
//
//
//trait ObjectVal extends Value
//case class ObjVal(v: Value) extends ObjectVal
//case class LDCVal(v: Any) extends ObjectVal
//case class CheckCastVal(v: Value, typ: FieldDescriptor) extends ObjectVal
//case class InvokeVirtualVal(instance: Value, method: Any, parameters: List[Value]) extends ObjectVal
//case class InvokeSpecialVal(instance: Value, method: Any, parameters: List[Value]) extends ObjectVal
//case class InvokeStaticVal(instance: Value, method: Any, parameters: List[Value]) extends ObjectVal
//case class InvokeInterfaceVal(instance: Value, method: Any, parameters: List[Value]) extends ObjectVal
//
//trait FieldVal extends Value
//case class InstanceFieldVal(obj: Value, field: Any, desc: String) extends FieldVal  // obj should be ObjectVal, but it can also be an Identifier.
//case class StaticFieldVal(obj: Value, field: Any, desc: String) extends FieldVal
