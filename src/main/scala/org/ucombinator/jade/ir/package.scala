package org.ucombinator.jade

import org.ucombinator.jade.jvm.classfile.descriptor.Descriptor.{ArrayType, FieldDescriptor, ObjectType}
import org.objectweb.asm.tree.analysis.{BasicValue => AsmBasicValue}


package object ir {
  /** Value */
  abstract class Value

  /** Tags */
  trait Tag

  trait PrimitiveT extends Tag

  trait ReferenceT         extends Tag
  trait NonArrayReferenceT extends ReferenceT
  trait ArrayReferenceT    extends ReferenceT

  trait ConstantT
  trait ByteT    extends ConstantT
  trait CharT    extends ConstantT
  trait DoubleT  extends ConstantT
  trait FloatT   extends ConstantT
  trait IntT     extends ConstantT
  trait LongT    extends ConstantT
  trait ShortT   extends ConstantT
  trait BooleanT extends ConstantT

  trait MathT              extends Tag
  trait ConversionT        extends Tag
  trait ArrayOperationT    extends Tag


  def isInt(v: Value): Boolean =
    v match {
      case _: IV | Identifier(_, _, AsmBasicValue.INT_VALUE) => true
      case _ => false
    }

  def isReference(v: Value): Boolean =
    v match {
      case Identifier(_, _, AsmBasicValue.REFERENCE_VALUE) => true
      case _: ArrayReferenceT => true
      case _ => false
    }

  def isReference(typ: FieldDescriptor): Boolean =
    typ.isInstanceOf[ObjectType] || typ.isInstanceOf[ArrayType]

  // TODO:
  // This one is not perfect:
  //   For `Identifier` we can find out if it is a REFERENCE_VALUE, but we can
  // distinguish "Array Reference" and "Non-Array Reference"! This is the
  // limitation of the org.objectweb.asm.tree.analysis.BasicValue
  def isArray(v: Value): Boolean = isReference(v)
}
