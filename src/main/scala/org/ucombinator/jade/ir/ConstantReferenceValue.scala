package org.ucombinator.jade.ir

import org.ucombinator.jade.jvm.classfile.descriptor.Descriptor.ObjectType


abstract class ConstantReferenceValue(v: Any) extends Value with ReferenceT with ConstantT {
  override val toString: String =
    v match {
      case null => "null"
      case _    => v.toString
    }
}

case object NullV extends ConstantReferenceValue(null)

abstract class ConstantNonArrayReferenceValue(v: Any)
  extends ConstantReferenceValue(v) with NonArrayReferenceT

case class StringLiteralV(v: String) extends ConstantNonArrayReferenceValue(v)
case class ClassV(t: ObjectType)     extends ConstantNonArrayReferenceValue(t)


// NO use for now.
// I think this design can be used to simplify Java auto-boxing and auto-unboxing
abstract class BoxedValue(v: Any) extends ConstantNonArrayReferenceValue(v) with NonArrayReferenceT

case class BoxedBV(v: Byte)   extends BoxedValue(v) with ByteT
case class BoxedCV(v: Char)   extends BoxedValue(v) with CharT
case class BoxedDV(v: Double) extends BoxedValue(v) with DoubleT
case class BoxedFV(v: Float)  extends BoxedValue(v) with FloatT
case class BoxedIV(v: Int)    extends BoxedValue(v) with IntT
case class BoxedJV(v: Long)   extends BoxedValue(v) with LongT
case class BoxedSV(v: Short)  extends BoxedValue(v) with ShortT
case class BoxedZV(v: Short)  extends BoxedValue(v) with BooleanT


