package org.ucombinator.jade.interpreter.ir

/** Primitive Constant */
abstract class ConstantPrimitiveValue(v: Any) extends Value with PrimitiveT with ConstantT {
  override val toString: String = v.toString
}

// Seems NO NEED to create `CV` for `char` and `ZV` or `boolean`, but I still create them ...
// If they are really useless, delete them later!
case class BV(v: Byte)   extends ConstantPrimitiveValue(v) with ByteT     // Opcodes.BIPUSH
case class CV(v: Char)   extends ConstantPrimitiveValue(v) with CharT     // No related opcode(s)
case class DV(v: Double) extends ConstantPrimitiveValue(v) with DoubleT   // LDC2_W -- not inside the `Opcodes` class
case class FV(v: Float)  extends ConstantPrimitiveValue(v) with FloatT    // Opcodes.LDC
case class IV(v: Int)    extends ConstantPrimitiveValue(v) with IntT      // Opcodes.LDC
case class JV(v: Long)   extends ConstantPrimitiveValue(v) with LongT     // LDC2_W -- not inside the `Opcodes` class
case class SV(v: Short)  extends ConstantPrimitiveValue(v) with ShortT    // Opcodes.SIPUSH
case class ZV(v: Short)  extends ConstantPrimitiveValue(v) with BooleanT  // No related opcode(s)

