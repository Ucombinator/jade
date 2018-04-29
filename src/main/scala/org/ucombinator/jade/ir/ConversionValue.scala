package org.ucombinator.jade.ir


abstract class ConversionValue extends Value with PrimitiveT with ConversionT

case class I2JV(v: Value) extends ConversionValue with LongT   // 133 (0x85) i2l
case class I2FV(v: Value) extends ConversionValue with FloatT  // 134 (0x86) i2f
case class I2DV(v: Value) extends ConversionValue with DoubleT // 135 (0x87) i2d
case class L2IV(v: Value) extends ConversionValue with IntT    // 136 (0x88) l2i
case class L2FV(v: Value) extends ConversionValue with FloatT  // 137 (0x89) l2f
case class L2DV(v: Value) extends ConversionValue with DoubleT // 138 (0x8a) l2d
case class F2IV(v: Value) extends ConversionValue with IntT    // 139 (0x8b) f2i
case class F2LV(v: Value) extends ConversionValue with LongT   // 140 (0x8c) f2l
case class F2DV(v: Value) extends ConversionValue with DoubleT // 141 (0x8d) f2d
case class D2IV(v: Value) extends ConversionValue with IntT    // 142 (0x8e) d2i
case class D2LV(v: Value) extends ConversionValue with LongT   // 143 (0x8f) d2l
case class D2FV(v: Value) extends ConversionValue with FloatT  // 144 (0x90) d2f
case class I2BV(v: Value) extends ConversionValue with ByteT   // 145 (0x91) i2b
case class I2CV(v: Value) extends ConversionValue with CharT   // 146 (0x92) i2c
case class I2SV(v: Value) extends ConversionValue with ShortT  // 147 (0x93) i2s


