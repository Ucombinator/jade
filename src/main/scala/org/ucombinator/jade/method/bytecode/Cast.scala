package org.ucombinator.jade.method.bytecode

import org.ucombinator.jade.method.Value


trait Cast extends Value
case class IntToLong(v: Value) extends Cast      // 133 (0x85) i2l
case class IntToFloat(v: Value) extends Cast     // 134 (0x86) i2f
case class IntToDouble(v: Value) extends Cast    // 135 (0x87) i2d
case class LongToInt(v: Value) extends Cast      // 136 (0x88) l2i
case class LongToFloat(v: Value) extends Cast    // 137 (0x89) l2f
case class LongToDouble(v: Value) extends Cast   // 138 (0x8a) l2d
case class FloatToInt(v: Value) extends Cast     // 139 (0x8b) f2i
case class FloatToLong(v: Value) extends Cast    // 140 (0x8c) f2l
case class FloatToDouble(v: Value) extends Cast  // 141 (0x8d) f2d
case class DoubleToInt(v: Value) extends Cast    // 142 (0x8e) d2i
case class DoubleToLong(v: Value) extends Cast   // 143 (0x8f) d2l
case class DoubleToFloat(v: Value) extends Cast  // 144 (0x90) d2f
case class IntToByte(v: Value) extends Cast      // 145 (0x91) i2b
case class IntToChar(v: Value) extends Cast      // 146 (0x92) i2c
case class IntToShort(v: Value) extends Cast     // 147 (0x93) i2s


