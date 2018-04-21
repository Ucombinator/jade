package org.ucombinator.jade.method.bytecode

import org.ucombinator.jade.method.Val


trait Cast extends Val
case class IntToLong(v: Val) extends Cast      // 133 (0x85) i2l
case class IntToFloat(v: Val) extends Cast     // 134 (0x86) i2f
case class IntToDouble(v: Val) extends Cast    // 135 (0x87) i2d
case class LongToInt(v: Val) extends Cast      // 136 (0x88) l2i
case class LongToFloat(v: Val) extends Cast    // 137 (0x89) l2f
case class LongToDouble(v: Val) extends Cast   // 138 (0x8a) l2d
case class FloatToInt(v: Val) extends Cast     // 139 (0x8b) f2i
case class FloatToLong(v: Val) extends Cast    // 140 (0x8c) f2l
case class FloatToDouble(v: Val) extends Cast  // 141 (0x8d) f2d
case class DoubleToInt(v: Val) extends Cast    // 142 (0x8e) d2i
case class DoubleToLong(v: Val) extends Cast   // 143 (0x8f) d2l
case class DoubleToFloat(v: Val) extends Cast  // 144 (0x90) d2f
case class IntToByte(v: Val) extends Cast      // 145 (0x91) i2b
case class IntToChar(v: Val) extends Cast      // 146 (0x92) i2c
case class IntToShort(v: Val) extends Cast     // 147 (0x93) i2s


