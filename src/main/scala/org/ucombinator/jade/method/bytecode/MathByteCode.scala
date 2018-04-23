package org.ucombinator.jade.method.bytecode

import org.ucombinator.jade.method.Value

trait MathByteCode extends Value

object MathByteCode {
  case class ADD(augend: Value, addends: Value) extends MathByteCode
  case class SUB(minuend: Value, subtrahend: Value) extends MathByteCode
  case class MUL(multiplicand: Value, multiplier: Value) extends MathByteCode
  case class DIV(dividend: Value, divisor: Value) extends MathByteCode
  case class REM(dividend: Value, divisor: Value) extends MathByteCode
  case class NEG(number: Value) extends MathByteCode
  case class SHL(number: Value, nbits: Value) extends MathByteCode
  case class SHR(number: Value, nbits: Value) extends MathByteCode
  case class USHR(number: Value, nbits: Value) extends MathByteCode
  case class AND(cond1: Value, cond2: Value) extends MathByteCode
  case class OR(cond1: Value, cond2: Value) extends MathByteCode
  case class XOR(cond1: Value, cond2: Value) extends MathByteCode
  case class IINC(number: Value, inc: Value) extends MathByteCode
}
