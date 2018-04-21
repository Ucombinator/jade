package org.ucombinator.jade.method.bytecode

import org.ucombinator.jade.method.Val

trait MathByteCode extends Val

object MathByteCode {
  case class ADD(augend: Val, addends: Val) extends MathByteCode
  case class SUB(minuend: Val, subtrahend: Val) extends MathByteCode
  case class MUL(multiplicand: Val, multiplier: Val) extends MathByteCode
  case class DIV(dividend: Val, divisor: Val) extends MathByteCode
  case class REM(dividend: Val, divisor: Val) extends MathByteCode
  case class NEG(number: Val) extends MathByteCode
  case class SHL(number: Val, nbits: Val) extends MathByteCode
  case class SHR(number: Val, nbits: Val) extends MathByteCode
  case class USHR(number: Val, nbits: Val) extends MathByteCode
  case class AND(cond1: Val, cond2: Val) extends MathByteCode
  case class OR(cond1: Val, cond2: Val) extends MathByteCode
  case class XOR(cond1: Val, cond2: Val) extends MathByteCode
  case class IINC(number: Val, inc: Val) extends MathByteCode
}
