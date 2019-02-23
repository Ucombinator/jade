package org.ucombinator.jade.interpreter.ir


abstract class MathValue extends Value with MathT with PrimitiveT

case class AddV(augend: Value, addends: Value)          extends MathValue
case class SubV(minuend: Value, subtrahend: Value)      extends MathValue
case class MulV(multiplicand: Value, multiplier: Value) extends MathValue
case class DivV(dividend: Value, divisor: Value)        extends MathValue
case class RemV(dividend: Value, divisor: Value)        extends MathValue
case class NegV(number: Value)                          extends MathValue
case class ShlV(number: Value, nbits: Value)            extends MathValue
case class ShrV(number: Value, nbits: Value)            extends MathValue
case class UshrV(number: Value, nbits: Value)           extends MathValue
case class AndV(cond1: Value, cond2: Value)             extends MathValue
case class OrV(cond1: Value, cond2: Value)              extends MathValue
case class XorV(cond1: Value, cond2: Value)             extends MathValue
case class IincV(number: Value, inc: Value)             extends MathValue
