package org.ucombinator.jade.ir


trait Tag

trait PrimitiveT         extends Tag

trait ReferenceT         extends Tag
trait NonArrayReferenceT extends ReferenceT
trait ArrayReferenceT    extends ReferenceT

trait ConstantT
trait ByteT              extends ConstantT
trait CharT              extends ConstantT
trait DoubleT            extends ConstantT
trait FloatT             extends ConstantT
trait IntT               extends ConstantT
trait LongT              extends ConstantT
trait ShortT             extends ConstantT
trait BooleanT           extends ConstantT

trait MathT              extends Tag
trait ConversionT        extends Tag
trait ArrayOperationT    extends Tag

