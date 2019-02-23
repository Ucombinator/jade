package org.ucombinator.jade.interpreter.ir

import org.ucombinator.jade.classfile.TypeCommons.BaseType
import org.ucombinator.jade.classfile.descriptor.Descriptor.{FieldDescriptor, ObjectType}


abstract class ArrayValue[T <: FieldDescriptor, D](typ: T, dimension: D)
    extends Value with ArrayReferenceT

abstract class OneDimensionArrayValue[T <: FieldDescriptor](typ: T, dimension: Value)
    extends ArrayValue[T, Value](typ, dimension) {
  require(isInt(dimension))
  override val toString: String = typ + "[" + dimension + "]"
}

/** newarray */
case class PrimitiveArrayV(typ: BaseType, dimension: Value)
    extends OneDimensionArrayValue[BaseType](typ, dimension)

/** anewarray */
case class ReferenceArrayV(typ: ObjectType, dimension: Value)
    extends OneDimensionArrayValue[ObjectType](typ, dimension)

/** multianewarray */
case class MultiDimensionArrayV(typ: FieldDescriptor, dimensions: List[Value])
    extends ArrayValue[FieldDescriptor, List[Value]](typ, dimensions) {
  require(dimensions.forall(isInt))
  override val toString: String = typ + dimensions.map(l => s"[$l]").mkString
}
