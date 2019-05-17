package org.ucombinator.jade.interpreter.ir

import org.ucombinator.jade.interpreter.descriptor.Descriptor.FieldDescriptor


/** Field */
abstract class FieldValue[T](obj: T, field: Any, desc: FieldDescriptor)
  extends Value

case class InstanceFieldV(obj: Value, field: Any, desc: FieldDescriptor)
  extends FieldValue[Value](obj, field, desc)

case class StaticFieldV(cls: String, field: String, desc: FieldDescriptor)
  extends FieldValue[String](cls, field, desc)


//case class FieldV(name: String) extends Value  // TODO: NameT ???
//case class InstanceV(name: String) extends Value  // TODO: ???


//case class EqualTest(lhs: Value, rhs: Value) extends Value
//case class SsaPHI(v1: Value, v2: Value) extends Value


