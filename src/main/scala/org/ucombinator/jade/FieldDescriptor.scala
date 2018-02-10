package org.ucombinator.jade

// jvms9 - page 80
// Table 4.3-A. Interpretation of field descriptors
sealed abstract class FieldDescriptor
case object B extends FieldDescriptor  // `byte`   - signed byte
case object C extends FieldDescriptor  // `char`   - Unicode character code point in the Basic Multilingual Plane, encoded with UTF-16
case object D extends FieldDescriptor  // `double` - double-precision floating-point value
case object F extends FieldDescriptor  // `float`  - single-precision floating-point value
case object I extends FieldDescriptor  // `int`    - integer
case object J extends FieldDescriptor  // `long`   - long integer
case class  L(className: String) extends FieldDescriptor  // reference  - reference an instance of class ClassName
case object S extends FieldDescriptor  // `short`   - signed short
case object Z extends FieldDescriptor  // `boolean` - true or false

abstract class ArrayFieldDescriptor extends FieldDescriptor // reference - one array dimension
case object `[B` extends ArrayFieldDescriptor
case object `[C` extends ArrayFieldDescriptor
case object `[D` extends ArrayFieldDescriptor
case object `[F` extends ArrayFieldDescriptor
case object `[I` extends ArrayFieldDescriptor
case object `[J` extends ArrayFieldDescriptor
case class  `[L`(className: String) extends ArrayFieldDescriptor
case object `[S` extends ArrayFieldDescriptor
case object `[Z` extends ArrayFieldDescriptor
case class  `[`(arr: ArrayFieldDescriptor) extends ArrayFieldDescriptor // High dimension array
