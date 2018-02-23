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
// TODO: Update to the se 9 version of specification
// Ref: https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.4.1
// "An array type descriptor is valid only if it represents 255 or fewer dimensions.")
case class  `[`(arr: FieldDescriptor) extends FieldDescriptor // reference - one array dimension
