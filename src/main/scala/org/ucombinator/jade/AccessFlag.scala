package org.ucombinator.jade

import scala.collection.immutable

sealed trait TAccessFlag {
  val hasKeyword: Boolean
  val keyword: String
  val code: Int
}

trait TClassFlag extends TAccessFlag
trait TNestedClassFlag extends TClassFlag
trait TFieldFlag extends TAccessFlag
trait TMethodFlag extends TAccessFlag
trait TParameterFlag extends TAccessFlag
trait TModuleFlag extends TAccessFlag

sealed abstract class AccessFlag {
}

case object ACC_PUBLIC extends AccessFlag
    with TClassFlag with TFieldFlag with TMethodFlag {
  val hasKeyword = true
  val keyword = "public"
  val code = 0x0001
}

case object ACC_PRIVATE extends AccessFlag
    with TClassFlag with TFieldFlag with TMethodFlag {
  val hasKeyword = true
  val keyword = "private"
  val code = 0x0002
}

case object ACC_PROTECTED extends AccessFlag
    with TClassFlag with TFieldFlag with TMethodFlag {
  val hasKeyword = true
  val keyword = "protected"
  val code = 0x0004
}

case object ACC_STATIC extends AccessFlag
    with TNestedClassFlag with TFieldFlag with TMethodFlag {
  val hasKeyword = true
  val keyword = "static"
  val code = 0x0008
}

case object ACC_FINAL extends AccessFlag
    with TClassFlag with TFieldFlag with TMethodFlag with TParameterFlag {
  val hasKeyword = true
  val keyword = "final"
  val code = 0x0010
}

case object ACC_SUPER extends AccessFlag
    with TClassFlag {
  val hasKeyword = false
  val keyword = ""
  val code = 0x0020
}

case object ACC_SYNCHRONIZED extends AccessFlag
    with TMethodFlag {
  val hasKeyword = true
  val keyword = "synchronized"
  val code = 0x0020
}

//val ACC_OPEN = 0x0020; // module
//val ACC_TRANSITIVE = 0x0020; // module requires

case object ACC_VOLATILE extends AccessFlag
    with TFieldFlag {
  val hasKeyword = true
  val keyword = "volatile"
  val code = 0x0040
}

case object ACC_BRIDGE extends AccessFlag
    with TMethodFlag {
  val hasKeyword = false
  val keyword = "compiler-BRIDGE"
  val code = 0x0040
}

//val ACC_STATIC_PHASE = 0x0040; // module requires

case object ACC_VARARGS extends AccessFlag
    with TMethodFlag {
  val hasKeyword = false
  val keyword = "compiler-VARARGS"
  val code = 0x0080
}

case object ACC_TRANSIENT extends AccessFlag
    with TFieldFlag {
  val hasKeyword = true
  val keyword = "transient"
  val code = 0x0080
}

case object ACC_NATIVE extends AccessFlag
    with TMethodFlag {
  val hasKeyword = true
  val keyword = "native"
  val code = 0x0100
}

case object ACC_INTERFACE extends AccessFlag
    with TClassFlag {
  val hasKeyword = true
  val keyword = "interface" // TODO: ??? From the specification, an interface also need acc_abstract flag??? page 68 JVM 8
  val code = 0x0200
}

case object ACC_ABSTRACT extends AccessFlag
    with TClassFlag with TMethodFlag {
  val hasKeyword = true
  val keyword = "abstract" // TODO: also used to mark `interface`
  val code = 0x0400
}

case object ACC_STRICT extends AccessFlag
    with TMethodFlag {
  val hasKeyword = true
  val keyword = "strictfp"
  val code = 0x0800
  }

// class, field, method, parameter, module *
case object ACC_SYNTHETIC extends AccessFlag
    with TClassFlag with TFieldFlag with TMethodFlag with TParameterFlag with TModuleFlag {
  val hasKeyword = false
  val keyword = "compiler-SYNTHETIC"
  val code = 0x1000
}

case object ACC_ANNOTATION extends AccessFlag
    with TClassFlag {
  val hasKeyword = true // TODO: ???
  val keyword = "@" // TODO: ???
  val code = 0x2000
}

case object ACC_ENUM extends AccessFlag
    with TClassFlag with TNestedClassFlag with TFieldFlag {  // class(?) field inner
  val hasKeyword = true
  val keyword = "enum"
  val code = 0x4000
  }

case object ACC_MANDATED extends AccessFlag
    with TParameterFlag with TModuleFlag {  // parameter, module, module * TODO: what is "module *"
//  ACC_MANDATED
val hasKeyword = false
  val keyword = "????"  // TODO:
  val code = 0x8000
}

// TODO:
//case object ACC_MODULE extends AccessFlag
//    with TClassFlag {
//
//  //val ACC_MODULE = 0x8000; // class
//}

//    // ASM specific pseudo access flags
//    val ACC_DEPRECATED = 0x20000; // class, field, method

object AccessFlag {
  val classFlags: Map[Int, TClassFlag] = immutable.HashMap(
    0x0001 -> ACC_PUBLIC,     // Declared public; may be accessed from outside its package.
    0x0002 -> ACC_PRIVATE,	  // Marked private in source (for classes, nested class only).
    0x0004 -> ACC_PROTECTED,  // Marked protected in source (for classes, nested class only).
    0x0008 -> ACC_STATIC,	    // Marked or implicitly static in source (for classes, nested class only).
    0x0010 -> ACC_FINAL,      // Declared final; no subclasses allowed.
    0x0020 -> ACC_SUPER,		  // Treat superclass methods specially when invoked by the `invokespecial` instruction (for classes, non-nested class only).
    0x0200 -> ACC_INTERFACE,  // Is an interface, not a class.
    0x0400 -> ACC_ABSTRACT,   // Declared abstract; must not be instantiated.
    0x1000 -> ACC_SYNTHETIC,  // Declared synthetic; not present in the source code.
    0x2000 -> ACC_ANNOTATION,	// Declared as an annotation type.
    0x4000 -> ACC_ENUM        // Declared as an enum type.
  )

  val nestedClassFlags: Map[Int, TClassFlag] = immutable.HashMap.empty // TODO
  val fieldFlags: Map[Int, TClassFlag] = immutable.HashMap.empty // TODO
  val methodFlags: Map[Int, TClassFlag] = immutable.HashMap.empty // TODO
  val parameterFlags: Map[Int, TClassFlag] = immutable.HashMap.empty // TODO
  val moduleFlags: Map[Int, TClassFlag] = immutable.HashMap.empty // TODO
}

