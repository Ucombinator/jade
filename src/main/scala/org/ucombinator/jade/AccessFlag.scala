package org.ucombinator.jade

import scala.collection.immutable._

sealed trait TAccessFlag {
  val hasKeyword: Boolean
  val keyword: Option[String]
  val code: Int
}

trait TClassFlag extends TAccessFlag
trait TFieldFlag extends TAccessFlag
trait TMethodFlag extends TAccessFlag
trait TNestedClassFlag extends TClassFlag
trait TParameterFlag extends TAccessFlag
trait TModuleFlag extends TAccessFlag

sealed abstract class AccessFlag extends TAccessFlag

case object ACC_PUBLIC extends AccessFlag
    with TClassFlag with TFieldFlag with TMethodFlag with TNestedClassFlag {
  val hasKeyword = true
  val keyword = Some("public")
  val code = 0x0001
}

case object ACC_PRIVATE extends AccessFlag
    with TClassFlag with TFieldFlag with TMethodFlag with TNestedClassFlag {
  val hasKeyword = true
  val keyword = Some("private")
  val code = 0x0002
}

case object ACC_PROTECTED extends AccessFlag
    with TClassFlag with TFieldFlag with TMethodFlag with TNestedClassFlag {
  val hasKeyword = true
  val keyword = Some("protected")
  val code = 0x0004
}

case object ACC_STATIC extends AccessFlag
    with TFieldFlag with TMethodFlag with TNestedClassFlag {
  val hasKeyword = true
  val keyword = Some("static")
  val code = 0x0008
}

case object ACC_FINAL extends AccessFlag
    with TClassFlag with TFieldFlag with TMethodFlag with TNestedClassFlag with TParameterFlag {
  val hasKeyword = true
  val keyword = Some("final")
  val code = 0x0010
}

case object ACC_SUPER extends AccessFlag
    with TClassFlag {
  val hasKeyword = false
  val keyword = Option.empty[String]
  val code = 0x0020
}

case object ACC_SYNCHRONIZED extends AccessFlag
    with TMethodFlag {
  val hasKeyword = true
  val keyword = Some("synchronized")
  val code = 0x0020
}

//val ACC_OPEN = 0x0020; // module

//val ACC_TRANSITIVE = 0x0020; // module requires

case object ACC_VOLATILE extends AccessFlag
    with TFieldFlag {
  val hasKeyword = true
  val keyword = Some("volatile")
  val code = 0x0040
}

case object ACC_BRIDGE extends AccessFlag
    with TMethodFlag {
  val hasKeyword = false
  val keyword = Option.empty[String]
  val code = 0x0040
}

//val ACC_STATIC_PHASE = 0x0040; // module requires

case object ACC_VARARGS extends AccessFlag
    with TMethodFlag {
  val hasKeyword = false
  val keyword = Option.empty[String]
  val code = 0x0080
}

case object ACC_TRANSIENT extends AccessFlag
    with TFieldFlag {
  val hasKeyword = true
  val keyword = Some("transient")
  val code = 0x0080
}

case object ACC_NATIVE extends AccessFlag
    with TMethodFlag {
  val hasKeyword = true
  val keyword = Some("native")
  val code = 0x0100
}

case object ACC_INTERFACE extends AccessFlag
    with TClassFlag with TNestedClassFlag {
  val hasKeyword = true
  val keyword = Some("interface")  // TODO: ??? From the specification, an interface also need acc_abstract flag??? page 68 JVM 8
  val code = 0x0200
}

case object ACC_ABSTRACT extends AccessFlag
    with TClassFlag with TMethodFlag with TNestedClassFlag {
  val hasKeyword = true
  val keyword = Some("abstract")  // TODO: also used to mark `interface`
  val code = 0x0400
}

case object ACC_STRICT extends AccessFlag
    with TMethodFlag {
  val hasKeyword = true
  val keyword = Some("strictfp")
  val code = 0x0800
  }

// class, field, method, parameter, module *
case object ACC_SYNTHETIC extends AccessFlag
    with TClassFlag with TFieldFlag with TMethodFlag with TNestedClassFlag
    with TParameterFlag with TModuleFlag {
  val hasKeyword = false
  val keyword = Option.empty[String]
  val code = 0x1000
}

case object ACC_ANNOTATION extends AccessFlag
    with TClassFlag with TNestedClassFlag {
  val hasKeyword = true // TODO: ???
  val keyword = Some("@") // TODO: ???
  val code = 0x2000
}

case object ACC_ENUM extends AccessFlag
    with TClassFlag with TFieldFlag with TNestedClassFlag {  // class(?) field inner
  val hasKeyword = true
  val keyword = Some("enum")
  val code = 0x4000
  }

case object ACC_MANDATED extends AccessFlag
    with TParameterFlag with TModuleFlag {  // parameter, module, module * TODO: what is "module *"
val hasKeyword = false
  val keyword = Option.empty[String]  // TODO:
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
  val classFlags: Map[Int, TClassFlag] = HashMap(
    0x0001 -> ACC_PUBLIC,      // Declared public; may be accessed from outside its package.
    0x0002 -> ACC_PRIVATE,	   // Marked private in source (for classes, nested class only).
    0x0004 -> ACC_PROTECTED,   // Marked protected in source (for classes, nested class only).
    0x0008 -> ACC_STATIC,	     // Marked or implicitly static in source (for classes, nested class only).
    0x0010 -> ACC_FINAL,       // Declared final; no subclasses allowed.
    0x0020 -> ACC_SUPER,		   // Treat superclass methods specially when invoked by the `invokespecial` instruction (for classes, non-nested class only).
    0x0200 -> ACC_INTERFACE,   // Is an interface, not a class.
    0x0400 -> ACC_ABSTRACT,    // Declared abstract; must not be instantiated.
    0x1000 -> ACC_SYNTHETIC,   // Declared synthetic; not present in the source code.
    0x2000 -> ACC_ANNOTATION,	 // Declared as an annotation type.
    0x4000 -> ACC_ENUM         // Declared as an enum type.
  )

  val fieldFlags: Map[Int, TFieldFlag] = HashMap(
    0x0001 -> ACC_PUBLIC,     // Declared public; may be accessed from outside its package.
    0x0002 -> ACC_PRIVATE,    // Declared private; usable only within the defining class.
    0x0004 -> ACC_PROTECTED,  // Declared protected; may be accessed within subclasses.
    0x0008 -> ACC_STATIC,     // Declared static.
    0x0010 -> ACC_FINAL,      // Declared final; never directly assigned to after object construction (JLS §17.5).
    0x0040 -> ACC_VOLATILE,   // Declared volatile; cannot be cached.
    0x0080 -> ACC_TRANSIENT,  // Declared transient; not written or read by a persistent object manager.
    0x1000 -> ACC_SYNTHETIC,  // Declared synthetic; not present in the source code.
    0x4000 -> ACC_ENUM,       // Declared as an element of an enum.
  )

  val methodFlags: Map[Int, TMethodFlag] = HashMap(
    0x0001 -> ACC_PUBLIC,        // Declared public; may be accessed from outside its package.
    0x0002 -> ACC_PRIVATE,       // Declared private; accessible only within the defining class.
    0x0004 -> ACC_PROTECTED,     // Declared protected; may be accessed within subclasses.
    0x0008 -> ACC_STATIC,        // Declared static.
    0x0010 -> ACC_FINAL,         // Declared final; must not be overridden (§5.4.5).
    0x0020 -> ACC_SYNCHRONIZED,  // Declared synchronized; invocation is wrapped by a monitor use.
    0x0040 -> ACC_BRIDGE,        // A bridge method, generated by the compiler.
    0x0080 -> ACC_VARARGS,       // Declared with variable number of arguments.
    0x0100 -> ACC_NATIVE,        // Declared native; implemented in a language other than Java.
    0x0400 -> ACC_ABSTRACT,      // Declared abstract; no implementation is provided.
    0x0800 -> ACC_STRICT,        // Declared strictfp; floating-point mode is FPstrict.
    0x1000 -> ACC_SYNTHETIC,     // Declared synthetic; not present in the source code.
  )

  val nestedClassFlags: Map[Int, TNestedClassFlag] = HashMap(
        0x0001 -> ACC_PUBLIC,      // Marked or implicitly public in source.
        0x0002 -> ACC_PRIVATE,     // Marked private in source.
        0x0004 -> ACC_PROTECTED,   // Marked protected in source.
        0x0008 -> ACC_STATIC,      // Marked or implicitly static in source.
        0x0010 -> ACC_FINAL,       // Marked final in source.
        0x0200 -> ACC_INTERFACE,   // Was an interface in source.
        0x0400 -> ACC_ABSTRACT,    // Marked or implicitly abstract in source.
        0x1000 -> ACC_SYNTHETIC,   // Declared synthetic; not present in the source code.
        0x2000 -> ACC_ANNOTATION,  // Declared as an annotation type.
        0x4000 -> ACC_ENUM,        // Declared as an enum type.
  )

  // TODO: Comment
  val parameterFlags: Map[Int, TParameterFlag] = HashMap(
    0x0010 -> ACC_FINAL,
    0x1000 -> ACC_SYNTHETIC,
    0x8000 -> ACC_MANDATED
  )

  // TODO
  val moduleFlags: Map[Int, TModuleFlag] = HashMap.empty
}

