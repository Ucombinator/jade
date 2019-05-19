package org.ucombinator.jade.util

import com.github.javaparser.ast.Modifier
import org.objectweb.asm.Opcodes

sealed trait ModifierType
// 4.1. The ClassFile Structure - Table 4.1-A. Class access and property modifiers - https://docs.oracle.com/javase/specs/jvms/se9/html/jvms-4.html#jvms-4.1-200-E.1
case object CLASS extends ModifierType
// Field access and property flags - https://docs.oracle.com/javase/specs/jvms/se9/html/jvms-4.html#jvms-4.5-200-A.1
case object FIELD extends ModifierType
// Method access and property flags - https://docs.oracle.com/javase/specs/jvms/se9/html/jvms-4.html#jvms-4.6-200-A.1
case object METHOD extends ModifierType
case object NESTED extends ModifierType // Table 4.7.6-A. Nested class access and property flags
case object PARAMETER extends ModifierType // 4.7.24. The MethodParameters Attribute
case object MODULE extends ModifierType // 4.7.25. The Module Attribute - https://docs.oracle.com/javase/specs/jvms/se9/html/jvms-4.html#jvms-4.7.25
case object REQUIRES extends ModifierType // 4.7.25. The Module Attribute
case object EXPORTS extends ModifierType // 4.7.25. The Module Attribute

sealed trait ModifierKind
case class Known(modifier: Modifier.Keyword) extends ModifierKind
case class Unknown(modifier: Int) extends ModifierKind

object Flag {
  import Modifier.Keyword._

  val flags = List[(Int, Option[Modifier.Keyword], Set[ModifierType])](
    (/* 0x0001 */ Opcodes.ACC_PUBLIC      , Some(PUBLIC)      , Set(CLASS, FIELD, METHOD)),
    (/* 0x0002 */ Opcodes.ACC_PRIVATE     , Some(PRIVATE)     , Set(CLASS, FIELD, METHOD)),
    (/* 0x0004 */ Opcodes.ACC_PROTECTED   , Some(PROTECTED)   , Set(CLASS, FIELD, METHOD)),
    (/* 0x0008 */ Opcodes.ACC_STATIC      , Some(STATIC)      , Set(FIELD, METHOD)),
    (/* 0x0010 */ Opcodes.ACC_FINAL       , Some(FINAL)       , Set(CLASS, FIELD, METHOD, PARAMETER)),
    (/* 0x0020 */ Opcodes.ACC_SUPER       , None              , Set(CLASS)),
    (/* 0x0020 */ Opcodes.ACC_SYNCHRONIZED, Some(SYNCHRONIZED), Set(METHOD)),
    (/* 0x0020 */ Opcodes.ACC_OPEN        , None              , Set(MODULE)),
    (/* 0x0020 */ Opcodes.ACC_TRANSITIVE  , Some(TRANSITIVE)  , Set(REQUIRES)),
    (/* 0x0040 */ Opcodes.ACC_VOLATILE    , Some(VOLATILE)    , Set(FIELD)),
    (/* 0x0040 */ Opcodes.ACC_BRIDGE      , None              , Set(METHOD)),
    (/* 0x0040 */ Opcodes.ACC_STATIC_PHASE, None              , Set(REQUIRES)),
    (/* 0x0080 */ Opcodes.ACC_VARARGS     , None              , Set(METHOD)),
    (/* 0x0080 */ Opcodes.ACC_TRANSIENT   , Some(TRANSIENT)   , Set(FIELD)),
    (/* 0x0100 */ Opcodes.ACC_NATIVE      , Some(NATIVE)      , Set(METHOD)),
    (/* 0x0200 */ Opcodes.ACC_INTERFACE   , None              , Set(CLASS)),
    (/* 0x0400 */ Opcodes.ACC_ABSTRACT    , Some(ABSTRACT)    , Set(CLASS, METHOD)),
    (/* 0x0800 */ Opcodes.ACC_STRICT      , Some(STRICTFP)    , Set(METHOD)),
    (/* 0x1000 */ Opcodes.ACC_SYNTHETIC   , None              , Set(CLASS, FIELD, METHOD, PARAMETER, EXPORTS)),
    (/* 0x2000 */ Opcodes.ACC_ANNOTATION  , None              , Set(CLASS)),
    (/* 0x4000 */ Opcodes.ACC_ENUM        , None              , Set(CLASS, FIELD)),
    (/* 0x8000 */ Opcodes.ACC_MANDATED    , None              , Set(PARAMETER, MODULE, EXPORTS)),
    (/* 0x8000 */ Opcodes.ACC_MODULE      , None              , Set(CLASS)))

  def foo(typ: ModifierType): Int => List[ModifierKind] = {
    x: Int => {
      val x2 = x & 0xFFFF // Ignore ASM specific flags, which occur above bit 16
      val q = flags.filter(f => (f._1 & x2) != 0 && f._3.contains(typ))
      val flagResult = q.map(_._1).fold(0)(_ | _)
      assert(x2 == flagResult, f"flag parsing error: want 0x$x%x, got 0x$flagResult%x")
      q.map(z => z._2 match {
        case Some(y) => Known(y)
        case None => Unknown(z._1) })
    }
  }

  def onlyKnown(flags: List[ModifierKind]): List[Modifier.Keyword] = {
    flags.flatMap({ case Known(m) => List(m); case Unknown(_) => List[Modifier.Keyword]() })
  }

  val classFlags = foo(CLASS)
  val methodFlags = foo(METHOD)
  val fieldFlags = foo(FIELD)
}
