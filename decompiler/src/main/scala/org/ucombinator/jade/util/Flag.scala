package org.ucombinator.jade.util

import com.github.javaparser.ast.Modifier


  // - https://docs.oracle.com/javase/specs/jvms/se9/html/jvms-4.html#jvms-4.1-200-E.1
  // - https://docs.oracle.com/javase/specs/jvms/se9/html/jvms-4.html#jvms-4.5-200-A.1
  // - https://docs.oracle.com/javase/specs/jvms/se9/html/jvms-4.html#jvms-4.6-200-A.1
  // - https://docs.oracle.com/javase/specs/jvms/se9/html/jvms-4.html#jvms-4.7.25
object Flag {
  val classFlags = Map[Int, Modifier.Keyword](
//    -> Modifier.Keyword.DEFAULT,
    0x0001 -> Modifier.Keyword.PUBLIC,
    0x0002 -> Modifier.Keyword.PRIVATE,
    0x0004 -> Modifier.Keyword.PROTECTED,
    0x0008 -> Modifier.Keyword.STATIC,
    0x0010 -> Modifier.Keyword.FINAL,
    0x0020 -> Modifier.Keyword.SYNCHRONIZED,
//    0x0020 -> Modifier.Keyword.TRANSITIVE,
    0x0040 -> Modifier.Keyword.VOLATILE,
    0x0080 -> Modifier.Keyword.TRANSIENT,
    0x0100 -> Modifier.Keyword.NATIVE,
    0x0400 -> Modifier.Keyword.ABSTRACT,
    0x0800 -> Modifier.Keyword.STRICTFP,

  )
//  int ACC_SUPER = 0x0020; // class
//  int ACC_OPEN = 0x0020; // module
//  int ACC_BRIDGE = 0x0040; // method
//  int ACC_STATIC_PHASE = 0x0040; // module requires
//  int ACC_VARARGS = 0x0080; // method
//  int ACC_INTERFACE = 0x0200; // class
//  int ACC_SYNTHETIC = 0x1000; // class, field, method, parameter, module *
//  int ACC_ANNOTATION = 0x2000; // class
//  int ACC_ENUM = 0x4000; // class(?) field inner
//  int ACC_MANDATED = 0x8000; // parameter, module, module *
//  int ACC_MODULE = 0x8000; // class

  def intToFlag[T](map: Map[Int, T], i: Int): Set[T] = {
    map.filter(kv => (i & kv._1) != 0).values.toSet
  }
}
