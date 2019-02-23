package org.ucombinator.jade.interpreter

import org.objectweb.asm.tree.analysis.{BasicValue => AsmBasicValue}


package object ir {
  def isInt(v: Value): Boolean =
    v match {
      case _: IV | Identifier(_, _, AsmBasicValue.INT_VALUE) => true
      case _ => false
    }

  def isReference(v: Value): Boolean =
    v match {
      case _: ClassV                                       => true
      case _: ArrayReferenceT                              => true
      case Identifier(_, _, AsmBasicValue.REFERENCE_VALUE) => true
      case _                                               => false
    }


  // TODO:
  // This one is not perfect:
  //   For `Identifier` we can find out if it is a REFERENCE_VALUE, but we can
  // distinguish "Array Reference" and "Non-Array Reference"! This is the
  // limitation of the org.objectweb.asm.tree.analysis.BasicValue
  def isArray(v: Value): Boolean = isReference(v)
}

