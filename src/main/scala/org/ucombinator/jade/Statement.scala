package org.ucombinator.jade

trait Statement

case class Assignment(lhs: Identifier, rhs: Any) extends Statement
case class Invocation(invk: Any) extends Statement
case class Tmp(a: Any) extends Statement


trait Variable
// Identifier
//case class ArrayIndexing(, List[Math])

