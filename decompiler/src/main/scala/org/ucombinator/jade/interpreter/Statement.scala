package org.ucombinator.jade.interpreter

import org.ucombinator.jade.interpreter.ir.{Identifier, Value}


trait Statement

case class VariableAssignment(variable: Identifier, value: Value) extends Statement
case class ArrayElementAssignment(variable: Value, value: Value) extends Statement
case class Invocation(invk: Any) extends Statement
case class Tmp(a: Any) extends Statement

