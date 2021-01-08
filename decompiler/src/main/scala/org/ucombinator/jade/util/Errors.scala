package org.ucombinator.jade.util

object Errors {
  def unmatchedType(x: Any): Nothing = {
    fatal("Type not handled by match: " + x.getClass.getName)
  }

  def impossibleMatch(x: Any): Nothing = {
    fatal("Impossible value found in match: " + x.getClass.getName)
  }

  def fatal(msg: String): Nothing = {
    throw new Exception("Fatal error: " + msg)
  }
}
