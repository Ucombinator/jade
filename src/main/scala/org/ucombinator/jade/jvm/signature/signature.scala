package org.ucombinator.jade.jvm

import scala.language.implicitConversions

package object signature {
  // The grammar includes the terminal symbol `Identifier` to denote the name of a type, field, method, formal parameter, local variable, or type variable, as generated by a Java compiler.
  def isSignatureIdentifierCharacter(char: Char): Boolean = { !".;[/<>:".contains(char) }

  // Helper so we can use a `String` as an `Identifier`
  implicit def stringToIdentifier(string: String): Identifier = Identifier(string)
}
