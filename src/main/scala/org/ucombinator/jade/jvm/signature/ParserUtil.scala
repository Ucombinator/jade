package org.ucombinator.jade.jvm.signature

import org.ucombinator.jade.jvm.signature.Parser._

import scala.language.implicitConversions

object ParserUtil {
  def repbra[T](left: => Parser[Any], p: => Parser[T], right: => Parser[Any]): Parser[List[T]] = {
    opt(left ~> rep1(p) <~ right) ^^ (_.getOrElse(Nil))
  }

  implicit def seqToTuple[A,B,Z](f: (A, B) => Z): A ~ B => Z = {
    case a ~ b => f(a, b)
  }

  implicit def seqToTuple[A,B,C,Z](f: (A, B, C) => Z): A ~ B ~ C => Z = {
    case a ~ b ~ c => f(a, b, c)
  }

  implicit def seqToTuple[A,B,C,D,Z](f: (A, B, C, D) => Z): A ~ B ~ C ~ D => Z = {
    case a ~ b ~ c ~ d => f(a, b, c, d)
  }
}
