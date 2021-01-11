package org.ucombinator.jade.util

sealed trait MyersList[+A] {
  val length: Int
  def head: A
  val next: MyersList[A]
  val jump: MyersList[A]
  def apply(length: Int): MyersList[A] = {
    if (length == this.length) { this }
    else if (length < this.jump.length) { this.next(length) }
    else { this.jump(length) }
  }
}

object MyersList {
  case object Nil extends MyersList[Nothing] {
    override val length = 0
    override def head: Nothing = Errors.fatal("Called head on MyersList.Nil")
    override val next = Nil
    override val jump = Nil
  }

  case class Cons[+A](head: A, tail: MyersList[A]) extends MyersList[A] {
    override val length = tail.length + 1
    override val next = tail
    override val jump = {
      if (tail.length - tail.jump.length == tail.jump.length - tail.jump.jump.length) { tail.jump.jump }
      else { tail }
    }
  }

  // Aligns with comparing .length but only when one is a predecessor of another
  def partialOrdering[A]: PartialOrdering[MyersList[A]] = new PartialOrdering[MyersList[A]] {
    override def tryCompare(x: MyersList[A], y: MyersList[A]): Option[Int] = {
      def test(result: Int, a: MyersList[A], b: MyersList[A]): Option[Int] = {
        if (a eq b) { Some(result) }
        else { None }
      }
      if (x.length == y.length) { test(0, x, y) }
      else if (x.length > y.length) { test(1, x(y.length), y) }
      else { test(-1, x, y(x.length)) }
    }
    override def lteq(x: MyersList[A], y: MyersList[A]): Boolean = {
      tryCompare(x, y) match {
        case None => false
        case Some(i) => i <= 0
      }
    }
  }
}
