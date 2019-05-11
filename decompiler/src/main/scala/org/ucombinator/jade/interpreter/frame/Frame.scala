package org.ucombinator.jade.interpreter.frame

import org.objectweb.asm.tree.analysis.{Value, Frame => AsmFrame}

case class Frame[A](local: Vector[A], stack: Vector[A]) {
  def map[B](f: A => B): Frame[B] = {
    Frame(this.local.map(f), this.stack.map(f))
  }
}

case object Frame {
  def apply[A <: Value](frame: AsmFrame[A]): Frame[A] = {
    val local = (0 until frame.getLocals).map(frame.getLocal).toVector
    val stack = (0 until frame.getStackSize).map(frame.getStack).toVector
    Frame(local, stack)
  }
}
