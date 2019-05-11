package org.ucombinator.jade.interpreter.frame

import org.ucombinator.jade.interpreter.ir.Identifier

/** Frame Operations:
  * 1. Local Variable Operations;
  * 2. Stack Operations.
  * */
trait BasicFrameOperations {
  /** Local Variable */
  final def nthLocalVariable(frame: Frame[Identifier], index: Int): Identifier = {
    println(s"frame index: $index")
    frame.local(index)
  }

  /** Stack */
  final def nthStack(frame: Frame[Identifier])(index: Int): Identifier =
    frame.stack(frame.stack.length - index - 1)

  final def topNStacks(frame: Frame[Identifier], n: Int): List[Identifier] =
    (0 until n).map(nthStack(frame)).toList

  final def topStack(frame: Frame[Identifier]): Identifier =
    topNStacks(frame, 1).head

  final def top2Stacks(frame: Frame[Identifier]): List[Identifier] =
    topNStacks(frame, 2)

  final def top3Stacks(frame: Frame[Identifier]): List[Identifier] =
    topNStacks(frame, 3)

}
