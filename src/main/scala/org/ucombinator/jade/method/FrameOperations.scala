package org.ucombinator.jade.method

import org.objectweb.asm.tree.analysis.Frame

import scala.annotation.tailrec

/** Frame Operations:
  * 1. Local Variable Operations;
  * 2. Stack Operations.
  * */
trait FrameOperations {
  /** Top nth Local Variable */
  final protected[this] def nthLocalVariable(frame: Frame[Identifier], index: Int): Identifier =
    frame.getLocal(index)

  /** Top nth Stack */
  final protected[this] def nthStack(frame: Frame[Identifier])(index: Int): Identifier =
    frame.getStack(frame.getStackSize - index - 1)

  /** Top N stacks */
  final protected[this] def topNStacks(frame: Frame[Identifier], n: Int): List[Identifier] =
    (0 until n).map(nthStack(frame)).toList

  final protected[this] def topNStackValues(frame: Frame[Identifier], n: Int,
                                            stackMap: Map[Identifier, Value],
                                            localVariableMap: Map[Identifier, Value]): List[Value] =
    topNStacks(frame, n).map(getEventually(_, stackMap, localVariableMap))


  /** Top stack */
  final protected[this] def topStack(frame: Frame[Identifier]): Identifier =
    topNStacks(frame, 1).head

  final protected[this] def topStackValue(frame: Frame[Identifier],
                                          stackMap: Map[Identifier, Value],
                                          localVariableMap: Map[Identifier, Value]): Value =
    topNStackValues(frame, 1, stackMap, localVariableMap).head



  /** Top 2 stacks */
  final protected[this] def top2Stacks(frame: Frame[Identifier]): List[Identifier] =
    topNStacks(frame, 2)

  final protected[this] def top2StackValues(frame: Frame[Identifier],
                                            stackMap: Map[Identifier, Value],
                                            localVariableMap: Map[Identifier, Value]): List[Value] =
    topNStackValues(frame, 2, stackMap, localVariableMap)

  /** Top 3 stacks */
  final protected[this] def top3Stacks(frame: Frame[Identifier]): List[Identifier] =
    topNStacks(frame, 3)

  final protected[this] def top3StackValues(frame: Frame[Identifier],
                                            stackMap: Map[Identifier, Value],
                                            localVariableMap: Map[Identifier, Value]): List[Value] =
    topNStackValues(frame, 3, stackMap, localVariableMap)


  @tailrec
  final protected[this] def getEventually(key: Identifier,
                                          stkMap: Map[Identifier, Value],
                                          locVarMap: Map[Identifier, Value]): Value = {
    println(s"key: $key")  // TODO: Debug message! Delete after finish

    if (locVarMap.contains(key))
      key
    else {
      stkMap(key) match {
        case id: Identifier => getEventually(id, stkMap, locVarMap)
        case v              => v
      }
    }
  }

}
