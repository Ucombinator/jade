package org.ucombinator.jade.interpreter.frame

import org.objectweb.asm.tree.analysis.Frame
import org.ucombinator.jade.interpreter.ir.{Identifier, Value}

import scala.annotation.tailrec


trait RichFrameOperations extends BasicFrameOperations {
  final def nthStackValue(frame: Frame[Identifier], index: Int,
                          stackMap: Map[Identifier, Value],
                          localVariableMap: Map[Identifier, Value]): Value =
    getEventually(nthStack(frame)(index), stackMap, localVariableMap)


  final def topNStackValues(frame: Frame[Identifier], n: Int,
                            stackMap: Map[Identifier, Value],
                            localVariableMap: Map[Identifier, Value]): List[Value] =
    topNStacks(frame, n).map(getEventually(_, stackMap, localVariableMap))


  final def topStackValue(frame: Frame[Identifier],
                          stackMap: Map[Identifier, Value],
                          localVariableMap: Map[Identifier, Value]): Value =
    topNStackValues(frame, 1, stackMap, localVariableMap).head

  final def top2StackValues(frame: Frame[Identifier],
                            stackMap: Map[Identifier, Value],
                            localVariableMap: Map[Identifier, Value]): List[Value] =
    topNStackValues(frame, 2, stackMap, localVariableMap)

  final def top3StackValues(frame: Frame[Identifier],
                            stackMap: Map[Identifier, Value],
                            localVariableMap: Map[Identifier, Value]): List[Value] =
    topNStackValues(frame, 3, stackMap, localVariableMap)


  @tailrec
  final def getEventually(key: Identifier,
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

