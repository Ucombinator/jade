package org.ucombinator.jade.decompile.method

import org.jgrapht.graph.DirectedPseudograph
import org.objectweb.asm.tree.TryCatchBlockNode
import org.ucombinator.jade.util.asm.Insn


/*
Non-Linear Stmt Types
  -Break
  -Continue
  -Return/Throw
  +Try-Catch-Finally
  +Synchronized
  +Do/While/For/For-each
  +If/Switch
Non-linear expressions
  Boolean(&&/||/!/==/!=/</>/<=/>=)
  Trinary Operator/Switch Expression
Nestings
*/

case class InsnTree(tree: InsnTree.Graph)

object InsnTree {
  type Graph = DirectedPseudograph[Vertex, Edge]

  sealed trait Vertex
  case class Single(insn: Insn) extends Vertex
  // NOTE: the following are not `case class` because we need pointer equality
  class Block() extends Vertex
  class If() extends Vertex
  class Switch() extends Vertex
  class Loop() extends Vertex
  class Try() extends Vertex
  class Synchronized() extends Vertex

// Edge types: if/goto/switch1/switch2
  sealed trait Edge
  case class BlockBody(source: Block, target: Vertex) extends Edge
  case class IfCondition(source: If, target: Vertex) extends Edge
  case class IfThen(source: If, target: Vertex) extends Edge
  case class IfElse(source: If, target: Vertex) extends Edge
  case class SwitchCondition(source: Switch, target: Vertex) extends Edge
  case class SwitchBranch(source: Switch, /*TODO: value,*/ target: Vertex) extends Edge
  case class LoopCondition(source: Loop, target: Vertex) extends Edge
  case class LoopBody(source: Loop, target: Vertex) extends Edge
  case class TryBody(source: Try, target: Vertex) extends Edge
  case class TryCatch(source: Try, target: Vertex, handler: TryCatchBlockNode) extends Edge
  case class SynchronizedObject(source: Synchronized, target: Vertex) extends Edge
  case class SynchronizedBody(source: Synchronized, target: Vertex) extends Edge

  // TODO: print graph
}
