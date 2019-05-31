package org.ucombinator.jade.interpreter

import org.objectweb.asm.ClassReader
import org.objectweb.asm.tree._
import org.objectweb.asm.tree.analysis.{Frame => AsmFrame}
import org.ucombinator.jade.interpreter.frame.Frame
import org.ucombinator.jade.interpreter.ir.{Identifier, Value}
import org.ucombinator.jade.decompile.method.controlFlowGraph.ControlFlowGraph
import org.ucombinator.jade.decompile.method.ssa.{SSA, Var}

import scala.collection.JavaConverters._


class TestIRGenerator(bytes: Array[Byte], methodName: String = "foo") {
  private val cn = new ClassNode

  private val cr = new ClassReader(bytes)

  cr.accept(cn, 0)

  private val method: MethodNode = cn.methods.asScala.filter(_.name == methodName).head
  private val cfg = ControlFlowGraph(cn.name, method)
  private val analyzer = SSA(cn.name, method, cfg)
  //val tree = new InsnBlockTree("TestIfLoop", m)

  protected[this] val insnFramePairs: List[(AbstractInsnNode, Frame[Identifier])] = {
    def f(frame: AsmFrame[Var]): Frame[Identifier] = Frame(frame).map(Identifier)
    val frames = analyzer.frames map f
    val pairs = method.instructions.toArray zip frames
    (pairs :+ pairs.last).toList
  }

  object TestStraightLineCode extends BytecodeInterpreter(insnFramePairs) {
    override def main(): Unit = {
      interp(insnFramePairs, Map.empty[Identifier, Value], localVariableMap)
    }

  }

  TestStraightLineCode.main()

  TestStraightLineCode.code.foreach(println)

}
