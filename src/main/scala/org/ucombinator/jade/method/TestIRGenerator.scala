package org.ucombinator.jade.method

import org.objectweb.asm.ClassReader
import org.objectweb.asm.tree._
import org.objectweb.asm.tree.analysis.Frame
import org.ucombinator.jade.ir.{Identifier, Value}

import scala.collection.JavaConverters._


class TestIRGenerator(bytes: Array[Byte], methodName: String = "foo") {
  private val cn = new ClassNode

  private val cr = new ClassReader(bytes)

  cr.accept(cn, 0)

  private val method: MethodNode = cn.methods.asScala.filter(_.name == methodName).head
  private val analyzer = new org.ucombinator.jade.method.IdentifierAnalyzer(cn.name, method)
  //val tree = new InsnBlockTree("TestIfLoop", m)

  protected[this] val insnFramePairs: List[(AbstractInsnNode, Frame[Identifier])] = {
    val pairs = method.instructions.toArray zip analyzer.frames
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
