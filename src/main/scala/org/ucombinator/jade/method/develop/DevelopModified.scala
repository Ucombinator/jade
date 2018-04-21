package org.ucombinator.jade.method.develop

import org.objectweb.asm.{ClassReader, Opcodes}
import org.objectweb.asm.tree._
import org.objectweb.asm.tree.analysis.Frame
import org.ucombinator.jade.method._

import scala.collection.JavaConverters._
import scala.collection.mutable.ListBuffer


class DevelopModified(bytes: Array[Byte]) {

  val cn = new ClassNode
  val cr = new ClassReader(bytes)
  cr.accept(cn, 0)
  val method: MethodNode = cn.methods.asScala.filter(_.name == "foo").head
  val instructions: Array[AbstractInsnNode] = method.instructions.toArray
  val analyzer = new org.ucombinator.jade.method.IdentifierAnalyzer("TestIfLoop", method)
  //val tree = new InsnBlockTree("TestIfLoop", m)

  private val constantBytecodeInsn =
    Set(
      Opcodes.NOP,
      Opcodes.ACONST_NULL,
      Opcodes.ICONST_M1,
      Opcodes.ICONST_0,
      Opcodes.ICONST_1,
      Opcodes.ICONST_2,
      Opcodes.ICONST_3,
      Opcodes.ICONST_4,
      Opcodes.ICONST_5,
      Opcodes.LCONST_0,
      Opcodes.LCONST_1,
      Opcodes.FCONST_0,
      Opcodes.FCONST_1,
      Opcodes.FCONST_2,
      Opcodes.DCONST_0,
      Opcodes.DCONST_1,
      Opcodes.BIPUSH,
      Opcodes.SIPUSH,
      Opcodes.LDC,
      19, // ldc_w
      20  // ldc2_w
    )

  protected[this] val constantInsnToConst =
    Map(
      Opcodes.ACONST_NULL -> null,
      Opcodes.ICONST_M1   -> IntVal(-1),
      Opcodes.ICONST_0    -> IntVal(0),
      Opcodes.ICONST_1    -> IntVal(1),
      Opcodes.ICONST_2    -> IntVal(2),
      Opcodes.ICONST_3    -> IntVal(3),
      Opcodes.ICONST_4    -> IntVal(4),
      Opcodes.ICONST_5    -> IntVal(5),
      Opcodes.LCONST_0    -> LongVal(0L),
      Opcodes.LCONST_1    -> LongVal(1L),
      Opcodes.FCONST_0    -> FloatVal(0.0F),
      Opcodes.FCONST_1    -> FloatVal(1.0F),
      Opcodes.FCONST_2    -> FloatVal(2.0F),
      Opcodes.DCONST_0    -> DoubleVal(0.0),
      Opcodes.DCONST_1    -> DoubleVal(1.0)
    )

  protected[this] def getStack(frame: Frame[Identifier], index: Int): Identifier =
    frame.getStack(frame.getStackSize - index - 1)

  protected[this] def getTopStack(frame: Frame[Identifier]): Identifier =
    getStack(frame: Frame[Identifier], 0)

  // TODO: conciser!!!
  protected[this] def getTop2Stack(frame: Frame[Identifier]): (Identifier, Identifier) = {
    val r = List(0, 1).map(getStack(frame, _))
    (r: @unchecked) match { case List(x, y) => (x, y) }
  }

  // TODO: conciser!!!
  protected[this] def getTop3Stack(frame: Frame[Identifier]): (Identifier, Identifier, Identifier) = {
    val r = List(0, 1, 2).map(getStack(frame, _))
    (r: @unchecked) match { case List(x, y, z) => (x, y, z) }
  }


  def printInsnNode(i: AbstractInsnNode): Unit =
    i match {
      case fld:  FieldInsnNode         => println(s"$fld -- name: ${fld.name}")
      case iinc: IincInsnNode          => println(s"$iinc -- var: ${iinc.`var`} -- incr: ${iinc.incr}")
      case insn: InsnNode              => println(s"$insn -- no operand")
      case int:  IntInsnNode           => println(s"$int -- ${int.operand}")
      case ivk:  InvokeDynamicInsnNode => println(s"$ivk -- bsm: ${ivk.bsm} -- bsmArgs: ${ivk.bsmArgs}")
      case jmp:  JumpInsnNode          => println(s"$jmp -- label: ${jmp.label}")
      case ldc:  LdcInsnNode           => println(s"$ldc -- cst: ${ldc.cst}")
      case ls:   LookupSwitchInsnNode  =>
        println(s"$ls -- dlft: ${ls.dflt} -- keys: ${ls.keys} -- lables: ${ls.labels}")
      case m:    MethodInsnNode        =>
        println(s"$m -- desc: ${m.desc} -- itf: ${m.itf} -- name: ${m.name}")
      case ts:   TableSwitchInsnNode   =>
        println(s"$ts -- dflt: ${ts.dflt} -- labels: ${ts.labels} -- max: ${ts.max} -- min: ${ts.min}")
      case t:    TypeInsnNode          => println(s"$t -- desc: ${t.desc}")
      case v:    VarInsnNode           => println(s"$v -- var: ${v.`var`}")
      case _ => println("Non-Insn")
    }

  var stackMap = Map.empty[Identifier, Any]
  var localVariableMap = Map.empty[Identifier, Any]

  var localVariable = Map.empty[Identifier, Any]

  def getEventually(key: Identifier, mp: => Map[Identifier, Any]): (Identifier, Any) = {
    println("(((((((((((((((((((((((((()")
    println(mp)

    if (!mp.contains(key))
      key -> key  //TODO:
    else
      mp(key) match {
        case id @ Identifier(_, 0, _) =>
          println("1")
          id -> id  //TODO:
        case id: Identifier           =>
          println("2")
          getEventually(id, mp)
        case v                        =>
          println("2")
          key -> v
      }
  }

  def nPara(desc: String): Int = {
    //  require  -- valid descriptor
    // val pattern = """\(.*\).*"""
    // TODO: this is wrong, it doesn't work for class and array
    desc.drop(1).takeWhile(_ != ')').length
  }

  def getInitLocalVariableMap(frame: Frame[Identifier]): Map[Identifier, Val] =
    (0 until frame.getLocals).
      map(frame.getLocal).
      map(id => id -> id).
      toMap

  val tmpInsns: Array[AbstractInsnNode] = instructions ++ Array(instructions.last)
  val tmpFrames: Array[Frame[Identifier]] = analyzer.frames ++ Array(analyzer.frames.last)
  val insnFramePairs: List[(AbstractInsnNode, Frame[Identifier])] = tmpInsns.toList zip tmpFrames

  // TODO:
//  val code = new ListBuffer[Statement]
  val code = new ListBuffer[String]
}

