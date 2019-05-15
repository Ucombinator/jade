package org.ucombinator.jade.method.ssa

import org.objectweb.asm._
import org.objectweb.asm.tree._
import org.objectweb.asm.tree.analysis._

import org.ucombinator.jade.method.controlFlowGraph.ControlFlowGraph

import scala.collection.JavaConverters._

sealed trait Var extends Value {
  def basicValue: BasicValue
  override def getSize: Int = basicValue.getSize
}

case class ParameterVar  (                        local: Int,   basicValue: BasicValue) extends Var
case class ReturnVar     (                                      basicValue: BasicValue) extends Var
case class CopyVar       (insn: AbstractInsnNode, version: Int, basicValue: BasicValue) extends Var
case class InstructionVar(insn: AbstractInsnNode,               basicValue: BasicValue) extends Var
case class ExceptionVar  (insn: AbstractInsnNode,               basicValue: BasicValue) extends Var
case class PhiVar        (insn: AbstractInsnNode, index: Int,   basicValue: BasicValue) extends Var
case object EmptyVar                                                                    extends Var {
  override val basicValue: BasicValue = BasicValue.UNINITIALIZED_VALUE
}

class SSAInterpreter extends Interpreter[Var](Opcodes.ASM7) {
  var copyVersion: Int = 0 // For `copyOperation` // TODO: copyIndex? copyPosition?
  var originInsn: AbstractInsnNode = _ // For `merge`
  var instructionArguments = Map.empty[AbstractInsnNode, (Var, List[Var])]
  var ssaMap = Map.empty[Var, Set[(AbstractInsnNode, Var)]]

  override def newValue(`type`: Type): Var = ??? // Should never be called

  override def newParameterValue(isInstanceMethod: Boolean, local: Int, `type`: Type): Var = {
    ParameterVar(local, SSA.basicInterpreter.newValue(`type`))
  }

  override def newReturnTypeValue(`type`: Type): Var = {
    // ASM requires that we return null when `type` is Type.VOID_TYPE
    if (`type` == Type.VOID_TYPE) { null }
    else { ReturnVar(SSA.basicInterpreter.newValue(`type`)) }
  }

  override def newEmptyValue(local: Int): Var = {
    EmptyVar
  }

  override def newExceptionValue(tryCatchBlockNode: TryCatchBlockNode, handlerFrame: Frame[Var], exceptionType: Type): Var = {
    ExceptionVar(tryCatchBlockNode.handler,
      SSA.basicInterpreter.newExceptionValue(
        tryCatchBlockNode, handlerFrame.asInstanceOf[Frame[BasicValue]], exceptionType))
  }

  def record(insn: AbstractInsnNode, args: List[Var], ret: Var): Var = {
    val x = (ret, args)
    this.instructionArguments += insn -> x
    ret
  }

  @throws[AnalyzerException]
  override def newOperation(insn: AbstractInsnNode): Var = {
    record(insn, List(), InstructionVar(insn, SSA.basicInterpreter.newOperation(insn)))
  }


  @throws[AnalyzerException]
  override def copyOperation(insn: AbstractInsnNode, value: Var): Var = {
    this.copyVersion += 1
    record(insn, List(value), CopyVar(insn, this.copyVersion, SSA.basicInterpreter.copyOperation(insn, value.basicValue)))
  }

  @throws[AnalyzerException]
  override def unaryOperation(insn: AbstractInsnNode, value: Var): Var = {
    record(insn, List(value), InstructionVar(insn, SSA.basicInterpreter.unaryOperation(insn, value.basicValue)))
  }

  @throws[AnalyzerException]
  override def binaryOperation(insn: AbstractInsnNode, value1: Var, value2: Var): Var = {
    record(insn, List(value1, value2),
      InstructionVar(insn,
        SSA.basicInterpreter.binaryOperation(
          insn, value1.basicValue, value2.basicValue)))
  }

  @throws[AnalyzerException]
  override def ternaryOperation(insn: AbstractInsnNode, value1: Var, value2: Var, value3: Var): Var = {
    record(insn, List(value1, value2, value3),
      InstructionVar(insn,
        SSA.basicInterpreter.ternaryOperation(
          insn, value1.basicValue, value2.basicValue, value3.basicValue)))
  }

  @throws[AnalyzerException]
  override def naryOperation(insn: AbstractInsnNode, values: java.util.List[_ <: Var]): Var = {
    record(insn, values.asScala.toList,
      InstructionVar(insn,
        SSA.basicInterpreter.naryOperation(
          insn, values.asScala.map(_.basicValue).asJava)))
  }

  @throws[AnalyzerException]
  override def returnOperation(insn: AbstractInsnNode, value: Var, expected: Var): Unit = {
    // Note that `unaryOperation` is also called whenever `returnOperation` is called
    SSA.basicInterpreter.returnOperation(insn, value.basicValue, expected.basicValue)
    record(insn, List(value), null)
  }

  override def merge(value1: Var, value2: Var): Var = {
    if (value1.isInstanceOf[PhiVar]) {
      val entry = (this.originInsn, value2)
      val ids = this.ssaMap.getOrElse(value1, Set.empty)
      this.ssaMap += (value1 -> (ids + entry))
      value1
    } else if (value1 == EmptyVar) {
      value2
    } else if (value2 == EmptyVar) {
      value1
    } else if (value1 == value2) {
      value1
    } else {
      throw new Exception(f"unexpected merge: value1: $value1 value2: $value2")
    }
  }
}

class SSAAnalyzer(method: MethodNode, cfg: ControlFlowGraph, interpreter: Interpreter[Var])
  extends Analyzer[Var](interpreter) {

  override protected def newFrame(numLocals: Int, numStack: Int): Frame[Var] = {
    // We override this method because it runs near the start of `Analyzer.analyze`
    // but after the `Analyzer.frames` array is created.
    // We use this method to initialize the frames at join points.
    for (insn <- method.instructions.toArray) {
      val insnIndex = method.instructions.indexOf(insn)
      // TODO: cache this computation?
      if (cfg.graph.incomingEdgesOf(insn).size() > 1 || cfg.handlers.exists(p => p.handler == insn)) {
        // We are at a join point
        val frame = cfg.frames(insnIndex)
        val newFrame = new Frame[Var](frame.getLocals, frame.getStackSize)
        // Note that we leave Frame.returnValue at null
        for (i <- 0 until frame.getLocals) {
          newFrame.setLocal(i, PhiVar(insn, i, frame.getLocal(i)))
        }
        for (i <- 0 until frame.getStackSize) {
          // Note that we use `push` instead of `setStack` as the `Frame` constructor
          // starts with an empty stack regardless of `stackSize`
          newFrame.push(PhiVar(insn, i + newFrame.getLocals, frame.getStack(i)))
        }
        this.getFrames()(insnIndex) = newFrame
      }
    }

    super.newFrame(numLocals, numStack)
  }
}

case class SSA(
  frames: Array[Frame[Var]],
  instructionArguments: Map[AbstractInsnNode, (Var, List[Var])],
  ssaMap: Map[Var, Set[(AbstractInsnNode, Var)]])

case object SSA {
  val basicInterpreter = new BasicInterpreter
  def apply(owner: String, method: MethodNode, cfg: ControlFlowGraph): SSA = {
    val interpreter = new SSAInterpreter()

    // Hook into a method that is called whenever `analyze` starts working on a new instruction
    val oldInstructions = method.instructions
    method.instructions = new InsnList {
      override def get(index: Int): AbstractInsnNode = {
        val insn = super.get(index)
        interpreter.copyVersion = 0
        interpreter.originInsn = insn
        insn
      }
    }

    for (i <- oldInstructions.toArray) {
      method.instructions.add(i)
    }

    val frames = new SSAAnalyzer(method, cfg, interpreter).analyze(owner, method)

    SSA(frames, interpreter.instructionArguments, interpreter.ssaMap)
  }
}
