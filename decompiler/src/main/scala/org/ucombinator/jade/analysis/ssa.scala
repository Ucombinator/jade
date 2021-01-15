package org.ucombinator.jade.analysis.ssa

import scala.jdk.CollectionConverters._

import org.objectweb.asm._
import org.objectweb.asm.tree._
import org.objectweb.asm.tree.analysis._
import org.ucombinator.jade.asm.Insn
import org.ucombinator.jade.asm.TypedBasicInterpreter
import org.ucombinator.jade.analysis.ControlFlowGraph
import org.ucombinator.jade.util.Errors
import org.ucombinator.jade.util.Log

sealed abstract class Var(val name: String) extends Value {
  def basicValue: BasicValue
  override def getSize: Int = basicValue.getSize
}

// format: off
case class ParameterVar  (basicValue: BasicValue,             local: Int  ) extends Var(f"parameterVar${local + 1}") // TODO: +1 parameter if non-static
case class ReturnVar     (basicValue: BasicValue                          ) extends Var(f"returnVar") // TODO: used only for "expected" return type
case class ExceptionVar  (basicValue: BasicValue, insn: Insn              ) extends Var(f"exceptionVar${insn.index}")
case class InstructionVar(basicValue: BasicValue, insn: Insn              ) extends Var(f"insnVar${insn.index}")
case class CopyVar       (basicValue: BasicValue, insn: Insn, version: Int) extends Var(f"copyVar${insn.index}_${version}")
case object EmptyVar                                                        extends Var(f"emptyVar") {
  override val basicValue: BasicValue = BasicValue.UNINITIALIZED_VALUE
}
case class PhiVar        (basicValue: BasicValue, insn: Insn, index: Int  , var changed: Boolean = false) extends Var(f"phiVar${insn.index}_${index}") {
// format: on
  // TODO: use private constructor to hide `changed`
  // Note that `changed` has to be in the parameters so that the analysis sees that the value has changed
  private var changedPhiVar: PhiVar = _
  def change(): PhiVar = {
    if (this.changedPhiVar != null) { this.changedPhiVar }
    else {
      this.changedPhiVar = this.copy(changed = true)
      this.changedPhiVar.changedPhiVar = this.changedPhiVar
      this.changedPhiVar
    }
  }
}

class SSAInterpreter(method: MethodNode) extends Interpreter[Var](Opcodes.ASM9) with Log {
  var copyOperationPosition: Int = 0 // For `copyOperation()`
  var originInsn: AbstractInsnNode = _ // For `merge`
  var instructionArguments = Map.empty[AbstractInsnNode, (Var, List[Var])]
  var ssaMap = Map.empty[Var, Set[(AbstractInsnNode, Var)]]
  var returnTypeValue: ReturnVar = _ // There is no getReturn method on frames, so we save it here

  def ssaMap(key: PhiVar, insn: AbstractInsnNode, value: Var, ignoreNull: Boolean = false): Unit = {
    if (!ignoreNull || value != null) {
      val usedKey = key.change()
      val entry = (insn, value)
      this.ssaMap += (usedKey -> (this.ssaMap.getOrElse(usedKey, Set.empty) + entry))
    }
  }

  override def newValue(`type`: Type): Var = Errors.fatal(f"Impossible call of newValue on ${`type`}")

  override def newParameterValue(isInstanceMethod: Boolean, local: Int, `type`: Type): Var = {
    ParameterVar(TypedBasicInterpreter.newValue(`type`), local)
  }

  override def newReturnTypeValue(`type`: Type): Var = {
    // ASM requires that we return null when `type` is Type.VOID_TYPE
    this.returnTypeValue =
      if (`type` == Type.VOID_TYPE) { null }
      else { ReturnVar(TypedBasicInterpreter.newReturnTypeValue(`type`)) }
    this.returnTypeValue
  }

  override def newEmptyValue(local: Int): Var = {
    EmptyVar
  }

  override def newExceptionValue(
      tryCatchBlockNode: TryCatchBlockNode,
      handlerFrame: Frame[Var],
      exceptionType: Type
  ): Var = {
    ExceptionVar(
      TypedBasicInterpreter
        .newExceptionValue(tryCatchBlockNode, handlerFrame.asInstanceOf[Frame[BasicValue]], exceptionType),
      Insn(method, tryCatchBlockNode.handler)
    )
  }

  def record(insn: AbstractInsnNode, args: List[Var], ret: Var): Var = {
    val x = (ret, args)
    this.instructionArguments += insn -> x
    ret
  }

  @throws[AnalyzerException]
  override def newOperation(insn: AbstractInsnNode): Var = {
    record(insn, List(), InstructionVar(TypedBasicInterpreter.newOperation(insn), Insn(method, insn)))
  }

  @throws[AnalyzerException]
  override def copyOperation(insn: AbstractInsnNode, value: Var): Var = {
    this.copyOperationPosition += 1
    record(
      insn,
      List(value),
      CopyVar(
        TypedBasicInterpreter.copyOperation(insn, value.basicValue),
        Insn(method, insn),
        this.copyOperationPosition
      )
    )
  }

  @throws[AnalyzerException]
  override def unaryOperation(insn: AbstractInsnNode, value: Var): Var = {
    record(
      insn,
      List(value),
      InstructionVar(TypedBasicInterpreter.unaryOperation(insn, value.basicValue), Insn(method, insn))
    )
  }

  @throws[AnalyzerException]
  override def binaryOperation(insn: AbstractInsnNode, value1: Var, value2: Var): Var = {
    record(
      insn,
      List(value1, value2),
      InstructionVar(
        TypedBasicInterpreter.binaryOperation(insn, value1.basicValue, value2.basicValue),
        Insn(method, insn)
      )
    )
  }

  @throws[AnalyzerException]
  override def ternaryOperation(insn: AbstractInsnNode, value1: Var, value2: Var, value3: Var): Var = {
    record(
      insn,
      List(value1, value2, value3),
      InstructionVar(
        TypedBasicInterpreter.ternaryOperation(insn, value1.basicValue, value2.basicValue, value3.basicValue),
        Insn(method, insn)
      )
    )
  }

  @throws[AnalyzerException]
  override def naryOperation(insn: AbstractInsnNode, values: java.util.List[_ <: Var]): Var = {
    record(
      insn,
      values.asScala.toList,
      InstructionVar(
        TypedBasicInterpreter.naryOperation(insn, values.asScala.map(_.basicValue).asJava),
        Insn(method, insn)
      )
    )
  }

  @throws[AnalyzerException]
  override def returnOperation(insn: AbstractInsnNode, value: Var, expected: Var): Unit = {
    // Note that `unaryOperation` is also called whenever `returnOperation` is called.
    // We override the effect of `unaryOperation` by calling `record` with `null` here.
    TypedBasicInterpreter.returnOperation(insn, value.basicValue, expected.basicValue)
    record(insn, List(value), null)
    ()
  }

  override def merge(value1: Var, value2: Var): Var = {
    if (value1.isInstanceOf[PhiVar]) {
      val newValue1 = value1.asInstanceOf[PhiVar].change()
      ssaMap(newValue1, this.originInsn, value2)
      newValue1
    } else if (value1 == EmptyVar) {
      value2
    } else if (value2 == EmptyVar) {
      value1
    } else if (value1 == value2) {
      value1
    } else {
      throw new Exception(f"unexpected merge: value1: ${value1} value2: ${value2}")
    }
  }
}

class SSAAnalyzer(cfg: ControlFlowGraph, interpreter: SSAInterpreter) extends Analyzer[Var](interpreter) {

  override def init(owner: String, method: MethodNode): Unit = {
    // We override this method because it runs near the start of `Analyzer.analyze`
    // but after the `Analyzer.frames` array is created.
    //
    // We use this method to:
    //  (1) initialize the frames at join points and
    //  (2) set the returnValue of each frame (as it is not updated by `Frame.merge`)
    //
    // We cannot do (1) in `merge` as all merged-in values need to know what instruction they
    // came from.  By the time `merge` runs, that information for the first value is gone.
    assert(cfg.method eq method)
    for (insn <- method.instructions.toArray) {
      val insnIndex = method.instructions.indexOf(insn)
      val minimumInEdges =
        if (insnIndex == 0) { 0 }
        else { 1 }
      if (
        cfg.graph.incomingEdgesOf(Insn(method, insn)).size() > minimumInEdges
        || cfg.method.tryCatchBlocks.asScala.exists(p => p.handler == insn)
      ) {
        // We are at a join point
        val cfgFrame = cfg.frames(insnIndex)
        val frame = this.getFrames()(insnIndex) match {
          case null => new Frame[Var](cfgFrame.getLocals, cfgFrame.getMaxStackSize)
          case f => f
        }
        // Note that Frame.returnValue is null until `frame.setReturn` later in this method
        for (i <- 0 until cfgFrame.getLocals) {
          assert((insnIndex == 0) == (frame.getLocal(i) != null))
          val phiVar = PhiVar(cfgFrame.getLocal(i), Insn(method, insn), i) // Note: not `.used`
          this.interpreter.ssaMap(phiVar.change(), this.interpreter.originInsn, frame.getLocal(i), true)
          frame.setLocal(i, phiVar)
        }
        // Note that we use `clearStack` and `push` instead of `setStack` as the `Frame` constructor
        // starts with an empty stack regardless of `stackSize`
        assert(frame.getStackSize == 0)
        for (i <- 0 until cfgFrame.getStackSize) {
          assert((insnIndex == 0) == (frame.getStack(i) != null))
          val phiVar = PhiVar(cfgFrame.getStack(i), Insn(method, insn), i + frame.getLocals)
          this.interpreter.ssaMap(phiVar.change(), this.interpreter.originInsn, frame.getStack(i), true)
          frame.push(phiVar)
        }
        this.getFrames()(insnIndex) = frame
      }
    }

    // Set the `Frame.returnValue` as it is not updated by `Frame.merge`.
    // This gets passed as to `returnOperation` as `expected`.
    for (frame <- this.getFrames) {
      // Unreachable code has null frames, so skip those
      if (frame != null) {
        frame.setReturn(interpreter.returnTypeValue)
      }
    }
  }
}

case class SSA(
    method: MethodNode,
    frames: Array[Frame[Var]],
    instructionArguments: Map[AbstractInsnNode, (Var, List[Var])],
    ssaMap: Map[Var, Set[(AbstractInsnNode, Var)]]
)

case object SSA {
  def apply(owner: String, method: MethodNode, cfg: ControlFlowGraph): SSA = {
    val interpreter = new SSAInterpreter(method)

    // Hook into a method that is called whenever `analyze` starts working on a new instruction
    val oldInstructions = method.instructions
    method.instructions = new InsnList {
      override def get(index: Int): AbstractInsnNode = {
        val insn = super.get(index)
        interpreter.copyOperationPosition = 0
        interpreter.originInsn = insn
        insn
      }
    }

    for (i <- oldInstructions.toArray) {
      method.instructions.add(i)
    }

    val frames = new SSAAnalyzer(cfg, interpreter).analyze(owner, method)

    SSA(method, frames, interpreter.instructionArguments, interpreter.ssaMap)
  }
}
