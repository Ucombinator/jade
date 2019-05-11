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
case class Phi           (insn: AbstractInsnNode, index: Int,   basicValue: BasicValue) extends Var
case object EmptyVar                                                                    extends Var {
  override val basicValue: BasicValue = BasicValue.UNINITIALIZED_VALUE
}

class SSAInterpreter extends Interpreter[Var](Opcodes.ASM7) {
  var copyVersion: Int = 0 // For `copyOperation`
  var originInsn: AbstractInsnNode = _ // For `merge`
  var instructionArguments = Map.empty[AbstractInsnNode, (Var, List[Var])]
  var ssaMap = Map.empty[Var, Set[(AbstractInsnNode, Var)]]

  def record(insn: AbstractInsnNode, args: List[Var], ret: Var): Var = {
    val x = (ret, args)
    this.instructionArguments += insn -> x
    ret
  }

  /**
   * Creates a new value that represents the given type.
   *
   * <p>Called for method parameters (including <code>this</code>), exception handler variable and
   * with <code>null</code> type for variables reserved by long and double types.
   *
   * <p>An interpreter may choose to implement one or more of {@link
   * Interpreter#newReturnTypeValue(Type)}, {@link Interpreter#newParameterValue(boolean, int,
   * Type)}, {@link Interpreter#newEmptyValue(int)}, {@link
   * Interpreter#newExceptionValue(TryCatchBlockNode, Frame, Type)} to distinguish different types
   * of new value.
   *
   * @param type a primitive or reference type, or {@literal null} to represent an uninitialized
   *     value.
   * @return a value that represents the given type. The size of the returned value must be equal to
   *     the size of the given type.
   */
  override def newValue(`type`: Type): Var = ??? // Should never be called

  /**
   * Creates a new value that represents the given parameter type. This method is called to
   * initialize the value of a local corresponding to a method parameter in a frame.
   *
   * <p>By default, calls <code>newValue(type)</code>.
   *
   * @param isInstanceMethod {@literal true} if the method is non-static.
   * @param local the local variable index.
   * @param type a primitive or reference type.
   * @return a value that represents the given type. The size of the returned value must be equal to
   *     the size of the given type.
   */
  override def newParameterValue(isInstanceMethod: Boolean, local: Int, `type`: Type): Var = {
    ParameterVar(local, SSA.basicInterpreter.newValue(`type`))
  }

  /**
   * Creates a new value that represents the given return type. This method is called to initialize
   * the return type value of a frame.
   *
   * <p>By default, calls <code>newValue(type)</code>.
   *
   * @param type a primitive or reference type.
   * @return a value that represents the given type. The size of the returned value must be equal to
   *         the size of the given type.
   */
  override def newReturnTypeValue(`type`: Type): Var = {
    // ASM requires that we return null when `type` is Type.VOID_TYPE
    if (`type` == Type.VOID_TYPE) { null }
    else { ReturnVar(SSA.basicInterpreter.newValue(`type`)) }
  }

  /**
   * Creates a new uninitialized value for a local variable. This method is called to initialize the
   * value of a local that does not correspond to a method parameter, and to reset one half of a
   * size-2 value when the other half is assigned a size-1 value.
   *
   * <p>By default, calls <code>newValue(null)</code>.
   *
   * @param local the local variable index.
   * @return a value representing an uninitialized value. The size of the returned value must be
   *     equal to 1.
   */
  override def newEmptyValue(local: Int): Var = {
    EmptyVar
  }

  /**
   * Creates a new value that represents the given exception type. This method is called to
   * initialize the exception value on the call stack at the entry of an exception handler.
   *
   * <p>By default, calls <code>newValue(exceptionType)</code>.
   *
   * @param tryCatchBlockNode the exception handler.
   * @param handlerFrame the exception handler frame.
   * @param exceptionType the exception type handled by this handler.
   * @return a value that represents the given { @code exceptionType}. The size of the returned value
   *     must be equal to 1.
   */
  override def newExceptionValue(tryCatchBlockNode: TryCatchBlockNode, handlerFrame: Frame[Var], exceptionType: Type): Var = {
    ExceptionVar(tryCatchBlockNode.handler,
      SSA.basicInterpreter.newExceptionValue(
        tryCatchBlockNode, handlerFrame.asInstanceOf[Frame[BasicValue]], exceptionType))
  }

  /**
   * Interprets a bytecode instruction without arguments. This method is called for the following
   * opcodes:
   *
   * <p>ACONST_NULL, ICONST_M1, ICONST_0, ICONST_1, ICONST_2, ICONST_3, ICONST_4, ICONST_5,
   * LCONST_0, LCONST_1, FCONST_0, FCONST_1, FCONST_2, DCONST_0, DCONST_1, BIPUSH, SIPUSH, LDC, JSR,
   * GETSTATIC, NEW
   *
   * @param insn the bytecode instruction to be interpreted.
   * @return the result of the interpretation of the given instruction.
   * @throws AnalyzerException if an error occurred during the interpretation.
   */
  @throws[AnalyzerException]
  override def newOperation(insn: AbstractInsnNode): Var = {
    record(insn, List(), InstructionVar(insn, SSA.basicInterpreter.newOperation(insn)))
  }


  /**
   * Interprets a bytecode instruction that moves a value on the stack or to or from local
   * variables. This method is called for the following opcodes:
   *
   * <p>ILOAD, LLOAD, FLOAD, DLOAD, ALOAD, ISTORE, LSTORE, FSTORE, DSTORE, ASTORE, DUP, DUP_X1,
   * DUP_X2, DUP2, DUP2_X1, DUP2_X2, SWAP
   *
   * @param insn the bytecode instruction to be interpreted.
   * @param value the value that must be moved by the instruction.
   * @return the result of the interpretation of the given instruction. The returned value must be
   *     {@code equal} to the given value.
   * @throws AnalyzerException if an error occurred during the interpretation.
   */
  @throws[AnalyzerException]
  override def copyOperation(insn: AbstractInsnNode, value: Var): Var = {
    this.copyVersion += 1
    record(insn, List(value), CopyVar(insn, this.copyVersion, SSA.basicInterpreter.copyOperation(insn, value.basicValue)))
  }

  /**
   * Interprets a bytecode instruction with a single argument. This method is called for the
   * following opcodes:
   *
   * <p>INEG, LNEG, FNEG, DNEG, IINC, I2L, I2F, I2D, L2I, L2F, L2D, F2I, F2L, F2D, D2I, D2L, D2F,
   * I2B, I2C, I2S, IFEQ, IFNE, IFLT, IFGE, IFGT, IFLE, TABLESWITCH, LOOKUPSWITCH, IRETURN, LRETURN,
   * FRETURN, DRETURN, ARETURN, PUTSTATIC, GETFIELD, NEWARRAY, ANEWARRAY, ARRAYLENGTH, ATHROW,
   * CHECKCAST, INSTANCEOF, MONITORENTER, MONITOREXIT, IFNULL, IFNONNULL
   *
   * @param insn the bytecode instruction to be interpreted.
   * @param value the argument of the instruction to be interpreted.
   * @return the result of the interpretation of the given instruction.
   * @throws AnalyzerException if an error occurred during the interpretation.
   */
  @throws[AnalyzerException]
  override def unaryOperation(insn: AbstractInsnNode, value: Var): Var = {
    record(insn, List(value), InstructionVar(insn, SSA.basicInterpreter.unaryOperation(insn, value.basicValue)))
  }

  /**
   * Interprets a bytecode instruction with two arguments. This method is called for the following
   * opcodes:
   *
   * <p>IALOAD, LALOAD, FALOAD, DALOAD, AALOAD, BALOAD, CALOAD, SALOAD, IADD, LADD, FADD, DADD,
   * ISUB, LSUB, FSUB, DSUB, IMUL, LMUL, FMUL, DMUL, IDIV, LDIV, FDIV, DDIV, IREM, LREM, FREM, DREM,
   * ISHL, LSHL, ISHR, LSHR, IUSHR, LUSHR, IAND, LAND, IOR, LOR, IXOR, LXOR, LCMP, FCMPL, FCMPG,
   * DCMPL, DCMPG, IF_ICMPEQ, IF_ICMPNE, IF_ICMPLT, IF_ICMPGE, IF_ICMPGT, IF_ICMPLE, IF_ACMPEQ,
   * IF_ACMPNE, PUTFIELD
   *
   * @param insn the bytecode instruction to be interpreted.
   * @param value1 the first argument of the instruction to be interpreted.
   * @param value2 the second argument of the instruction to be interpreted.
   * @return the result of the interpretation of the given instruction.
   * @throws AnalyzerException if an error occurred during the interpretation.
   */
  @throws[AnalyzerException]
  override def binaryOperation(insn: AbstractInsnNode, value1: Var, value2: Var): Var = {
    record(insn, List(value1, value2),
      InstructionVar(insn,
        SSA.basicInterpreter.binaryOperation(
          insn, value1.basicValue, value2.basicValue)))
  }

  /**
   * Interprets a bytecode instruction with three arguments. This method is called for the following
   * opcodes:
   *
   * <p>IASTORE, LASTORE, FASTORE, DASTORE, AASTORE, BASTORE, CASTORE, SASTORE
   *
   * @param insn the bytecode instruction to be interpreted.
   * @param value1 the first argument of the instruction to be interpreted.
   * @param value2 the second argument of the instruction to be interpreted.
   * @param value3 the third argument of the instruction to be interpreted.
   * @return the result of the interpretation of the given instruction.
   * @throws AnalyzerException if an error occurred during the interpretation.
   */
  @throws[AnalyzerException]
  override def ternaryOperation(insn: AbstractInsnNode, value1: Var, value2: Var, value3: Var): Var = {
    record(insn, List(value1, value2, value3),
      InstructionVar(insn,
        SSA.basicInterpreter.ternaryOperation(
          insn, value1.basicValue, value2.basicValue, value3.basicValue)))
  }

  /**
   * Interprets a bytecode instruction with a variable number of arguments. This method is called
   * for the following opcodes:
   *
   * <p>INVOKEVIRTUAL, INVOKESPECIAL, INVOKESTATIC, INVOKEINTERFACE, MULTIANEWARRAY and
   * INVOKEDYNAMIC
   *
   * @param insn the bytecode instruction to be interpreted.
   * @param values the arguments of the instruction to be interpreted.
   * @return the result of the interpretation of the given instruction.
   * @throws AnalyzerException if an error occurred during the interpretation.
   */
  @throws[AnalyzerException]
  override def naryOperation(insn: AbstractInsnNode, values: java.util.List[_ <: Var]): Var = {
    record(insn, values.asScala.toList,
      InstructionVar(insn,
        SSA.basicInterpreter.naryOperation(
          insn, values.asScala.map(_.basicValue).asJava)))
  }

  /**
   * Interprets a bytecode return instruction. This method is called for the following opcodes:
   *
   * <p>IRETURN, LRETURN, FRETURN, DRETURN, ARETURN
   *
   * @param insn the bytecode instruction to be interpreted.
   * @param value the argument of the instruction to be interpreted.
   * @param expected the expected return type of the analyzed method.
   * @throws AnalyzerException if an error occurred during the interpretation.
   */
  @throws[AnalyzerException]
  override def returnOperation(insn: AbstractInsnNode, value: Var, expected: Var): Unit = {
    // Note that `unaryOperation` is also called whenever `returnOperation` is called
    SSA.basicInterpreter.returnOperation(insn, value.basicValue, expected.basicValue)
    record(insn, List(value), null)
  }

  /**
   * Merges two values. The merge operation must return a value that represents both values (for
   * instance, if the two values are two types, the merged value must be a common super type of the
   * two types. If the two values are integer intervals, the merged value must be an interval that
   * contains the previous ones. Likewise for other types of values).
   *
   * @param value1 a value.
   * @param value2 another value.
   * @return the merged value. If the merged value is equal to {@code value1}, this method
   *     <i>must</i> return { @code value1}.
   */
  override def merge(value1: Var, value2: Var): Var = {
    if (value1.isInstanceOf[Phi]) {
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
      if (cfg.graph.incomingEdgesOf(insn).size() > 1) {
        // We are at a join point
        val frame = cfg.frames(insnIndex)
        val newFrame = new Frame[Var](frame.getLocals, frame.getStackSize)
        // Note that we leave Frame.returnValue at null
        for (i <- 0 until frame.getLocals) {
          newFrame.setLocal(i, Phi(insn, i, frame.getLocal(i)))
        }
        for (i <- 0 until frame.getStackSize) {
          // Note that we use `push` instead of `setStack` as the `Frame` constructor starts with an empty stack regardless of `stackSize`
          newFrame.push(Phi(insn, i + newFrame.getLocals, frame.getStack(i)))
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
