package org.ucombinator.jade

import org.jgrapht.graph.DirectedPseudograph
import org.objectweb.asm._
import org.objectweb.asm.tree._
import org.objectweb.asm.tree.analysis._

import scala.collection.JavaConverters._
import scala.collection.immutable

case class Identifier(id: Int, copyVersion: Int, basicValue: BasicValue) extends Value {
  override def getSize: Int = basicValue.getSize
}

class IdentifierInterpreter(identifierAnalyzer: IdentifierAnalyzer)
  extends Interpreter[Identifier](Opcodes.ASM6) {

  private var identifierCount = this.identifierAnalyzer.method.instructions.size

  private def identifier(insn: AbstractInsnNode, basicValue: BasicValue): Identifier = {
    if (basicValue == null) { null }
    else                    { Identifier(this.identifierAnalyzer.method.instructions.indexOf(insn), 0, basicValue) }
  }

  private def identifier(basicValue: BasicValue): Identifier = {
    val id =
      if (basicValue == null) { null }
      else {
        this.identifierCount += 1
        Identifier(this.identifierCount - 1, 0, basicValue)
      }

    if (this.identifierAnalyzer.tryCatchBlockNode != null) {
      // It is the caught exception
      this.identifierAnalyzer.caughtExceptionIdentifiers += this.identifierAnalyzer.tryCatchBlockNode.handler -> id
      this.identifierAnalyzer.tryCatchBlockNode = null
    }

    id
  }

  private val basicInterpreter = new BasicInterpreter

  /** Creates a new value that represents the given type.
    *
    * Called for method parameters (including <code>this</code>), exception
    * handler variable and with <code>null</code> type for variables reserved
    * by long and double types.
    *
    * @param typ
    *            a primitive or reference type, or <tt>null</tt> to represent
    *            an uninitialized value.
    * @return a value that represents the given type. The size of the returned
    *         value must be equal to the size of the given type.
    */
  override def newValue(typ: Type): Identifier = {
    val basicValue = this.basicInterpreter.newValue(typ)
    this.identifier(basicValue)
  }

  /** Interprets a bytecode instruction without arguments. This method is
    * called for the following opcodes:
    *
    * ACONST_NULL, ICONST_M1, ICONST_0, ICONST_1, ICONST_2, ICONST_3, ICONST_4,
    * ICONST_5, LCONST_0, LCONST_1, FCONST_0, FCONST_1, FCONST_2, DCONST_0,
    * DCONST_1, BIPUSH, SIPUSH, LDC, JSR, GETSTATIC, NEW
    *
    * @param insn the bytecode instruction to be interpreted.
    * @return the result of the interpretation of the given instruction.
    * @throws AnalyzerException
    * if an error occurred during the interpretation.
    */
  override def newOperation(insn: AbstractInsnNode): Identifier = {
    val basicValue = this.basicInterpreter.newOperation(insn)
    this.identifierAnalyzer.instructionArguments += insn -> List.empty
    this.identifier(insn, basicValue)
  }

  /** Interprets a bytecode instruction that moves a value on the stack or to
    * or from local variables. This method is called for the following opcodes:
    *
    * ILOAD, LLOAD, FLOAD, DLOAD, ALOAD, ISTORE, LSTORE, FSTORE, DSTORE,
    * ASTORE, DUP, DUP_X1, DUP_X2, DUP2, DUP2_X1, DUP2_X2, SWAP
    *
    * @param insn
    * the bytecode instruction to be interpreted.
    * @param value
    * the value that must be moved by the instruction.
    * @return the result of the interpretation of the given instruction. The
    *         returned value must be <tt>equal</tt> to the given value.
    * @throws AnalyzerException
    * if an error occurred during the interpretation.
    */
  override def copyOperation(insn: AbstractInsnNode, value: Identifier): Identifier = {
    val basicValue = this.basicInterpreter.copyOperation(insn, value.basicValue)
    // TODO
    this.identifierAnalyzer.copyVersion += 1
    val result = Identifier(this.identifierAnalyzer.method.instructions.indexOf(insn), this.identifierAnalyzer.copyVersion, basicValue)
    //    println(s"Before copy: $value")
    //    println(s"After copy: $result")
    result
  }

  /** Interprets a bytecode instruction with a single argument. This method is
    * called for the following opcodes:
    *
    * INEG, LNEG, FNEG, DNEG, IINC, I2L, I2F, I2D, L2I, L2F, L2D, F2I, F2L,
    * F2D, D2I, D2L, D2F, I2B, I2C, I2S, IFEQ, IFNE, IFLT, IFGE, IFGT, IFLE,
    * TABLESWITCH, LOOKUPSWITCH, IRETURN, LRETURN, FRETURN, DRETURN, ARETURN,
    * PUTSTATIC, GETFIELD, NEWARRAY, ANEWARRAY, ARRAYLENGTH, ATHROW, CHECKCAST,
    * INSTANCEOF, MONITORENTER, MONITOREXIT, IFNULL, IFNONNULL
    *
    * @param insn
    * the bytecode instruction to be interpreted.
    * @param value
    * the argument of the instruction to be interpreted.
    * @return the result of the interpretation of the given instruction.
    * @throws AnalyzerException
    * if an error occurred during the interpretation.
    */
  override def unaryOperation(insn: AbstractInsnNode, value: Identifier): Identifier = {
    val basicValue = this.basicInterpreter.unaryOperation(insn, value.basicValue)
    this.identifierAnalyzer.instructionArguments += insn -> List(value)
    this.identifier(insn, basicValue)
  }

  /** Interprets a bytecode instruction with two arguments. This method is
    * called for the following opcodes:
    *
    * IALOAD, LALOAD, FALOAD, DALOAD, AALOAD, BALOAD, CALOAD, SALOAD, IADD,
    * LADD, FADD, DADD, ISUB, LSUB, FSUB, DSUB, IMUL, LMUL, FMUL, DMUL, IDIV,
    * LDIV, FDIV, DDIV, IREM, LREM, FREM, DREM, ISHL, LSHL, ISHR, LSHR, IUSHR,
    * LUSHR, IAND, LAND, IOR, LOR, IXOR, LXOR, LCMP, FCMPL, FCMPG, DCMPL,
    * DCMPG, IF_ICMPEQ, IF_ICMPNE, IF_ICMPLT, IF_ICMPGE, IF_ICMPGT, IF_ICMPLE,
    * IF_ACMPEQ, IF_ACMPNE, PUTFIELD
    *
    * @param insn
    * the bytecode instruction to be interpreted.
    * @param value1
    * the first argument of the instruction to be interpreted.
    * @param value2
    * the second argument of the instruction to be interpreted.
    * @return the result of the interpretation of the given instruction.
    * @throws AnalyzerException
    * if an error occurred during the interpretation.
    */
  override def binaryOperation(insn: AbstractInsnNode, value1: Identifier, value2: Identifier): Identifier = {
    val basicValue = this.basicInterpreter.binaryOperation(insn, value1.basicValue, value2.basicValue)
    this.identifierAnalyzer.instructionArguments += insn -> List(value1, value2)
    this.identifier(insn, basicValue)
  }


  /** Interprets a bytecode instruction with three arguments. This method is
    * called for the following opcodes:
    *
    * IASTORE, LASTORE, FASTORE, DASTORE, AASTORE, BASTORE, CASTORE, SASTORE
    *
    * @param insn
    * the bytecode instruction to be interpreted.
    * @param value1
    * the first argument of the instruction to be interpreted.
    * @param value2
    * the second argument of the instruction to be interpreted.
    * @param value3
    * the third argument of the instruction to be interpreted.
    * @return the result of the interpretation of the given instruction.
    * @throws AnalyzerException
    * if an error occurred during the interpretation.
    */
  override def ternaryOperation(insn: AbstractInsnNode, value1: Identifier, value2: Identifier, value3: Identifier): Identifier = {
    val basicValue = this.basicInterpreter.ternaryOperation(insn, value1.basicValue, value2.basicValue, value3.basicValue)
    this.identifierAnalyzer.instructionArguments += insn -> List(value1, value2, value3)
    this.identifier(insn, basicValue)
  }

  /** Interprets a bytecode instruction with a variable number of arguments.
    * This method is called for the following opcodes:
    *
    * INVOKEVIRTUAL, INVOKESPECIAL, INVOKESTATIC, INVOKEINTERFACE,
    * MULTIANEWARRAY and INVOKEDYNAMIC
    *
    * @param insn
    * the bytecode instruction to be interpreted.
    * @param values
    * the arguments of the instruction to be interpreted.
    * @return the result of the interpretation of the given instruction.
    * @throws AnalyzerException
    * if an error occurred during the interpretation.
    */
  override def naryOperation(insn: AbstractInsnNode, values: java.util.List[_ <: Identifier]): Identifier = {
    val basicValue = this.basicInterpreter.naryOperation(insn, values.asScala.map(_.basicValue).asJava)
    this.identifierAnalyzer.instructionArguments += insn -> values.asScala.toList
    this.identifier(insn, basicValue)
  }

  /** Interprets a bytecode return instruction. This method is called for the
    * following opcodes:
    *
    * IRETURN, LRETURN, FRETURN, DRETURN, ARETURN
    *
    * @param insn
    * the bytecode instruction to be interpreted.
    * @param value
    * the argument of the instruction to be interpreted.
    * @param expected
    * the expected return type of the analyzed method.
    * @throws AnalyzerException
    * if an error occurred during the interpretation.
    */
  override def returnOperation(insn: AbstractInsnNode, value: Identifier, expected: Identifier): Unit = {
    // TODO: noop since already is a unary operation
    this.basicInterpreter.returnOperation(insn, value.basicValue, expected.basicValue)
    this.identifierAnalyzer.instructionArguments += insn -> List(value)
  }

  /** Merges two values. The merge operation must return a value that
    * represents both values (for instance, if the two values are two types,
    * the merged value must be a common super type of the two types. If the two
    * values are integer intervals, the merged value must be an interval that
    * contains the previous ones. Likewise for other types of values).
    *
    * @param v
    * a value.
    * @param w
    * another value.
    * @return the merged value. If the merged value is equal to <tt>v</tt>,
    *         this method <i>must</i> return <tt>v</tt>.
    */
  override def merge(v: Identifier, w: Identifier): Identifier = {
    val basicValue = this.basicInterpreter.merge(v.basicValue, w.basicValue)

    //    println(s"v: $v, w: $w")

    if (v == w) {
      //      println(s"== branch: $v")
      v
    }
    else if (this.identifierAnalyzer.ssaMap.getOrElse(v, Set.empty).contains(w)) {
      //      println(s"contains w branch: $v")
      v
    }
    else if (this.identifierAnalyzer.ssaMap.getOrElse(w, Set.empty).contains(v)) {
      //      println(s"contains v branch: $w")
      w
    }
    else {
      val result = this.identifier(basicValue)
      val ids = this.identifierAnalyzer.ssaMap.getOrElse(result, Set.empty) + (v, w)
      this.identifierAnalyzer.ssaMap += (result -> ids)
      //      println(s"new identifier branch: $result")
      result
    }
  }
}

// NOOP, GOTO, RET, RETURN
// JSR (exception?)

class IdentifierAnalyzer(val owner: String, val method: MethodNode) {
  var copyVersion: Int = 0

  var tryCatchBlockNode: TryCatchBlockNode = _
  var caughtExceptionIdentifiers = immutable.Map.empty[AbstractInsnNode, Identifier]
  var instructionArguments = immutable.Map.empty[AbstractInsnNode, List[Identifier]]
  var ssaMap = immutable.Map.empty[Identifier, Set[Identifier]]

  val edges = new DirectedPseudograph[AbstractInsnNode, Edge](classOf[Edge])
  val interpreter = new IdentifierInterpreter(this)
  val analyzer = new IdentifierAnalyzerImpl(this)
  val frames: Array[Frame[Identifier]] = analyzer.analyze(owner, method)

  println(f"method: ${method.signature} ${method.desc}")
  println("frames: " + frames.length)
  frames.foreach(f => println(f"frame: ${f}"))

  for {i <- 0 until method.instructions.size
       insn = method.instructions.get(i)} {
    println(f"args(${i}): ${insn.getOpcode} ${instructionArguments.get(insn)}")
  }

  for ((key, value) <- caughtExceptionIdentifiers) {
    println(f"handler: $key -> $value")
  }

  for ((key, value) <- ssaMap) {
    println(s"$key -> $value")
  }

}

final class Edge(val source: AbstractInsnNode, val target: AbstractInsnNode, val isException: Boolean) {
  override def toString = f"Edge($source, $target, $isException)"
}

class IdentifierAnalyzerImpl(identifierAnalyzer: IdentifierAnalyzer)
  extends Analyzer[Identifier](identifierAnalyzer.interpreter) {

  override protected def newControlFlowEdge(insn: Int, successor: Int): Unit = {
    this.identifierAnalyzer.copyVersion = 0
    val source = this.identifierAnalyzer.method.instructions.get(insn)
    val target = this.identifierAnalyzer.method.instructions.get(successor)
    this.identifierAnalyzer.edges.addVertex(source)
    this.identifierAnalyzer.edges.addVertex(target)
    this.identifierAnalyzer.edges.addEdge(source, target, new Edge(source, target, false))
    println(f"newControlFlowEdge: ${insn} -> ${successor}")
  }

  override protected def newControlFlowExceptionEdge(insn: Int, successor: Int): Boolean = ??? // Should never be called

  override protected def newControlFlowExceptionEdge(insn: Int, successor: TryCatchBlockNode): Boolean = {
    this.identifierAnalyzer.tryCatchBlockNode = successor

    val source = this.identifierAnalyzer.method.instructions.get(insn)
    val target = this.identifierAnalyzer.method.instructions.get(this.identifierAnalyzer.method.instructions.indexOf(successor.handler))
    this.identifierAnalyzer.edges.addVertex(source)
    this.identifierAnalyzer.edges.addVertex(target)
    this.identifierAnalyzer.edges.addEdge(source, target, new Edge(source, target, false))
    println(f"newControlFlowEdge: ${insn} -> ${successor}")
    true // the edge must always be considered by the analyzer
  }
}

