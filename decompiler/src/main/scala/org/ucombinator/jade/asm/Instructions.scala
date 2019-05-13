package org.ucombinator.jade.asm

import org.objectweb.asm.tree._

object Instructions {
  def toString(l: InsnList, i: AbstractInsnNode): String = {
    val index = l.indexOf(i)
    val typ = InstructionTypes.fromInt(i.getType)
    val opcode = if (i.getOpcode == -1) { "no_opcode" } else Opcodes.fromInt(i.getOpcode)
    f"$index $typ $opcode"
  }
}

/*
/** A node that represents a bytecode instruction. */
abstract class AbstractInstructionNode(opcode: Int) {
  def isInsn: Boolean = true
}

case class NonInsnNode(insn: AbstractInsnNode) extends AbstractInstructionNode(-1) {
  override val isInsn: Boolean = false
}

/** A node that represents an IINC bytecode instruction. */
case class IincInstructionNode(index: Int, incr: Int) extends AbstractInstructionNode(132)
/** A node that represents a zero operand instruction. */
case class InstructionNode(opcode: Int) extends AbstractInstructionNode(opcode) {
  // TODO:
  // require
}
/** A node that represents an instruction with a single int operand. */
case class IntInstructionNode(opcode: Int, operand: Int) extends AbstractInstructionNode(opcode) {
  // TODO:
  // require
}
/** A node that represents an invokedynamic instruction. */
case class InvokeDynamicInstructionNode(name: String, desc: String, bsm: Handle, bsmArgs: AnyRef*)
  extends AbstractInstructionNode(186)

/** A node that represents a jump instruction. */
case class JumpInstructionNode(opcode: Int, label: LabelNode) extends AbstractInstructionNode(opcode) {
  // TODO:
  // require
  // the opcode of the type instruction to be constructed. This opcode must be:
  // IFEQ, IFNE, IFLT, IFGE, IFGT, IFLE, IF_ICMPEQ, IF_ICMPNE, IF_ICMPLT, IF_ICMPGE, IF_ICMPGT, IF_ICMPLE,
  // IF_ACMPEQ, IF_ACMPNE, GOTO, JSR, IFNULL or IFNONNULL.
}

/** A node that represents an LDC instruction. */
case class LdcInstructionNode(cst: AnyRef) extends  AbstractInstructionNode(18) { // ?? LDC, LDC_W, LDC2_W
  // TODO: ???
  // require
}

/**  A node that represents a LOOKUPSWITCH instruction. */
case class LookupSwitchInstructionNode(dflt: LabelNode, keys: Array[Int], labels: Array[LabelNode]) extends  AbstractInstructionNode(171)

/** A node that represents a method instruction. */
case class MethodInstructionNode(opcode: Int, owner: String, name: String, desc: String)
  extends  AbstractInstructionNode(opcode) {
  // TODO: ???
  // require

}

/** A node that represents a MULTIANEWARRAY instruction. */
case class MultiANewArrayInstructionNode(desc: String, dims: Int)
  extends AbstractInstructionNode(197)
/** A node that represents a TABLESWITCH instruction. */
case class TableSwitchInstructionNode(min: Int, max: Int, dflt: LabelNode, labels: List[LabelNode])
  extends AbstractInstructionNode(170)

/** A node that represents a type instruction. */
case class TypeInstructionNode(opcode: Int, desc: String)
  extends AbstractInstructionNode(opcode) {
  // TODO: require
  // opcode - the opcode of the type instruction to be constructed. This opcode must be:
  // NEW, ANEWARRAY, CHECKCAST or INSTANCEOF.
}


/** A node that represents a local variable instruction. */
case class VarInstructionNode(opcode: Int, index: Int)
  extends AbstractInstructionNode(opcode) {
  // TODO: require
  // opcode - the opcode of the local variable instruction to be constructed. This opcode must be:
  // ILOAD, LLOAD, FLOAD, DLOAD, ALOAD, ISTORE, LSTORE, FSTORE, DSTORE, ASTORE or RET.
}


object Instructions {
  def iincInsnNode(insn: IincInsnNode): IincInstructionNode = IincInstructionNode(insn.`var`, insn.incr)
  def insnNode(insn: InsnNode): InstructionNode = InstructionNode(insn.getOpcode)

//  case class InstructionNode(opcode: Int) extends AbstractInstructionNode(opcode) {
  def intInsnNode(insn: IntInsnNode): IntInstructionNode = IntInstructionNode(insn.getOpcode, insn.operand)

  def invokeDynamicInsnNode(insn: InvokeDynamicInsnNode): InvokeDynamicInstructionNode =
    InvokeDynamicInstructionNode(insn.name, insn.desc, insn.bsm, insn.bsmArgs)

  def jumpInsnNode(insn: JumpInsnNode): JumpInstructionNode = JumpInstructionNode(insn.getOpcode, insn.label)
  def ldcInsnNode(insn: LdcInsnNode): LdcInstructionNode = LdcInstructionNode(insn.cst)  // ???

  def lookupSwitchInsnNode(insn: LookupSwitchInsnNode): LookupSwitchInstructionNode =
    LookupSwitchInstructionNode(insn.dflt, insn.keys.asScala.toArray.map(_.toInt), insn.labels.asScala.toArray)

  def methodInsnNode(insn: MethodInsnNode): MethodInstructionNode =
    MethodInstructionNode(insn.getOpcode, insn.owner, insn.name, insn.desc)

  def multiANewArrayInsnNode(insn: MultiANewArrayInsnNode): MultiANewArrayInstructionNode =
    MultiANewArrayInstructionNode(insn.desc, insn.dims)

  def tableSwitchInsnNode(insn: TableSwitchInsnNode): TableSwitchInstructionNode =
    TableSwitchInstructionNode(insn.min, insn.max, insn.dflt, insn.labels.asScala.toList)

  def typeInsnNode(insn: TypeInsnNode): TypeInstructionNode = TypeInstructionNode(insn.getOpcode, insn.desc)
  def varInsnNode(insn: VarInsnNode): VarInstructionNode = VarInstructionNode(insn.getOpcode, insn.`var`)

  // implicit
  def abstractNode(insn: AbstractInsnNode): AbstractInstructionNode = insn match {
    case ii: IincInsnNode              => iincInsnNode(ii)
    case in: InsnNode                  => insnNode(in)
    case int: IntInsnNode              => intInsnNode(int)
    case invDyn: InvokeDynamicInsnNode => invokeDynamicInsnNode(invDyn)
    case j: JumpInsnNode               => jumpInsnNode(j)
    case ldc: LdcInsnNode              => ldcInsnNode(ldc)
    case ls: LookupSwitchInsnNode      => lookupSwitchInsnNode(ls)
    case mtd: MethodInsnNode           => methodInsnNode(mtd)
    case mana: MultiANewArrayInsnNode  => multiANewArrayInsnNode(mana)
    case ts: TableSwitchInsnNode       => tableSwitchInsnNode(ts)
    case typ: TypeInsnNode             => typeInsnNode(typ)
    case v: VarInsnNode                => varInsnNode(v)
    case _                             => NonInsnNode(insn)
  }

}


//InsnList A doubly linked list of AbstractInsnNode objects.

//AnnotationNode A node that represents an annotationn.
//ClassNode A node that represents a class.
// TODO: !!!
//FieldInsnNode A node that represents a field instruction.
//FieldNode A node that represents a field.
//FrameNode A node that represents a stack map frame.
//InnerClassNode A node that represents an inner class.

//MethodNode A node that represents a method.

//LabelNode An AbstractInsnNode that encapsulates a Label.
//LineNumberNode A node that represents a line number declaration.


//LocalVariableNode A node that represents a local variable declaration.

//TryCatchBlockNode A node that represents a try catch block.

*/