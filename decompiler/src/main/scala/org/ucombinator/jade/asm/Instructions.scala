package org.ucombinator.jade.asm

import org.objectweb.asm.tree._

/*
import org.objectweb.asm.util.{TraceMethodVisitor, Textifier => ASMTextifier}

class Textifier(method: MethodNode) extends ASMTextifier {
  def foo(insn: AbstractInsnNode): Unit = {
    val p = new Textifier()
    val mv = new TraceMethodVisitor(p)
    insn.accept(???)

  }

  override def visitInsn(opcode: Int): Unit = {
    this.tab2 = "+"
    super.visitInsn(opcode)
  }

}
 */

object Instructions {
  def toString(l: InsnList, i: AbstractInsnNode): String = {
    val index = l.indexOf(i)
    val typ = InstructionTypes.fromInt(i.getType)
    val opcode = if (i.getOpcode == -1) { "no_opcode" } else Opcodes.fromInt(i.getOpcode)
    f"$index $typ $opcode"

    // TODO
    // realInstruction: Boolean
    // Annotations
    //
    // Types: (reflection to automate this? code gen to automate this?)
    //   field: owner, name, desc
    //   frame: type, local, stack
    //   iinc: index, incr (how does analyzer treat this?) // A node that represents an IINC instruction.
    //   insn // no operand instruction
    //   int: operand // A node that represents an instruction with a single int operand.
    //   invokeDynamic: name: String, desc: String, bsm: Handle, bsmArgs: AnyRef*
    //   jump: label
    //   label: value
    //   lineNumber: line, start
    //   ldc: cst (constant)
    //   lookupSwitch: dflt: LabelNode, keys: Array[Int], labels: Array[LabelNode]
    //   tableSwitch: min: Int, max: Int, dflt: LabelNode, labels: List[LabelNode])
    //   method (other than dynamic): owner: String, name: String, desc: String, inf: Boolean
    //   multiANewArray: desc: String, dims: Int
    //   type: desc: String
    //   var: index: Int
  }
}

//InsnList A doubly linked list of AbstractInsnNode objects.

//AnnotationNode A node that represents an annotationn.
//ClassNode A node that represents a class.
//FieldNode A node that represents a field.
//FrameNode A node that represents a stack map frame.
//InnerClassNode A node that represents an inner class.

//MethodNode A node that represents a method.

//LocalVariableAnnotationNode
//TypeAnnotationNode
//LocalVariableNode A node that represents a local variable declaration.

//TryCatchBlockNode A node that represents a try catch block.

//ModuleExportNode
//ModuleNode
//ModuleOpenNode
//ModuleProvideNode
//ModuleRequireNode

//ParameterNode

//Opcodes

//////////////
//Type