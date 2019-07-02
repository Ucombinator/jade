package org.ucombinator.jade.util.asm

import java.io.{PrintWriter, StringWriter}

import org.objectweb.asm.Opcodes
import org.objectweb.asm.tree._
import org.objectweb.asm.util.{Textifier, TraceMethodVisitor}

import scala.collection.JavaConverters._

case class Insn(method: MethodNode, insn: AbstractInsnNode) {
  def index: Int = method.instructions.indexOf(insn)
  def shortString: String = Insn.shortString(method, insn)
  def longString: String = Insn.longString(method, insn)
  override def toString: String = Insn.longString(method, insn)

  // TODO: temporary until dominator algorithm performance is improved
  override def hashCode(): Int = insn.hashCode()
  // TODO: temporary until dominator algorithm performance is improved
  override def equals(obj: Any): Boolean = {
    (obj != null) && obj.isInstanceOf[Insn] && (obj.asInstanceOf[Insn].insn == insn)
  }
}

object Insn extends Textifier(Opcodes.ASM7) {
  private val stringWriter = new StringWriter()
  private val stringBuffer = stringWriter.getBuffer
  private val printWriter = new PrintWriter(stringWriter)
  private val methodVisitor = new TraceMethodVisitor(this)
  private var insnList: InsnList = _

  def shortString(method: MethodNode, insn: AbstractInsnNode): String = {
    shortString(method.instructions, insn)
  }

  def shortString(insnList: InsnList, insn: AbstractInsnNode): String = {
    // Ensure labels have the correct name
    if (insnList ne this.insnList) {
      this.insnList = insnList
      this.labelNames = new java.util.HashMap()
      for (insn <- insnList.iterator().asScala) {
        insn match {
          case insn: LabelNode => this.labelNames.put(insn.getLabel, f"L${insnList.indexOf(insn)}")
          case _ => /* Do nothing */
        }
      }
    }
    insn.accept(methodVisitor)
    this.print(printWriter)
    printWriter.flush()
    val string = stringBuffer.toString.trim
    stringBuffer.setLength(0)
    this.getText.clear()
    string
  }

  def longString(method: MethodNode, insn: AbstractInsnNode): String = {
    longString(method.instructions, insn)
  }

  def longString(insnList: InsnList, insn: AbstractInsnNode): String = {
    val index = insnList.indexOf(insn)
    val string = this.shortString(insnList, insn)
    val insnType = InsnTypes.fromInt(insn.getType)
    val typeString =
      if (insnType.endsWith("INSN")) {
        insnType.replace("_INSN", "")
      } else {
        "*" + insnType
      }

    // Not currently needed but keep it around so we can find it again
    //val opcode = if (i.getOpcode == -1) { "no_opcode" } else Printer.OPCODES(i.getOpcode)

    f"$index:$string ($typeString)"
  }
}
