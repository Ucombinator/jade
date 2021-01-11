package org.ucombinator.jade.asm

import java.io.{PrintWriter, StringWriter}
import java.lang.reflect.Modifier

import org.objectweb.asm.Opcodes
import org.objectweb.asm.tree._
import org.objectweb.asm.util.{Textifier, TraceMethodVisitor}

import scala.jdk.CollectionConverters._

import org.ucombinator.jade.util.Errors

case class Insn(method: MethodNode, insn: AbstractInsnNode) {
  def index: Int = method.instructions.indexOf(insn)
  def next: Insn = Insn(method, insn.getNext)
  def shortString: String = Insn.shortString(method, insn)
  def longString: String = Insn.longString(method, insn)
  override def toString: String = Insn.longString(method, insn)
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
    print(printWriter)
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
    val string = shortString(insnList, insn)
    val insnType = intToType(insn.getType)
    val typeString =
      if (insnType.endsWith("INSN")) {
        insnType.replace("_INSN", "")
      } else {
        "*" + insnType
      }

    // Not currently needed but keep it around so we can find it again
    //val opcode = if (i.getOpcode == -1) { "no_opcode" } else Printer.OPCODES(i.getOpcode)

    f"${index}:${string} (${typeString})"
  }

  val typeToInt: Map[String, Int] =
    (for (field <- classOf[AbstractInsnNode].getDeclaredFields) yield {
      // As of ASM 7.1, all final public static int members of AbstractInsNode are ones we want. Updates beware.
      if (field.getType == classOf[Int] && field.getModifiers == (Modifier.FINAL | Modifier.PUBLIC | Modifier.STATIC)) {
        Some(field.getName -> field.get(null).asInstanceOf[Integer].intValue())
      } else {
        None
      }
    }).toList.flatten.toMap

  val intToType: Map[Int, String] = typeToInt map { _.swap }

  // NOTE: valid only for Insn for the same method
  implicit val ordering = new Ordering[Insn] {
    override def compare(x: Insn, y: Insn): Int = {
      assert(x.method eq y.method) // TODO: assert message or log message
      if ((x eq y) || (x.insn eq y.insn)) { 0 }
      else if (x.index < y.index) { -1 }
      else if (x.index > y.index) { 1 }
      else { Errors.fatal(f"Incomparable Insn ${x.longString} and ${y.longString}") }
    }
  }
}
