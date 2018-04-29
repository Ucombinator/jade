package org.ucombinator.jade.util

import java.io.File
import java.nio.file.Path

import org.jgrapht._
import org.jgrapht.io.{DOTExporter, StringComponentNameProvider}
import org.objectweb.asm.tree._


object DebugUtil {

  class EscapedStringComponentNameProvider[N](quotes: Boolean) extends StringComponentNameProvider[N] {
    override def getName(component: N): String = {
      val s = (component.toString + " " + component.hashCode)
        .replaceAll("\\\\", "\\\\\\\\")
        .replaceAll("\"", "\\\\\"")
      if (quotes) { "\"" + s + "\""}
      else { s }
    }
  }

  def printToGraphvizFile[N, E](outputPath: Path, filename: String, graph: Graph[N, E]): Unit = {
    val dotExporter = new DOTExporter[N, E](
      new EscapedStringComponentNameProvider[N](true),
      null,
      null
    )
    dotExporter.exportGraph(graph, new File(outputPath.toString, filename))
  }

  def printInsnNode(i: AbstractInsnNode): Unit = {
    val op = org.ucombinator.jade.util.DebugUtil.translator(i)

    val message =
      i match {
        case fld:  FieldInsnNode         => s"$op  --  $i -- name: ${fld.name}"
        case iinc: IincInsnNode          => s"$op  --  $i -- var: ${iinc.`var`} -- incr: ${iinc.incr}"
        case _:    InsnNode              => s"$op  --  $i -- NO OPERAND"
        case int:  IntInsnNode           => s"$op  --  $i -- ${int.operand}"
        case ivk:  InvokeDynamicInsnNode => s"$op  --  $i -- bsm: ${ivk.bsm} -- bsmArgs: ${ivk.bsmArgs}"
        case jmp:  JumpInsnNode          => s"$op  --  $i -- label: ${jmp.label}"
        case ldc:  LdcInsnNode           => s"$op  --  $i -- cst: ${ldc.cst}"
        case ls:   LookupSwitchInsnNode  => s"$op  --  $i -- dlft: ${ls.dflt} -- keys: ${ls.keys} -- lables: ${ls.labels}"
        case m:    MethodInsnNode        => s"$op  --  $i -- desc: ${m.desc} -- itf: ${m.itf} -- name: ${m.name}"
        case ts:   TableSwitchInsnNode   => s"$op  --  $i -- dflt: ${ts.dflt} -- labels: ${ts.labels} -- max: ${ts.max} -- min: ${ts.min}"
        case t:    TypeInsnNode          => s"$op  --  $i -- desc: ${t.desc}"
        case v:    VarInsnNode           => s"$op  --  $i -- var: ${v.`var`}"
        case _                           => s"$op  --  $i"
      }

    println(message)
  }


  def translator(insnNode: AbstractInsnNode): String = {
    val codeToName: Map[Int, String] = Map(
      0 -> "NOP", // visitInsn
      1 -> "ACONST_NULL", // -
      2 -> "ICONST_M1", // -
      3 -> "ICONST_0", // -
      4 -> "ICONST_1", // -
      5 -> "ICONST_2", // -
      6 -> "ICONST_3", // -
      7 -> "ICONST_4", // -
      8 -> "ICONST_5", // -
      9 -> "LCONST_0", // -
      10 -> "LCONST_1", // -
      11 -> "FCONST_0", // -
      12 -> "FCONST_1", // -
      13 -> "FCONST_2", // -
      14 -> "DCONST_0", // -
      15 -> "DCONST_1", // -
      16 -> "BIPUSH", // visitIntInsn
      17 -> "SIPUSH", // -
      18 -> "LDC", // visitLdcInsn
      19 -> "LDC_W", // -
      20 -> "LDC2_W", // -
      21 -> "ILOAD", // visitVarInsn
      22 -> "LLOAD", // -
      23 -> "FLOAD", // -
      24 -> "DLOAD", // -
      25 -> "ALOAD", // -
      26 -> "ILOAD_0", // -
      27 -> "ILOAD_1", // -
      28 -> "ILOAD_2", // -
      29 -> "ILOAD_3", // -
      30 -> "LLOAD_0", // -
      31 -> "LLOAD_1", // -
      32 -> "LLOAD_2", // -
      33 -> "LLOAD_3", // -
      34 -> "FLOAD_0", // -
      35 -> "FLOAD_1", // -
      36 -> "FLOAD_2", // -
      37 -> "FLOAD_3", // -
      38 -> "DLOAD_0", // -
      39 -> "DLOAD_1", // -
      40 -> "DLOAD_2", // -
      41 -> "DLOAD_3", // -
      42 -> "ALOAD_0", // -
      43 -> "ALOAD_1", // -
      44 -> "ALOAD_2", // -
      45 -> "ALOAD_3", // -
      46 -> "IALOAD", // visitInsn
      47 -> "LALOAD", // -
      48 -> "FALOAD", // -
      49 -> "DALOAD", // -
      50 -> "AALOAD", // -
      51 -> "BALOAD", // -
      52 -> "CALOAD", // -
      53 -> "SALOAD", // -
      54 -> "ISTORE", // visitVarInsn
      55 -> "LSTORE", // -
      56 -> "FSTORE", // -
      57 -> "DSTORE", // -
      58 -> "ASTORE", // -
      59 -> "ISTORE_0", // -
      60 -> "ISTORE_1", // -
      61 -> "ISTORE_2", // -
      62 -> "ISTORE_3", // -
      63 -> "LSTORE_0", // -
      64 -> "LSTORE_1", // -
      65 -> "LSTORE_2", // -
      66 -> "LSTORE_3", // -
      67 -> "FSTORE_0", // -
      68 -> "FSTORE_1", // -
      69 -> "FSTORE_2", // -
      70 -> "FSTORE_3", // -
      71 -> "DSTORE_0", // -
      72 -> "DSTORE_1", // -
      73 -> "DSTORE_2", // -
      74 -> "DSTORE_3", // -
      75 -> "ASTORE_0", // -
      76 -> "ASTORE_1", // -
      77 -> "ASTORE_2", // -
      78 -> "ASTORE_3", // -
      79 -> "IASTORE", // visitInsn
      80 -> "LASTORE", // -
      81 -> "FASTORE", // -
      82 -> "DASTORE", // -
      83 -> "AASTORE", // -
      84 -> "BASTORE", // -
      85 -> "CASTORE", // -
      86 -> "SASTORE", // -
      87 -> "POP", // -
      88 -> "POP2", // -
      89 -> "DUP", // -
      90 -> "DUP_X1", // -
      91 -> "DUP_X2", // -
      92 -> "DUP2", // -
      93 -> "DUP2_X1", // -
      94 -> "DUP2_X2", // -
      95 -> "SWAP", // -
      96 -> "IADD", // -
      97 -> "LADD", // -
      98 -> "FADD", // -
      99 -> "DADD", // -
      100 -> "ISUB", // -
      101 -> "LSUB", // -
      102 -> "FSUB", // -
      103 -> "DSUB", // -
      104 -> "IMUL", // -
      105 -> "LMUL", // -
      106 -> "FMUL", // -
      107 -> "DMUL", // -
      108 -> "IDIV", // -
      109 -> "LDIV", // -
      110 -> "FDIV", // -
      111 -> "DDIV", // -
      112 -> "IREM", // -
      113 -> "LREM", // -
      114 -> "FREM", // -
      115 -> "DREM", // -
      116 -> "INEG", // -
      117 -> "LNEG", // -
      118 -> "FNEG", // -
      119 -> "DNEG", // -
      120 -> "ISHL", // -
      121 -> "LSHL", // -
      122 -> "ISHR", // -
      123 -> "LSHR", // -
      124 -> "IUSHR", // -
      125 -> "LUSHR", // -
      126 -> "IAND", // -
      127 -> "LAND", // -
      128 -> "IOR", // -
      129 -> "LOR", // -
      130 -> "IXOR", // -
      131 -> "LXOR", // -
      132 -> "IINC", // visitIincInsn
      133 -> "I2L", // visitInsn
      134 -> "I2F", // -
      135 -> "I2D", // -
      136 -> "L2I", // -
      137 -> "L2F", // -
      138 -> "L2D", // -
      139 -> "F2I", // -
      140 -> "F2L", // -
      141 -> "F2D", // -
      142 -> "D2I", // -
      143 -> "D2L", // -
      144 -> "D2F", // -
      145 -> "I2B", // -
      146 -> "I2C", // -
      147 -> "I2S", // -
      148 -> "LCMP", // -
      149 -> "FCMPL", // -
      150 -> "FCMPG", // -
      151 -> "DCMPL", // -
      152 -> "DCMPG", // -
      153 -> "IFEQ", // visitJumpInsn
      154 -> "IFNE", // -
      155 -> "IFLT", // -
      156 -> "IFGE", // -
      157 -> "IFGT", // -
      158 -> "IFLE", // -
      159 -> "IF_ICMPEQ", // -
      160 -> "IF_ICMPNE", // -
      161 -> "IF_ICMPLT", // -
      162 -> "IF_ICMPGE", // -
      163 -> "IF_ICMPGT", // -
      164 -> "IF_ICMPLE", // -
      165 -> "IF_ACMPEQ", // -
      166 -> "IF_ACMPNE", // -
      167 -> "GOTO", // -
      168 -> "JSR", // -
      169 -> "RET", // visitVarInsn
      170 -> "TABLESWITCH", // visiTableSwitchInsn
      171 -> "LOOKUPSWITCH", // visitLookupSwitch
      172 -> "IRETURN", // visitInsn
      173 -> "LRETURN", // -
      174 -> "FRETURN", // -
      175 -> "DRETURN", // -
      176 -> "ARETURN", // -
      177 -> "RETURN", // -
      178 -> "GETSTATIC", // visitFieldInsn
      179 -> "PUTSTATIC", // -
      180 -> "GETFIELD", // -
      181 -> "PUTFIELD", // -
      182 -> "INVOKEVIRTUAL", // visitMethodInsn
      183 -> "INVOKESPECIAL", // -
      184 -> "INVOKESTATIC", // -
      185 -> "INVOKEINTERFACE", // -
      186 -> "INVOKEDYNAMIC", // visitInvokeDynamicInsn
      187 -> "NEW", // visitTypeInsn
      188 -> "NEWARRAY", // visitIntInsn
      189 -> "ANEWARRAY", // visitTypeInsn
      190 -> "ARRAYLENGTH", // visitInsn
      191 -> "ATHROW", // -
      192 -> "CHECKCAST", // visitTypeInsn
      193 -> "INSTANCEOF", // -
      194 -> "MONITORENTER", // visitInsn
      195 -> "MONITOREXIT", // -
      196 -> "WIDE", // NOT VISITED
      197 -> "MULTIANEWARRAY", // visitMultiANewArrayInsn
      198 -> "IFNULL", // visitJumpInsn
      199 -> "IFNONNULL", // -
      200 -> "GOTO_W", // -
      201 -> "JSR_W"// -
    )

    codeToName.getOrElse(insnNode.getOpcode, insnNode.toString)
  }

}
