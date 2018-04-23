package org.ucombinator.jade.method

import org.objectweb.asm.{ClassReader, Opcodes}
import org.objectweb.asm.tree._
import org.objectweb.asm.tree.analysis.Frame
import org.ucombinator.jade.jvm.classfile.TypeCommons._
import org.ucombinator.jade.jvm.classfile.descriptor.DescriptorParser
import org.ucombinator.jade.method.bytecode._
import org.ucombinator.jade.method.bytecode.MathByteCode._

import scala.collection.JavaConverters._
import scala.collection.mutable.ListBuffer


// TODO: abstract here is added for testing: in Scala worksheet, I always extends this abstract class, and finish the `interp` method.
abstract class BytecodeInterpreter(bytes: Array[Byte], methodName: String) {

  protected[this] val cn = new ClassNode
  protected[this] val cr = new ClassReader(bytes)
  cr.accept(cn, 0)
  val method: MethodNode = cn.methods.asScala.filter(_.name == methodName).head  // TODO: for develop
  val analyzer = new org.ucombinator.jade.method.IdentifierAnalyzer("TestIfLoop", method)
  //val tree = new InsnBlockTree("TestIfLoop", m)

  //  protected[this] val insnFramePairs: List[(AbstractInsnNode, Frame[Identifier])] =
  //    (method.instructions.toArray zip analyzer.frames).toList

  protected[this] val insnFramePairs: List[(AbstractInsnNode, Frame[Identifier])] = {
    val instructions = method.instructions.toArray
    val tmpInsns: Array[AbstractInsnNode] = instructions ++ Array(instructions.last)
    val tmpFrames: Array[Frame[Identifier]] = analyzer.frames ++ Array(analyzer.frames.last)
    tmpInsns.toList zip tmpFrames
  }

  protected[this] val code = new ListBuffer[String] // TODO: Should finally be `val code = new ListBuffer[Statement]`


  protected[this] val newArrayMap = Map(
    4  -> Z, // "boolean"
    5  -> C, // "char"
    6  -> F, // "float"
    7  -> D, // "double"
    8  -> B, // "byte"
    9  -> S, // "short"
    10 -> I, // "int"
    11 -> J, // "long"
  )

  protected[this] val constantInsnToConst: Map[Int, Value] =
    Map(
      Opcodes.ACONST_NULL -> NullV,
      Opcodes.ICONST_M1   -> IV(-1),
      Opcodes.ICONST_0    -> IV(0),
      Opcodes.ICONST_1    -> IV(1),
      Opcodes.ICONST_2    -> IV(2),
      Opcodes.ICONST_3    -> IV(3),
      Opcodes.ICONST_4    -> IV(4),
      Opcodes.ICONST_5    -> IV(5),
      Opcodes.LCONST_0    -> JV(0L),
      Opcodes.LCONST_1    -> JV(1L),
      Opcodes.FCONST_0    -> FV(0.0F),
      Opcodes.FCONST_1    -> FV(1.0F),
      Opcodes.FCONST_2    -> FV(2.0F),
      Opcodes.DCONST_0    -> DV(0.0),
      Opcodes.DCONST_1    -> DV(1.0)
    )

  // TODO: getTopStackAsVariable  without getEventually
  // TODO: getTopStackAsValue     with    getEventually

  protected[this] def getStack(frame: Frame[Identifier])(index: Int): Identifier =
    frame.getStack(frame.getStackSize - index - 1)

  protected[this] def getTopStack(frame: Frame[Identifier]): Identifier =
    getStack(frame: Frame[Identifier])(0)

  protected[this] def getTopNStack(frame: Frame[Identifier], n: Int): List[Identifier] =
    (0 until n).map(getStack(frame)).toList

  protected[this] def getTop2Stack(frame: Frame[Identifier]): List[Identifier] =
    getTopNStack(frame, 2)

  protected[this] def getTop3Stack(frame: Frame[Identifier]): List[Identifier] =
    getTopNStack(frame, 3)

  protected[this] def getConstVal(insn: AbstractInsnNode, opcodes: Int): Value = {
    require(opcodes >= 1 && opcodes <= 18)
    // since in ASM LDC also represents opcode LDC_W, LDC2_W in the JVMS,
    // the actually opcodes processed here are in the range of [1, 20].

    // ASM guarantees the `cst` of `LdcInsnNode` will never be `null`.
    //   Since there is an opcode `aconst_null`, I guess that all compilers
    // shouldn't use the opcodes `ldc` or `ldc_w` to process `null` value.  // TODO: is this MANDATORY?!?!?!?
    opcodes match {
      case Opcodes.BIPUSH => BV(insn.asInstanceOf[IntInsnNode].operand.toByte)
      case Opcodes.SIPUSH => SV(insn.asInstanceOf[IntInsnNode].operand.toShort)
      // TODO: https://stackoverflow.com/questions/28264012/im-curious-about-what-ldc-short-for-in-jvm/28268671

      // TODO: check the API and you can find useful info about the type of `cst`:
      // TODO(CONTINUE)   Integer, Float, Long, Double, String, or (ASM) Type

      // TODO: ??? These types are not consistent with the info given in the stackoverflow link above
      case Opcodes.LDC    => LDCVal(insn.asInstanceOf[LdcInsnNode].cst)

      case _              => constantInsnToConst(opcodes)
    }
  }

  protected[this] def getEventually(key: Identifier,
                                    stkMap: Map[Identifier, Value],
                                    locMap: Map[Identifier, Value]): Value = {
    println(s"key: $key")  // Debug message!

    if (locMap.contains(key))
      key
    else
      stkMap(key) match {
        case id: Identifier => getEventually(id, stkMap, locMap)
        case v              => v
      }

  }

  // Example: val desc = "(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;"
  def nParameters(desc: String): Int =
    DescriptorParser.parseMethodDescriptor(desc).get.parameterDescriptors.length

  /** Debug usage */
  def printInsnNode(i: AbstractInsnNode): Unit = {
    val op = org.ucombinator.jade.util.Util.translator(i)

    val message =
    i match {
      case fld:  FieldInsnNode         => s"$op  --  $i -- name: ${fld.name}"
      case iinc: IincInsnNode          => s"$op  --  $i -- var: ${iinc.`var`} -- incr: ${iinc.incr}"
      case _: InsnNode                 => s"$op  --  $i -- NO OPERAND"
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

  def interp(insnFramePairs: List[(AbstractInsnNode, Frame[Identifier])],
             stackMap: Map[Identifier, Value],
             localVariableMap: Map[Identifier, Value]): Unit = {

    println(s"stackMap: $stackMap")
    println(s"localVar: $localVariableMap")
    println("-------------------------------------------------------------------\n\n")

    insnFramePairs match {
      case Nil => // TODO:

      case List(x) => // TODO:
        interp(Nil, stackMap, localVariableMap)

      case (insn, frame) :: (leftInsns@(nextInsn, nextFrame) :: _) =>

        /** Debug print - Start */
        val instruction = org.ucombinator.jade.util.Util.translator(insn)
        println(s"insn: $instruction")
        println(s"nLocals: ${frame.getLocals}")
        println(s"nStack: ${frame.getStackSize}")
        println(s"frame: $frame")

        /** Debug print - End */

        insn.getOpcode match {
          /** Constants: 0 ~ 20 */
          // Opcodes.NOP /* 0 */ => Do nothing

          case c@(
            Opcodes.ACONST_NULL | Opcodes.ICONST_M1 | Opcodes.ICONST_0 | // \
            Opcodes.ICONST_1 | Opcodes.ICONST_2 | Opcodes.ICONST_3 | //  \
            Opcodes.ICONST_4 | Opcodes.ICONST_5 | Opcodes.LCONST_0 | //   --> InsnNode
            Opcodes.LCONST_1 | Opcodes.FCONST_0 | Opcodes.FCONST_1 | //  /
            Opcodes.FCONST_2 | Opcodes.DCONST_0 | Opcodes.DCONST_1 | // /
            Opcodes.BIPUSH | Opcodes.SIPUSH | // ----> IntInsnNode
            Opcodes.LDC) // ----> LdcInsnNode
            // This case also covers:
            // LDC_W (19), LDC2_W (20)
          =>
            val key = getTopStack(nextFrame)
            val value = getConstVal(insn, c)
            interp(leftInsns, stackMap + (key -> value), localVariableMap)

          /** LOADS (Primitives)
            * JVM opcodes: 21 ~ 45 */
          case Opcodes.ILOAD | Opcodes.LLOAD | Opcodes.FLOAD | Opcodes.DLOAD |
               Opcodes.ALOAD // 21 ~ 25
            // Also covers 26 ~ 45:
            // ILOAD_0, ILOAD_1, ILOAD_2, ILOAD_3, LLOAD_0, LLOAD_1, LLOAD_2,
            // LLOAD_3, FLOAD_0, FLOAD_1, FLOAD_2, FLOAD_3, DLOAD_0, DLOAD_1,
            // DLOAD_2, DLOAD_3, ALOAD_0, ALOAD_1, ALOAD_2, ALOAD_3
          =>
            val key = getTopStack(nextFrame)
            val value = {
              val idx = insn.asInstanceOf[VarInsnNode].`var`
              frame.getLocal(idx)
            }
            interp(leftInsns, stackMap + (key -> value), localVariableMap)

          /** LOADS (Arrays)
            * JVM opcodes: 46 ~ 53 */
          case Opcodes.IALOAD | Opcodes.LALOAD | Opcodes.FALOAD |
               Opcodes.DALOAD | Opcodes.AALOAD | Opcodes.BALOAD |
               Opcodes.CALOAD | Opcodes.SALOAD
          =>
            val key = getTopStack(nextFrame)

            val value = {
              val List(index, arrayRef) = getTop2Stack(frame).map(getEventually(_, stackMap, localVariableMap))
              ArrayElementV(arrayRef, index)
            }
            interp(leftInsns, stackMap + (key -> value), localVariableMap)

          /** Stores (Primitives)
            * JVM opcodes: 54 ~ 78 */
          case Opcodes.ISTORE | Opcodes.LSTORE | Opcodes.FSTORE |
               Opcodes.DSTORE | Opcodes.ASTORE // 54 ~ 58
            // Also covers 59 ~ 78:
            // ISTORE_0, ISTORE_1, ISTORE_2, ISTORE_3, LSTORE_0, LSTORE_1,
            // LSTORE_2, LSTORE_3, FSTORE_0, FSTORE_1, FSTORE_2, FSTORE_3,
            // DSTORE_0, DSTORE_1, DSTORE_2, DSTORE_3, ASTORE_0, ASTORE_1,
            // ASTORE_2, ASTORE_3
          =>
            val key = {
              val idx = insn.asInstanceOf[VarInsnNode].`var`
              nextFrame.getLocal(idx)
            }
            val value = getTopStack(frame)
            code += key + " = " + getEventually(value, stackMap, localVariableMap)
            interp(leftInsns, stackMap, localVariableMap + (key -> value))

          /** Stores (Arrays)
            * JVM opcodes: 79 - 86 */
          case Opcodes.IASTORE | Opcodes.LASTORE | Opcodes.FASTORE |
               Opcodes.DASTORE | Opcodes.AASTORE | Opcodes.BASTORE |
               Opcodes.CASTORE | Opcodes.SASTORE // 79 ~ 86
          =>
            val key = {
              val i = insn.asInstanceOf[VarInsnNode].`var`
              nextFrame.getLocal(i)
            }

            val value = {
              val Seq(v, index, arrayRef) = getTop3Stack(frame).map(getEventually(_, stackMap, localVariableMap))
              val result = ArrayElementV(arrayRef.asInstanceOf[Identifier], index)
              code += result + " = " + v // TODO: or `k`
              result
            }
            interp(leftInsns, stackMap, localVariableMap + (key -> value))

          // ---------------------------------------------------------------
          /** Stack */
          case Opcodes.POP | Opcodes.POP2 // 87, 88  // TODO: not sure
          =>
          // The implementation of POP is right if your purpose is to generate compilable code.
          // TODO: However, this implmentation CANNOT recover the original code like:
          //   def void foo() {
          //       Integer ii = Integer.valueOf(3);
          //       Integer jj = ii.valueOf(4);  // the result of decompilation of this line: Integer jj = Integer.valueOf(4);
          //   }

            interp(leftInsns, stackMap, localVariableMap)

          case Opcodes.DUP /* 89 */
          =>
            // TODO: getTop2Stack !!!
            val key = getTopStack(nextFrame)
            val value = getStack(nextFrame)(1)
            interp(leftInsns, stackMap + (key -> value), localVariableMap)

          // TODO:
          //         Opcodes.DUP_X1  /* 90 */

          //         Opcodes.DUP_X2  /* 91 */

          //         Opcodes.DUP2    /* 92 */
          /* com.google.common.cache: LongAdder.add, Striped64$1, Striped64$Cell */
          /**  DenseImmutableTable
               DenseImmutableTable$1
               DenseImmutableTable$Column
               DenseImmutableTable$ColumnMap
               DenseImmutableTable$ImmutableArrayMap$1
               DenseImmutableTable$ImmutableArrayMap
               DenseImmutableTable$Row
               DenseImmutableTable$RowMap */


          //         Opcodes.DUP2_X1 /* 93 */
          /** AbstractMapBasedMultiset
              AbstractMapBasedMultiset$1$1
              AbstractMapBasedMultiset$1
              AbstractMapBasedMultiset$MapBasedMultisetIterator */

          //         Opcodes.DUP2_X2 /* 94 */

          case Opcodes.SWAP /* 95 */ =>  // TODO: ???
            interp(leftInsns, stackMap, localVariableMap)

          // ---------------------------------------------------------------
          /** Math */
          case binaryOp if (binaryOp >= 96 && binaryOp <= 115) || (binaryOp >= 120 && binaryOp <= 132)
          =>
            val key = getTopStack(nextFrame)
            val Seq(i2, i1) = getTop2Stack(frame)
            val v1 = getEventually(i1, stackMap, localVariableMap)
            val v2 = getEventually(i2, stackMap, localVariableMap)

            val value =
              if      (Set(Opcodes.IADD, Opcodes.LADD, Opcodes.FADD, Opcodes.DADD).contains(binaryOp))
                ADD(v1, v2)
              else if (Set(Opcodes.ISUB, Opcodes.LSUB, Opcodes.FSUB, Opcodes.DSUB).contains(binaryOp))
                SUB(v1, v2)
              else if (Set(Opcodes.IMUL, Opcodes.LMUL, Opcodes.FMUL, Opcodes.DMUL).contains(binaryOp))
                MUL(v1, v2)
              else if (Set(Opcodes.IDIV, Opcodes.LDIV, Opcodes.FDIV, Opcodes.DDIV).contains(binaryOp))
                DIV(v1, v2)
              else if (Set(Opcodes.IREM, Opcodes.LREM, Opcodes.FREM, Opcodes.DREM).contains(binaryOp))
                REM(v1, v2)
              else if (Set(Opcodes.ISHL, Opcodes.LSHL).contains(binaryOp))
                SHL(v1, v2)
              else if (Set(Opcodes.ISHR, Opcodes.LSHR).contains(binaryOp))
                SHR(v1, v2)
              else if (Set(Opcodes.IUSHR, Opcodes.LUSHR).contains(binaryOp))
                USHR(v1, v2)
              else if (Set(Opcodes.IAND, Opcodes.LAND).contains(binaryOp))
                AND(v1, v2)
              else if (Set(Opcodes.IOR, Opcodes.LOR).contains(binaryOp))
                OR(v1, v2)
              else if (Set(Opcodes.IXOR, Opcodes.LXOR).contains(binaryOp))
                XOR(v1, v2)
              else  // Opcodes.IINC
                IINC(v1, v2)

            interp(leftInsns, stackMap + (key -> value), localVariableMap)

          case Opcodes.INEG | Opcodes.LNEG | Opcodes.FNEG | Opcodes.DNEG // 116 ~ 119
          =>
            val key = getTopStack(nextFrame)
            val value = {
              val id = getTopStack(frame)
              val v = getEventually(id, stackMap, localVariableMap)
              NEG(v)
            }
            interp(leftInsns, stackMap + (key -> value), localVariableMap)

          // ---------------------------------------------------------------
          /** Cast */
          case cast @ (
            Opcodes.I2L | Opcodes.I2F | Opcodes.I2D | Opcodes.L2I |
            Opcodes.L2F | Opcodes.L2D | Opcodes.F2I | Opcodes.F2L |
            Opcodes.F2D | Opcodes.D2I | Opcodes.D2L | Opcodes.D2F |
            Opcodes.I2B | Opcodes.I2C | Opcodes.I2S
            ) // 133 ~ 147
          =>
            val key = getTopStack(nextFrame)
            val value = {
              val v = getEventually(getTopStack(frame), stackMap, localVariableMap)

              (cast: @unchecked) match {
                case Opcodes.I2L /* 133 */ => IntToLong(v)
                case Opcodes.I2F /* 134 */ => IntToFloat(v)
                case Opcodes.I2D /* 135 */ => IntToDouble(v)
                case Opcodes.L2I /* 136 */ => LongToInt(v)
                case Opcodes.L2F /* 137 */ => LongToFloat(v)
                case Opcodes.L2D /* 138 */ => LongToDouble(v)
                case Opcodes.F2I /* 139 */ => FloatToInt(v)
                case Opcodes.F2L /* 140 */ => FloatToLong(v)
                case Opcodes.F2D /* 141 */ => FloatToDouble(v)
                case Opcodes.D2I /* 142 */ => DoubleToInt(v)
                case Opcodes.D2L /* 143 */ => DoubleToLong(v)
                case Opcodes.D2F /* 144 */ => DoubleToFloat(v)
                case Opcodes.I2B /* 145 */ => IntToByte(v)
                case Opcodes.I2C /* 146 */ => IntToChar(v)
                case Opcodes.I2S /* 147 */ => IntToShort(v)
              }
            }
            interp(leftInsns, stackMap + (key -> value), localVariableMap)

          // TODO:
          /**  Comparisons */

          // TODO:
          /**  Controls */
          //          167 (0xa7) goto
          //          168 (0xa8) jsr
          //          169 (0xa9) ret
          //          170 (0xaa) tableswitch
          //          171 (0xab) lookupswitch

          case Opcodes.IRETURN | Opcodes.LRETURN | Opcodes.FRETURN |
               Opcodes.DRETURN | Opcodes.ARETURN // 172 ~ 177
          =>
            val result = getEventually(getTopStack(frame), stackMap, localVariableMap)
            code += "return" + " " + result
            interp(leftInsns, stackMap, localVariableMap)

          case Opcodes.RETURN /* 177 (0xb1) */ =>
            // TODO: if `return;` at the end, don't generate source code!
            // TODO: if `return;` not at the end, DO generate source code to quit a method!
            code += "return;"
            interp(leftInsns, stackMap, localVariableMap)


          // Developing --------------------------------------------------------------
          /** References */
          case Opcodes.GETSTATIC /* 178 */ =>
            val key = getTopStack(nextFrame)
            val value = {
              val ins = insn.asInstanceOf[FieldInsnNode]
              val desc = DescriptorParser.parseFieldDescriptor(ins.desc).get  // TODO: The returned `desc` may not be the internal form, which make my parser crash!!!!
              StaticFieldV(ins.owner, ins.name, desc)
            }
            interp(leftInsns, stackMap + (key -> value), localVariableMap)

          case Opcodes.PUTSTATIC /* 179 */ =>
            val ins = insn.asInstanceOf[FieldInsnNode]
            val value = getEventually(getTopStack(frame), stackMap, localVariableMap)
            code += s"${ins.owner}.${ins.name} = value"
            // No change for maps -- PUTSTATIC change the source code and then
            // the future computation, but it doesn't change the `stackMap` and the `localVariableMap`
            interp(leftInsns, stackMap, localVariableMap)

          case Opcodes.GETFIELD /* 180 */ =>
            val key = getTopStack(nextFrame)
            val value = {
              val obj = getEventually(getTopStack(frame), stackMap, localVariableMap)
              val ins = insn.asInstanceOf[FieldInsnNode]
              val name = ins.name
              val desc = DescriptorParser.parseFieldDescriptor(ins.name).get
              InstanceFieldV(obj, name, desc)  // TODO: The returned `desc` may not be the internal form, which make my parser crash!!!!
            }
            interp(leftInsns, stackMap + (key -> value), localVariableMap)

          case Opcodes.PUTFIELD /* 181 */ =>
            val ins = insn.asInstanceOf[FieldInsnNode]
            val Seq(value, obj) = getTop2Stack(frame).map(getEventually(_, stackMap, localVariableMap))
            code += obj + "." + ins.name + " = "  + value
            interp(leftInsns, stackMap, localVariableMap)

          case Opcodes.INVOKEVIRTUAL /* 182 */ =>
            val ins = insn.asInstanceOf[MethodInsnNode]
            val parameterCount = nParameters(ins.desc)
            val id = getStack(frame)(parameterCount)
            val key = getTopStack(nextFrame)
            val obj = getEventually(id, stackMap, localVariableMap)
            val parameters = getTopNStack(frame, parameterCount).reverse.map(getEventually(_, stackMap, localVariableMap)).toList
            interp(leftInsns, stackMap + (key -> InvokeVirtualV(obj, ins.name, parameters)), localVariableMap)

          case Opcodes.INVOKESPECIAL /* 183 */ =>
            val ins = insn.asInstanceOf[MethodInsnNode]
            val parameterCount = nParameters(ins.desc)
            val id = getStack(frame)(parameterCount)
            val key = getTopStack(nextFrame)
            val obj = getEventually(id, stackMap, localVariableMap)
            val parameters = getTopNStack(frame, parameterCount).reverse.map(getEventually(_, stackMap, localVariableMap))
            interp(leftInsns, stackMap + (key -> InvokeSpecialV(obj, ins.name, parameters)), localVariableMap)

          case Opcodes.INVOKESTATIC /* 184 (0xb8) */ =>
            val ins = insn.asInstanceOf[MethodInsnNode]
            val parameterCount = nParameters(ins.desc)
            val key = getTopStack(nextFrame)
            val parameters = getTopNStack(frame, parameterCount).reverse.map(getEventually(_, stackMap, localVariableMap))
            interp(leftInsns, stackMap + (key -> InvokeStaticV(ClassV(ins.owner), ins.name, parameters)), localVariableMap)

          case Opcodes.INVOKEINTERFACE /* 185 (0xb9) */ =>
            val ins = insn.asInstanceOf[MethodInsnNode]
            val parameterCount = nParameters(ins.desc)
            val id = getStack(frame)(parameterCount)
            val key = getTopStack(nextFrame)
            val obj = getEventually(id, stackMap, localVariableMap)
            val parameters = getTopNStack(frame, parameterCount).reverse.map(getEventually(_, stackMap, localVariableMap))
            interp(leftInsns, stackMap + (key -> InvokeInterfaceV(obj, ins.name, parameters)), localVariableMap)



          // TODO: ???
          case Opcodes.INVOKEDYNAMIC /* 186 */ =>
            val ins = insn.asInstanceOf[InvokeDynamicInsnNode]
            println(s"desc: ${ins.desc} \n name: ${ins.name} \n bsm: ${ins.bsm} \n bsmArgs: ${ins.bsmArgs}")

            println(s"-----frame------- $frame\n")

            println(s"-----stack------- $stackMap")
            println(s"-----localVar---- $localVariableMap\n")

            println(s"-----nextFrame---- $nextFrame\n")




          case Opcodes.NEW /* 187 (0xbb) */ =>
            val key = getTopStack(nextFrame)
            val value = insn.asInstanceOf[TypeInsnNode].desc
            interp(leftInsns, stackMap + (key -> ClassV(value)), localVariableMap)

          case Opcodes.NEWARRAY /* 188 (0xbc) */ =>
            val key = getTopStack(nextFrame)
            val value = {
              val typeCode = insn.asInstanceOf[IntInsnNode].operand
              val nDimensions = getTopStack(frame)
              val v = getEventually(nDimensions, stackMap, localVariableMap)
              NewPrimitiveArrayV(newArrayMap(typeCode), v)
            }
            interp(leftInsns, stackMap + (key -> value), localVariableMap)

          case Opcodes.ANEWARRAY /* 189 (0xbd) */ =>
            val key = getTopStack(nextFrame)
            val value = {
              val arrLen = getEventually(getTopStack(frame), stackMap, localVariableMap)
              val elementType = DescriptorParser.parseObjectDescriptor(insn.asInstanceOf[TypeInsnNode].desc).get
              NewReferenceArrayV(elementType, arrLen)
            }
            interp(leftInsns, stackMap + (key -> value), localVariableMap)

          case Opcodes.ARRAYLENGTH     /* 190 (0xbe) */ =>
            val key = getTopStack(nextFrame)
            val value = {
              val obj = getEventually(getTopStack(frame), stackMap, localVariableMap)
              ArrayLengthV(obj)
            }
            interp(leftInsns, stackMap + (key -> value), localVariableMap)

          case Opcodes.ATHROW /* 191 (0xbf) */ =>
            code += "throw" + " " + getEventually(getTopStack(frame), stackMap, localVariableMap)
            interp(leftInsns, stackMap, localVariableMap)

          case Opcodes.CHECKCAST /* 192 (0xc0) */ =>  // TODO: similar to `INSTANCEOF`
            // TODO: from the JVMS -- "If objectref is null, then the operand stack is unchanged.".    DO I need to correct this instruction  ???
            val key = getTopStack(nextFrame)
            val value = {
              val obj = getEventually(getTopStack(frame), stackMap, localVariableMap)
              val descriptor = DescriptorParser.parseFieldDescriptor(insn.asInstanceOf[TypeInsnNode].desc).get
              CheckCastV(obj, descriptor)
            }
            interp(leftInsns, stackMap + (key -> value), localVariableMap)

          case Opcodes.INSTANCEOF /* 193 (0xc1) */ =>
            val key = getTopStack(nextFrame)
            val value = {
              val obj = getEventually(getTopStack(frame), stackMap, localVariableMap)
              val descriptor = DescriptorParser.parseFieldDescriptor(insn.asInstanceOf[TypeInsnNode].desc).get
              InstanceOf(obj, descriptor)
            }
            interp(leftInsns, stackMap + (key -> value), localVariableMap)

          case Opcodes.MONITORENTER    /* 194 (0xc2) */ =>
            // TODO:
            println(s"-----frame------- $frame\n")
            println(s"-----stack------- $stackMap")
            println(s"-----localVar---- $localVariableMap\n")
            println(s"-----nextFrame---- $nextFrame\n")

          case Opcodes.MONITOREXIT     /* 195 (0xc3) */ =>
            // TODO:
            println(s"-----frame------- $frame\n")
            println(s"-----stack------- $stackMap")
            println(s"-----localVar---- $localVariableMap\n")
            println(s"-----nextFrame---- $nextFrame\n")

          /** Extended */
          //          case Opcodes.WIDE               /* 196 */ NO =>

          case Opcodes.MULTIANEWARRAY /* 197 */ =>
            val ins = insn.asInstanceOf[MultiANewArrayInsnNode]
            val dims = getTopNStack(frame, ins.dims).map(getEventually(_, stackMap, localVariableMap))
            val key = getTopStack(nextFrame)
            val t = DescriptorParser.parseArrayDescriptor(ins.desc).get.typ
            val value = NewMultiDimArray(t, dims)
            interp(leftInsns, stackMap + (key -> value), localVariableMap)

          case Opcodes.IFNULL             /* 198 */ =>
          case Opcodes.IFNONNULL          /* 199 */ =>

          //          case Opcodes.GOTO_W             /* 200 */  NO =>
          //          case Opcodes.JSR_W              /* 201 */  NO =>

          /** -------------------- DEVELOPING -------------START-------- */
          //            println(s"desc: ${ins.desc}  --  ${ins.dims}")
          //            println(s"-----frame------- $frame\n")
          //            println(s"-----stack------- $stackMap")
          //            println(s"-----localVar---- $localVariableMap\n")
          //            println(s"-----nextFrame---- $nextFrame\n")
          /** -------------------- DEVELOPING --------------END-------- */

          case -1 =>
            interp(leftInsns, stackMap, localVariableMap)

          // TODO: Remove this after finish all bytecode instructions
          case _ /* -1, Opcodes.NOP*/ =>
            interp(leftInsns, stackMap, localVariableMap)
        }

    }
  }

  // FOR Develop
  def main(): Unit
//  def main(): Unit = {
//    val localVariableMap: Map[Identifier, Val] = {
//      val frame = analyzer.frames.head
//      (0 until frame.getLocals).
//        map(frame.getLocal). // Filter non initialized ???
//        map(id => id -> id).
//        toMap
//    }
//
//    println(this.insnFramePairs)
//    interp(insnFramePairs, stackMap = Map.empty[Identifier, Val], localVariableMap)
//    code.foreach(println)
//  }

}
