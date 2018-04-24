package org.ucombinator.jade.method

import org.objectweb.asm.{ClassReader, Opcodes, Type}
import org.objectweb.asm.tree._
import org.objectweb.asm.tree.analysis.{Frame, BasicValue => AsmBasicValue}
import org.ucombinator.jade.jvm.classfile.TypeCommons._
import org.ucombinator.jade.jvm.classfile.descriptor.DescriptorParser
import org.ucombinator.jade.method.bytecode._
import org.ucombinator.jade.method.bytecode.MathByteCode._
import org.ucombinator.jade.Util.printInsnNode

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.collection.mutable.ListBuffer


// TODO: abstract here is added for testing: in Scala worksheet, I always extends this abstract class, and finish the `interp` method.
abstract class BytecodeInterpreter(bytes: Array[Byte], methodName: String) extends FrameOperations {

  protected[this] val cn = new ClassNode
  protected[this] val cr = new ClassReader(bytes)
  cr.accept(cn, 0)
  val method: MethodNode = cn.methods.asScala.filter(_.name == methodName).head  // TODO: for develop
  val analyzer = new org.ucombinator.jade.method.IdentifierAnalyzer(cn.name, method)
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

  protected[this] val primitiveArrayElementType = Map(
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

  final protected[this] def getConstVal(insn: IntInsnNode): Value =
    (insn.getOpcode: @unchecked) match {
      case Opcodes.BIPUSH => BV(insn.operand.toByte)
      case Opcodes.SIPUSH => SV(insn.operand.toShort)
    }

  final protected[this] def getConstVal(insn: LdcInsnNode): Value =
    (insn.cst: @unchecked) match {
      case i: java.lang.Integer => IV(i)
      case f: java.lang.Float   => FV(f)
      case j: java.lang.Long    => JV(j)
      case d: java.lang.Double  => DV(d)
      case _: java.lang.String  => Class(DescriptorParser.parseReferenceDescriptor("Ljava/lang/String;").get)
      case t: Type /* ASM) */   => Class(DescriptorParser.parseReferenceDescriptor(t.getDescriptor).get)
      // TODO: https://stackoverflow.com/questions/28264012/im-curious-about-what-ldc-short-for-in-jvm/28268671
      // `ldc` can load `java.lang.invoke.MethodType` and `java.lang.invoke.MethodHandle`, but this is NOT for
      // Java code.
      // TODO: This may change in the future! If this changes, the actual type of `cst` will also change.
    }

//  final protected[this] def getConstVal(insn: AbstractInsnNode): Value =
//    (insn.getOpcode: @unchecked) match {
//      case Opcodes.BIPUSH =>
//        BV(insn.asInstanceOf[IntInsnNode].operand.toByte)
//
//      case Opcodes.SIPUSH =>
//        SV(insn.asInstanceOf[IntInsnNode].operand.toShort)
//
//      case Opcodes.LDC =>
//        val rawValue = insn.asInstanceOf[LdcInsnNode].cst // Integer, Float, Long, Double, String, or (ASM) Type
//        (rawValue: @unchecked) match {
//          case i: java.lang.Integer => IV(i)
//          case f: java.lang.Float   => FV(f)
//          case j: java.lang.Long    => JV(j)
//          case d: java.lang.Double  => DV(d)
//          case _: java.lang.String  => Class(DescriptorParser.parseReferenceDescriptor("Ljava/lang/String;").get)
//          case t: Type              => Class(DescriptorParser.parseReferenceDescriptor(t.getDescriptor).get)
//          // TODO: https://stackoverflow.com/questions/28264012/im-curious-about-what-ldc-short-for-in-jvm/28268671
//          // `ldc` can load `java.lang.invoke.MethodType` and `java.lang.invoke.MethodHandle`, but this is NOT for
//          // Java code.
//          // TODO: This may change in the future! If this changes, the actual type of `cst` will also change.
//        }
//    }

  // Example: val desc = "(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;"
  final protected[this] def nParameters(desc: String): Int =
    DescriptorParser.parseMethodDescriptor(desc).get.parameterDescriptors.length

  @tailrec
  final def interp(insnFramePairs: List[(AbstractInsnNode, Frame[Identifier])],
                   stackMap: Map[Identifier, Value] = Map.empty[Identifier, Value],
                   localVariableMap: Map[Identifier, Value]): Unit = {

    println(s"stackMap: $stackMap\nlocalVar: $localVariableMap\n\n")

    insnFramePairs match {
      case Nil =>

      case List(_) => // TODO:
        interp(Nil, stackMap, localVariableMap)

      case (insn, frame) :: (leftInsns@(nextInsn, nextFrame) :: _) =>

        /** Debug print - Start */
        printInsnNode(insn)
        println(s"nLocals: ${frame.getLocals}")
        println(s"nStack: ${frame.getStackSize}")
        println(s"frame: $frame")
        println("--------------------------------------------------------------------------")

        /** Debug print - End */

        insn.getOpcode match {
          /** Constants: 0 ~ 20 */
          // Opcodes.NOP /* 0 */ => Do nothing
          case c @ (
            Opcodes.ACONST_NULL | Opcodes.ICONST_M1 | Opcodes.ICONST_0 |
            Opcodes.ICONST_1    | Opcodes.ICONST_2  | Opcodes.ICONST_3 |
            Opcodes.ICONST_4    | Opcodes.ICONST_5  | Opcodes.LCONST_0 |
            Opcodes.LCONST_1    | Opcodes.FCONST_0  | Opcodes.FCONST_1 |
            Opcodes.FCONST_2    | Opcodes.DCONST_0  | Opcodes.DCONST_1
            )
          =>
            val key = topStack(nextFrame)
            val value = constantInsnToConst(c)
            interp(leftInsns, stackMap + (key -> value), localVariableMap)

          case Opcodes.BIPUSH | Opcodes.SIPUSH =>
            val key = topStack(nextFrame)
            val value = getConstVal(insn.asInstanceOf[IntInsnNode])
            interp(leftInsns, stackMap + (key -> value), localVariableMap)

          case Opcodes.LDC =>
            val key = topStack(nextFrame)
            val value = getConstVal(insn.asInstanceOf[LdcInsnNode])
            interp(leftInsns, stackMap + (key -> value), localVariableMap)

          /** LOADS (Primitives): 21 ~ 45 */
          case Opcodes.ILOAD | Opcodes.LLOAD | Opcodes.FLOAD | Opcodes.DLOAD |
               Opcodes.ALOAD // 21 ~ 25
            // Also covers 26 ~ 45:
            // ILOAD_0, ILOAD_1, ILOAD_2, ILOAD_3, LLOAD_0, LLOAD_1, LLOAD_2,
            // LLOAD_3, FLOAD_0, FLOAD_1, FLOAD_2, FLOAD_3, DLOAD_0, DLOAD_1,
            // DLOAD_2, DLOAD_3, ALOAD_0, ALOAD_1, ALOAD_2, ALOAD_3
          =>
            val key = topStack(nextFrame)
            val value = nthLocalVariable(frame, index = insn.asInstanceOf[VarInsnNode].`var`)
            interp(leftInsns, stackMap + (key -> value), localVariableMap)

          /** LOADS (Arrays): 46 ~ 53 */
          case Opcodes.IALOAD | Opcodes.LALOAD | Opcodes.FALOAD |
               Opcodes.DALOAD | Opcodes.AALOAD | Opcodes.BALOAD |
               Opcodes.CALOAD | Opcodes.SALOAD
          =>
            val key = topStack(nextFrame)
            val value = top2StackValues(frame, stackMap, localVariableMap) match {
              case List(index, arrayRef) => ArrayElementV(arrayRef, index)
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
            val key = nthLocalVariable(nextFrame, index = insn.asInstanceOf[VarInsnNode].`var`)
            // TODO: High priority!!!
            // TODO: Which one is right? The old one? OR the new one?
//            val value = getTopStack(frame)
//            code += key + " = " + getEventually(value, stackMap, localVariableMap)
//            interp(leftInsns, stackMap, localVariableMap + (key -> value))
            val value = topStackValue(frame, stackMap, localVariableMap)
            interp(leftInsns, stackMap, localVariableMap + (key -> value))

          /** Stores (Arrays)
            * JVM opcodes: 79 - 86 */
          case Opcodes.IASTORE | Opcodes.LASTORE | Opcodes.FASTORE |
               Opcodes.DASTORE | Opcodes.AASTORE | Opcodes.BASTORE |
               Opcodes.CASTORE | Opcodes.SASTORE // 79 ~ 86
          =>
            val key = nthLocalVariable(nextFrame, index = insn.asInstanceOf[VarInsnNode].`var`)
            val value = top3StackValues(frame, stackMap, localVariableMap) match {
              case List(v, index, arrayRef) =>
                val result = ArrayElementV(arrayRef, index)
                code += result + " = " + v // TODO: or `k`
                result
            }
            interp(leftInsns, stackMap, localVariableMap + (key -> value))

          // ---------------------------------------------------------------
          /** Stack */
          case Opcodes.POP /* 87 */ =>
            // The implementation of POP is right if your purpose is to generate compilable code IR.
            // TODO (Lowest priority): CANNOT recover the original code like
            //   def void foo() {
            //       Integer ii = Integer.valueOf(3);
            //       Integer jj = ii.valueOf(4);  // the result of decompilation of this line: Integer jj = Integer.valueOf(4);
            //   }
            topStackValue(frame, stackMap, localVariableMap) match {
              case _: Identifier =>
              case v             => code += v.toString
            }
            interp(leftInsns, stackMap, localVariableMap)

          case  Opcodes.POP2 /* 88 */ =>
            // TODO: DO something similar as Opcodes.POP. Find an example and figure out the details of what to do.
            interp(leftInsns, stackMap, localVariableMap)

          case Opcodes.DUP /* 89 */
          =>
            val Seq(key, value) = top2Stacks(nextFrame)
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
          case binaryOp
            if (binaryOp >= Opcodes.IADD && binaryOp <= Opcodes.DREM) ||
              (binaryOp >= Opcodes.ISHL && binaryOp <= Opcodes.IINC)
          =>
            val key = topStack(nextFrame)
            val Seq(v2, v1) = top2StackValues(frame, stackMap, localVariableMap)
            val value = (binaryOp : @unchecked) match {
              // TODO: Not sure if we need finer-grain. For instance, use case classes like `IADD` rather than `ADD`
              case Opcodes.IADD  | Opcodes.LADD | Opcodes.FADD | Opcodes.DADD => ADD(v1, v2)
              case Opcodes.ISUB  | Opcodes.LSUB | Opcodes.FSUB | Opcodes.DSUB => SUB(v1, v2)
              case Opcodes.IMUL  | Opcodes.LMUL | Opcodes.FMUL | Opcodes.DMUL => MUL(v1, v2)
              case Opcodes.IDIV  | Opcodes.LDIV | Opcodes.FDIV | Opcodes.DDIV => DIV(v1, v2)
              case Opcodes.IREM  | Opcodes.LREM | Opcodes.FREM | Opcodes.DREM => REM(v1, v2)
              case Opcodes.ISHL  | Opcodes.LSHL                               => SHL(v1, v2)
              case Opcodes.ISHR  | Opcodes.LSHR                               => SHR(v1, v2)
              case Opcodes.IUSHR | Opcodes.LUSHR                              => USHR(v1, v2)
              case Opcodes.IAND  | Opcodes.LAND                               => AND(v1, v2)
              case Opcodes.IOR   | Opcodes.LOR                                => OR(v1, v2)
              case Opcodes.IXOR  | Opcodes.LXOR                               => XOR(v1, v2)
              case Opcodes.IINC                                               => IINC(v1, v2)
            }
            interp(leftInsns, stackMap + (key -> value), localVariableMap)

          case Opcodes.INEG | Opcodes.LNEG | Opcodes.FNEG | Opcodes.DNEG // 116 ~ 119
          =>
            val key = topStack(nextFrame)
            val value = NEG(topStackValue(frame, stackMap, localVariableMap))
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
            val key = topStack(nextFrame)
            val value = {
              val v = topStackValue(frame, stackMap, localVariableMap)

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
            code += "return" + " " + topStackValue(frame, stackMap, localVariableMap)
            interp(leftInsns, stackMap, localVariableMap)

          case Opcodes.RETURN /* 177 */ =>
            // TODO: if `return;` at the end, don't generate source code!
            // TODO: if `return;` not at the end, DO generate source code to quit a method!
            code += "return;"
            interp(leftInsns, stackMap, localVariableMap)


          /** References */
          case Opcodes.GETSTATIC /* 178 */ =>
            val key = topStack(nextFrame)
            val value = {
              val ins = insn.asInstanceOf[FieldInsnNode]
              val desc = DescriptorParser.parseFieldDescriptor(ins.desc).get  // TODO: The returned `desc` may not be the internal form, which make my parser crash!!!!
              StaticFieldV(ins.owner, ins.name, desc)
            }
            interp(leftInsns, stackMap + (key -> value), localVariableMap)

          case Opcodes.PUTSTATIC /* 179 */ =>
            val ins = insn.asInstanceOf[FieldInsnNode]
            val value = topStackValue(frame, stackMap, localVariableMap)
            code += s"${ins.owner}.${ins.name} = $value"
            // No change for maps -- PUTSTATIC change the source code and then
            // the future computation, but it doesn't change the `stackMap` and the `localVariableMap`
            interp(leftInsns, stackMap, localVariableMap)

          case Opcodes.GETFIELD  /* 180 */ =>
            val key = topStack(nextFrame)
            val value = {
              val obj = topStackValue(frame, stackMap, localVariableMap)
              val ins = insn.asInstanceOf[FieldInsnNode]
              val desc = DescriptorParser.parseFieldDescriptor(ins.name).get
              // TODO: The returned `desc` may not be the internal form, which make my parser crash!!!!
              InstanceFieldV(obj, ins.name, desc)
            }
            interp(leftInsns, stackMap + (key -> value), localVariableMap)

          case Opcodes.PUTFIELD  /* 181 */ =>
            val ins = insn.asInstanceOf[FieldInsnNode]
            top2StackValues(frame, stackMap, localVariableMap) match {
              case List(value, obj) => code += s"$obj.${ins.name} = $value"
            }
            interp(leftInsns, stackMap, localVariableMap)

          case Opcodes.INVOKEVIRTUAL /* 182 */ =>  // TODO: Simplify this part!
            val ins = insn.asInstanceOf[MethodInsnNode]
            val parameterCount = nParameters(ins.desc)
            val id = nthStack(frame)(parameterCount)
            val key = topStack(nextFrame)
            val obj = getEventually(id, stackMap, localVariableMap)
            val parameters = topNStackValues(frame, parameterCount, stackMap, localVariableMap).reverse
            interp(leftInsns, stackMap + (key -> InvokeVirtualV(obj, ins.name, parameters)), localVariableMap)

          case Opcodes.INVOKESPECIAL /* 183 */ =>  // TODO: Simplify this part!
            val ins = insn.asInstanceOf[MethodInsnNode]
            val parameterCount = nParameters(ins.desc)
            val id = nthStack(frame)(parameterCount)
            val key = topStack(nextFrame)
            val obj = getEventually(id, stackMap, localVariableMap)
            val parameters = topNStackValues(frame, parameterCount, stackMap, localVariableMap).reverse
            // TODO: This `code` line may not be useful! We need a better one!
            code += InvokeSpecialV(obj, ins.name, parameters).toString
            interp(leftInsns, stackMap + (key -> InvokeSpecialV(obj, ins.name, parameters)), localVariableMap)

          case Opcodes.INVOKESTATIC /* 184 */ =>  // TODO: Simplify this part!
            val ins = insn.asInstanceOf[MethodInsnNode]
            val parameterCount = nParameters(ins.desc)
            val key = topStack(nextFrame)
            val parameters = topNStackValues(frame, parameterCount, stackMap, localVariableMap).reverse
            interp(leftInsns, stackMap + (key -> InvokeStaticV(ClassV(ins.owner), ins.name, parameters)), localVariableMap)

          case Opcodes.INVOKEINTERFACE /* 185 */ =>  // TODO: Simplify this part!
            val ins = insn.asInstanceOf[MethodInsnNode]
            val parameterCount = nParameters(ins.desc)
            val id = nthStack(frame)(parameterCount)
            val key = topStack(nextFrame)
            val obj = getEventually(id, stackMap, localVariableMap)
            val parameters = topNStackValues(frame, parameterCount, stackMap, localVariableMap).reverse
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
            val key = topStack(nextFrame)
            val value = insn.asInstanceOf[TypeInsnNode].desc
            interp(leftInsns, stackMap + (key -> ClassV(value)), localVariableMap)  // TODO: Use `Descriptor` ???

          case Opcodes.NEWARRAY /* 188 (0xbc) */ =>
            val key = topStack(nextFrame)
            val value = {
              val typeCode = insn.asInstanceOf[IntInsnNode].operand
              val nDimensions = topStackValue(frame, stackMap, localVariableMap)
              NewPrimitiveArrayV(primitiveArrayElementType(typeCode), nDimensions)
            }
            interp(leftInsns, stackMap + (key -> value), localVariableMap)

          case Opcodes.ANEWARRAY /* 189 (0xbd) */ =>
            val key = topStack(nextFrame)
            val value = {
              val arrLen = topStackValue(frame, stackMap, localVariableMap)
              val elementType = DescriptorParser.parseObjectDescriptor(insn.asInstanceOf[TypeInsnNode].desc).get
              NewReferenceArrayV(elementType, arrLen)
            }
            interp(leftInsns, stackMap + (key -> value), localVariableMap)

          case Opcodes.ARRAYLENGTH     /* 190 (0xbe) */ =>
            val key = topStack(nextFrame)
            val value = ArrayLengthV(topStackValue(frame, stackMap, localVariableMap))
            interp(leftInsns, stackMap + (key -> value), localVariableMap)

          case Opcodes.ATHROW /* 191 (0xbf) */ =>
            code += s"throw ${topStackValue(frame, stackMap, localVariableMap)}"
            interp(leftInsns, stackMap, localVariableMap)

          case Opcodes.CHECKCAST /* 192 (0xc0) */ =>  // TODO: similar to `INSTANCEOF`
            // TODO: from the JVMS -- "If objectref is null, then the operand stack is unchanged.".    DO I need to correct this instruction  ???
            val key = topStack(nextFrame)
            val value = {
              val obj = topStackValue(frame, stackMap, localVariableMap)
              val descriptor = DescriptorParser.parseFieldDescriptor(insn.asInstanceOf[TypeInsnNode].desc).get
              CheckCastV(obj, descriptor)
            }
            interp(leftInsns, stackMap + (key -> value), localVariableMap)

          case Opcodes.INSTANCEOF /* 193 (0xc1) */ =>
            val key = topStack(nextFrame)
            val value = {
              val obj = topStackValue(frame, stackMap, localVariableMap)
              val descriptor = DescriptorParser.parseFieldDescriptor(insn.asInstanceOf[TypeInsnNode].desc).get
              InstanceOfV(obj, descriptor)
            }
            interp(leftInsns, stackMap + (key -> value), localVariableMap)

          case Opcodes.MONITORENTER  /* 194 */ =>

          case Opcodes.MONITOREXIT  /* 195 */ =>

          /** Extended */
          //          case Opcodes.WIDE               /* 196 */ NO =>

          case Opcodes.MULTIANEWARRAY /* 197 */ =>
            val ins = insn.asInstanceOf[MultiANewArrayInsnNode]
            val key = topStack(nextFrame)
            val value = {
              val t = DescriptorParser.parseArrayDescriptor(ins.desc).get.typ
              val dims = topNStackValues(frame, ins.dims, stackMap, localVariableMap)
              NewMultiDimArray(t, dims)
            }
            interp(leftInsns, stackMap + (key -> value), localVariableMap)

          case Opcodes.IFNULL  /* 198 */ => // TODO:

          case Opcodes.IFNONNULL  /* 199 */ => // TODO:

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

  /** FOR Develop */
  def main(): Unit
//  def main(): Unit = {
//    val localVariableMap: Map[Identifier, Value] = {
//      val frame = analyzer.frames.head
//      (0 until frame.getLocals).
//        map(frame.getLocal).
//        filter(_.basicValue != AsmBasicValue.UNINITIALIZED_VALUE).
//        map(id => id -> id).
//        toMap
//    }
//
//    println(this.insnFramePairs)
//    interp(insnFramePairs, localVariableMap = localVariableMap)
//    code.foreach(println)
//  }

}
