package org.ucombinator.jade.method

import org.objectweb.asm.Opcodes
import org.objectweb.asm.tree._
import org.objectweb.asm.tree.analysis.{Frame, BasicValue => AsmBasicValue}
import org.ucombinator.jade.jvm.classfile.descriptor.DescriptorParser
import org.ucombinator.jade.ir._
import org.ucombinator.jade.method.frame.RichFrameOperations
import org.ucombinator.jade.util.DebugUtil.printInsnNode

import scala.annotation.tailrec


// TODO: abstract here is added for testing:
// TODO (continue): in Scala worksheet, I always extends this abstract class, and finish the `interp` method.
abstract class BytecodeInterpreter(insnFramePairs: List[(AbstractInsnNode, Frame[Identifier])])
    extends IRGenerator with RichFrameOperations {

  import IRGeneratorUtil._

  require(insnFramePairs.length >= 2)

  val localVariableMap: Map[Identifier, Value] =
    insnFramePairs.head match {
      case (_, frame) =>
        (0 until frame.getLocals).map(frame.getLocal).
          filter(_.basicValue != AsmBasicValue.UNINITIALIZED_VALUE).
          map(id => id -> id).toMap
  }

  final def getInvokeValue(methodInsn: MethodInsnNode,
                           frame: Frame[Identifier],
                           stackMap: Map[Identifier, Value] = Map.empty[Identifier, Value],
                           localVariableMap: Map[Identifier, Value]): Value = {
    val parameterCount = nParameters(methodInsn.desc)
    val parameters = topNStackValues(frame, parameterCount, stackMap, localVariableMap).reverse
    val opcode = methodInsn.getOpcode

    opcode match {
      case Opcodes.INVOKEVIRTUAL | Opcodes.INVOKESPECIAL |
           Opcodes.INVOKEINTERFACE
      =>
        val obj = nthStackValue(frame, parameterCount, stackMap, localVariableMap)
        InvokeValue.of(opcode)(obj, methodInsn.name, parameters)

      case Opcodes.INVOKESTATIC
      =>
        val cls = descString2ClassV(methodInsn.owner)
        InvokeValue.of(opcode)(cls, methodInsn.name, parameters)
    }
  }

  @tailrec
  final def interp(insnFramePairs: List[(AbstractInsnNode, Frame[Identifier])],
                   stackMap: Map[Identifier, Value] = Map.empty[Identifier, Value],
                   localVariableMap: Map[Identifier, Value]): Unit = {

    println("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
    println(s"stackMap: $stackMap\nlocalVar: $localVariableMap\n\n")

    insnFramePairs match {
      case Nil | List(_)=>

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
            val value = topStackValue(frame, stackMap, localVariableMap)
            code += s"$key = $value"
            interp(leftInsns, stackMap, localVariableMap + (key -> value))

          /** Stores (Arrays)
            * JVM opcodes: 79 - 86 */
          case Opcodes.IASTORE | Opcodes.LASTORE | Opcodes.FASTORE |
               Opcodes.DASTORE | Opcodes.AASTORE | Opcodes.BASTORE |
               Opcodes.CASTORE | Opcodes.SASTORE // 79 ~ 86
          =>
            val key = nthLocalVariable(nextFrame, index = insn.asInstanceOf[VarInsnNode].`var`)
            val List(v, index, arrayRef) = top3StackValues(frame, stackMap, localVariableMap)
            val value = ArrayElementV(arrayRef, index)
            code += s"$value = $v"
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
            interp(leftInsns, stackMap, localVariableMap)

          case  Opcodes.POP2 /* 88 */ => // POP2 is a little bit tricky, please check the JVMS!
            topStack(frame) match {
              case Identifier(_, _, AsmBasicValue.LONG_VALUE) | Identifier(_, _, AsmBasicValue.DOUBLE_VALUE)
              =>
                interp(leftInsns, stackMap, localVariableMap)

              case _
              =>
                // TODO: When the Current Stack Top is not of type `long` or `double`, pop2 operate two stacks, and
                // TODO(continue): I may need to do something here!
                interp(leftInsns, stackMap, localVariableMap)
            }

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

          case Opcodes.SWAP /* 95 */ =>
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
              case Opcodes.IADD  | Opcodes.LADD | Opcodes.FADD | Opcodes.DADD => AddV(v1, v2)
              case Opcodes.ISUB  | Opcodes.LSUB | Opcodes.FSUB | Opcodes.DSUB => SubV(v1, v2)
              case Opcodes.IMUL  | Opcodes.LMUL | Opcodes.FMUL | Opcodes.DMUL => MulV(v1, v2)
              case Opcodes.IDIV  | Opcodes.LDIV | Opcodes.FDIV | Opcodes.DDIV => DivV(v1, v2)
              case Opcodes.IREM  | Opcodes.LREM | Opcodes.FREM | Opcodes.DREM => RemV(v1, v2)
              case Opcodes.ISHL  | Opcodes.LSHL                               => ShlV(v1, v2)
              case Opcodes.ISHR  | Opcodes.LSHR                               => ShrV(v1, v2)
              case Opcodes.IUSHR | Opcodes.LUSHR                              => UshrV(v1, v2)
              case Opcodes.IAND  | Opcodes.LAND                               => AndV(v1, v2)
              case Opcodes.IOR   | Opcodes.LOR                                => OrV(v1, v2)
              case Opcodes.IXOR  | Opcodes.LXOR                               => XorV(v1, v2)
              case Opcodes.IINC                                               => IincV(v1, v2)
            }
            interp(leftInsns, stackMap + (key -> value), localVariableMap)

          case Opcodes.INEG | Opcodes.LNEG | Opcodes.FNEG | Opcodes.DNEG // 116 ~ 119
          =>
            val key = topStack(nextFrame)
            val value = NegV(topStackValue(frame, stackMap, localVariableMap))
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
                case Opcodes.I2L /* 133 */ => I2JV(v)
                case Opcodes.I2F /* 134 */ => I2FV(v)
                case Opcodes.I2D /* 135 */ => I2DV(v)
                case Opcodes.L2I /* 136 */ => L2IV(v)
                case Opcodes.L2F /* 137 */ => L2FV(v)
                case Opcodes.L2D /* 138 */ => L2DV(v)
                case Opcodes.F2I /* 139 */ => F2IV(v)
                case Opcodes.F2L /* 140 */ => F2LV(v)
                case Opcodes.F2D /* 141 */ => F2DV(v)
                case Opcodes.D2I /* 142 */ => D2IV(v)
                case Opcodes.D2L /* 143 */ => D2LV(v)
                case Opcodes.D2F /* 144 */ => D2FV(v)
                case Opcodes.I2B /* 145 */ => I2BV(v)
                case Opcodes.I2C /* 146 */ => I2CV(v)
                case Opcodes.I2S /* 147 */ => I2SV(v)
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
            // If a call for side effect happens just before before the end of a
            // void return value method, the value will be dropped, but no `pop`
            // used! Copy the expression below from `pop` to here.
            code += "return;"
            interp(leftInsns, stackMap, localVariableMap)


          /** References */
          case Opcodes.GETSTATIC /* 178 */ =>
            val key = topStack(nextFrame)
            val value = {
              val ins = insn.asInstanceOf[FieldInsnNode]
              // TODO: The returned `desc` may not be the internal form, which make my parser crash!!!!
              val desc = DescriptorParser.parseFieldDescriptor(ins.desc).get
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

          /* 181 ~ 184 */
          case Opcodes.INVOKEVIRTUAL | Opcodes.INVOKESPECIAL   |
               Opcodes.INVOKESTATIC  | Opcodes.INVOKEINTERFACE
          =>
            val value = getInvokeValue(insn.asInstanceOf[MethodInsnNode], frame, stackMap, localVariableMap)
            val isOnlyForSideEffect = {
              val isEmptyStack = nextFrame.getStackSize == 0
              val isDroppedValue = nextFrame.getStackSize == 1 &&
                (nextInsn.getOpcode == Opcodes.POP || nextInsn.getOpcode == Opcodes.POP2)
              // POP2 is a little bit tricky, please check the JVMS to understand this part!
              isEmptyStack && isDroppedValue
            }

            if (isOnlyForSideEffect) {
              code += value.toString
              interp(leftInsns, stackMap, localVariableMap)
            } else {
              val key = topStack(nextFrame)
              interp(leftInsns, stackMap + (key -> value), localVariableMap)
            }

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
            val value = {
              val objectType = DescriptorParser.parseObjectDescriptor("L" + insn.asInstanceOf[TypeInsnNode].desc + ";").get
              ClassV(objectType)
            }
            interp(leftInsns, stackMap + (key -> value), localVariableMap)  // TODO: Use `Descriptor` ???

          case Opcodes.NEWARRAY /* 188 (0xbc) */ =>
            val key = topStack(nextFrame)
            val value = {
              val typeCode = insn.asInstanceOf[IntInsnNode].operand
              val nDimensions = topStackValue(frame, stackMap, localVariableMap)
              PrimitiveArrayV(primitiveArrayElementType(typeCode), nDimensions)
            }
            interp(leftInsns, stackMap + (key -> value), localVariableMap)

          case Opcodes.ANEWARRAY /* 189 (0xbd) */ =>
            val key = topStack(nextFrame)
            val value = {
              val arrLen = topStackValue(frame, stackMap, localVariableMap)
              val elementType = DescriptorParser.parseObjectDescriptor(insn.asInstanceOf[TypeInsnNode].desc).get
              ReferenceArrayV(elementType, arrLen)
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
          // case WIDE               /* 196 */ NO =>

          case Opcodes.MULTIANEWARRAY /* 197 */ =>
            val ins = insn.asInstanceOf[MultiANewArrayInsnNode]
            val key = topStack(nextFrame)
            val value = {
              val t = DescriptorParser.parseArrayDescriptor(ins.desc).get.typ
              val dims = topNStackValues(frame, ins.dims, stackMap, localVariableMap)
              MultiDimensionArrayV(t, dims)
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
// = {
//    interp(insnFramePairs, localVariableMap = localVariableMap)
//    code.foreach(println)
//  }

}
