package org.ucombinator.jade.decompile

import com.github.javaparser.ast.`type`.PrimitiveType
import com.github.javaparser.ast.expr._
import com.github.javaparser.ast.stmt._
import com.github.javaparser.ast.{ArrayCreationLevel, NodeList}
import org.objectweb.asm.Opcodes
import org.objectweb.asm.tree._
import org.ucombinator.jade.util.classfile.Descriptor

sealed trait DecompiledInsn
case class DecompiledStatement(statement: Statement) extends DecompiledInsn
case class DecompiledExpression(expression: Expression) extends DecompiledInsn
case class DecompiledMove(/*TODO*/) extends DecompiledInsn
case class DecompiledIinc(/*TODO*/) extends DecompiledInsn
case class DecompiledCompare(/*TODO*/) extends DecompiledInsn
case class DecompiledIf(/*TODO*/) extends DecompiledInsn
case class DecompiledGoto(/*TODO*/) extends DecompiledInsn
case class DecompiledSwitch(/*TODO*/) extends DecompiledInsn
case class DecompiledCheckCast(/*TODO*/) extends DecompiledInsn
case class DecompiledMonitor(/*TODO*/) extends DecompiledInsn
case class DecompiledUnsupported(/*TODO*/) extends DecompiledInsn

// TODO: typo in Opcodes.java: visiTableSwitchInsn -> visitTableSwitchInsn

object DecompileInsn {
  def decompileInsn(node: AbstractInsnNode): DecompiledInsn = {
    val args: Array[Expression] = ???
    node.getOpcode match {
      // InsnNode
      case Opcodes.NOP => DecompiledStatement(new EmptyStmt())
      case Opcodes.ACONST_NULL => DecompiledExpression(new NullLiteralExpr())
      case Opcodes.ICONST_M1 => DecompiledExpression(new IntegerLiteralExpr(-1))
      case Opcodes.ICONST_0 => DecompiledExpression(new IntegerLiteralExpr(0))
      case Opcodes.ICONST_1 => DecompiledExpression(new IntegerLiteralExpr(1))
      case Opcodes.ICONST_2 => DecompiledExpression(new IntegerLiteralExpr(2))
      case Opcodes.ICONST_3 => DecompiledExpression(new IntegerLiteralExpr(3))
      case Opcodes.ICONST_4 => DecompiledExpression(new IntegerLiteralExpr(4))
      case Opcodes.ICONST_5 => DecompiledExpression(new IntegerLiteralExpr(5))
        // TODO: Research idea distribution of iconst instructions in real code
        // TODO: hex form of constant in comment
      case Opcodes.LCONST_0 => DecompiledExpression(new LongLiteralExpr("0L"))
      case Opcodes.LCONST_1 => DecompiledExpression(new LongLiteralExpr("1L"))
      case Opcodes.FCONST_0 => DecompiledExpression(new DoubleLiteralExpr("0.0F"))
      case Opcodes.FCONST_1 => DecompiledExpression(new DoubleLiteralExpr("1.0F"))
      case Opcodes.FCONST_2 => DecompiledExpression(new DoubleLiteralExpr("2.0F"))
      case Opcodes.DCONST_0 => DecompiledExpression(new DoubleLiteralExpr("0.0D"))
      case Opcodes.DCONST_1 => DecompiledExpression(new DoubleLiteralExpr("1.0D"))
      // IntInsnNode
      case Opcodes.BIPUSH => ???
      case Opcodes.SIPUSH => ???
      // LdcInsnNode
      case Opcodes.LDC =>
        DecompiledExpression(node.asInstanceOf[LdcInsnNode].cst match {
          case cst: java.lang.Integer => new IntegerLiteralExpr(cst)
          case cst: java.lang.Float => new DoubleLiteralExpr(cst.toString + "F") // TODO: float vs double
          case cst: java.lang.Long => new LongLiteralExpr(cst)
          case cst: java.lang.Double => new DoubleLiteralExpr(cst.toString + "D") // TODO: float vs double
          case cst: java.lang.String => new StringLiteralExpr(cst)
          case cst: org.objectweb.asm.Type => new ClassExpr(???)
        })
      // VarInsnNode
      case Opcodes.ILOAD => DecompiledExpression(args(0))
      case Opcodes.LLOAD => DecompiledExpression(args(0))
      case Opcodes.FLOAD => DecompiledExpression(args(0))
      case Opcodes.DLOAD => DecompiledExpression(args(0))
      case Opcodes.ALOAD => DecompiledExpression(args(0))
      // TODO: use `|` patterns
      // InsnNode
      case Opcodes.IALOAD => DecompiledExpression(new ArrayAccessExpr(args(0), args(1)))
      case Opcodes.LALOAD => DecompiledExpression(new ArrayAccessExpr(args(0), args(1)))
      case Opcodes.FALOAD => DecompiledExpression(new ArrayAccessExpr(args(0), args(1)))
      case Opcodes.DALOAD => DecompiledExpression(new ArrayAccessExpr(args(0), args(1)))
      case Opcodes.AALOAD => DecompiledExpression(new ArrayAccessExpr(args(0), args(1)))
      case Opcodes.BALOAD => DecompiledExpression(new ArrayAccessExpr(args(0), args(1)))
      case Opcodes.CALOAD => DecompiledExpression(new ArrayAccessExpr(args(0), args(1)))
      case Opcodes.SALOAD => DecompiledExpression(new ArrayAccessExpr(args(0), args(1)))
      // VarInsnNode
      case Opcodes.ISTORE => DecompiledExpression(args(0))
      case Opcodes.LSTORE => DecompiledExpression(args(0))
      case Opcodes.FSTORE => DecompiledExpression(args(0))
      case Opcodes.DSTORE => DecompiledExpression(args(0))
      case Opcodes.ASTORE => DecompiledExpression(args(0))
      // InsnNode
      case Opcodes.IASTORE => ???
      case Opcodes.LASTORE => ???
      case Opcodes.FASTORE => ???
      case Opcodes.DASTORE => ???
      case Opcodes.AASTORE => ???
      case Opcodes.BASTORE => ???
      case Opcodes.CASTORE => ???
      case Opcodes.SASTORE => ???
      case Opcodes.POP => ???
      case Opcodes.POP2 => ???
      case Opcodes.DUP => ???
      case Opcodes.DUP_X1 => ???
      case Opcodes.DUP_X2 => ???
      case Opcodes.DUP2 => ???
      case Opcodes.DUP2_X1 => ???
      case Opcodes.DUP2_X2 => ???
      case Opcodes.SWAP => ???
      case Opcodes.IADD => DecompiledExpression(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.PLUS))
      case Opcodes.LADD => DecompiledExpression(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.PLUS))
      case Opcodes.FADD => DecompiledExpression(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.PLUS))
      case Opcodes.DADD => DecompiledExpression(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.PLUS))
      case Opcodes.ISUB => DecompiledExpression(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.MINUS))
      case Opcodes.LSUB => DecompiledExpression(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.MINUS))
      case Opcodes.FSUB => DecompiledExpression(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.MINUS))
      case Opcodes.DSUB => DecompiledExpression(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.MINUS))
      case Opcodes.IMUL => DecompiledExpression(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.MULTIPLY))
      case Opcodes.LMUL => DecompiledExpression(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.MULTIPLY))
      case Opcodes.FMUL => DecompiledExpression(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.MULTIPLY))
      case Opcodes.DMUL => DecompiledExpression(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.MULTIPLY))
      case Opcodes.IDIV => DecompiledExpression(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.DIVIDE))
      case Opcodes.LDIV => DecompiledExpression(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.DIVIDE))
      case Opcodes.FDIV => DecompiledExpression(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.DIVIDE))
      case Opcodes.DDIV => DecompiledExpression(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.DIVIDE))
      case Opcodes.IREM => DecompiledExpression(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.REMAINDER))
      case Opcodes.LREM => DecompiledExpression(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.REMAINDER))
      case Opcodes.FREM => DecompiledExpression(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.REMAINDER))
      case Opcodes.DREM => DecompiledExpression(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.REMAINDER))
      // TODO: UnaryExpr.Operator.BITWISE_COMPLEMENT
      case Opcodes.INEG => DecompiledExpression(new UnaryExpr(args(0), UnaryExpr.Operator.MINUS))
      case Opcodes.LNEG => DecompiledExpression(new UnaryExpr(args(0), UnaryExpr.Operator.MINUS))
      case Opcodes.FNEG => DecompiledExpression(new UnaryExpr(args(0), UnaryExpr.Operator.MINUS))
      case Opcodes.DNEG => DecompiledExpression(new UnaryExpr(args(0), UnaryExpr.Operator.MINUS))
      case Opcodes.ISHL  => DecompiledExpression(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.LEFT_SHIFT))
      case Opcodes.LSHL  => DecompiledExpression(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.LEFT_SHIFT))
      case Opcodes.ISHR  => DecompiledExpression(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.SIGNED_RIGHT_SHIFT))
      case Opcodes.LSHR  => DecompiledExpression(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.SIGNED_RIGHT_SHIFT))
      case Opcodes.IUSHR => DecompiledExpression(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.UNSIGNED_RIGHT_SHIFT))
      case Opcodes.LUSHR => DecompiledExpression(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.UNSIGNED_RIGHT_SHIFT))
      case Opcodes.IAND  => DecompiledExpression(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.BINARY_AND))
      case Opcodes.LAND  => DecompiledExpression(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.BINARY_AND))
      case Opcodes.IOR   => DecompiledExpression(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.BINARY_OR))
      case Opcodes.LOR   => DecompiledExpression(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.BINARY_OR))
      case Opcodes.IXOR  => DecompiledExpression(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.XOR))
      case Opcodes.LXOR  => DecompiledExpression(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.XOR))
      // IincInsnNode
      case Opcodes.IINC => ???
      // InsnNode
      case Opcodes.I2L => DecompiledExpression(new CastExpr(PrimitiveType.longType  , args(0)))
      case Opcodes.I2F => DecompiledExpression(new CastExpr(PrimitiveType.floatType , args(0)))
      case Opcodes.I2D => DecompiledExpression(new CastExpr(PrimitiveType.doubleType, args(0)))
      case Opcodes.L2I => DecompiledExpression(new CastExpr(PrimitiveType.intType   , args(0)))
      case Opcodes.L2F => DecompiledExpression(new CastExpr(PrimitiveType.floatType , args(0)))
      case Opcodes.L2D => DecompiledExpression(new CastExpr(PrimitiveType.doubleType, args(0)))
      case Opcodes.F2I => DecompiledExpression(new CastExpr(PrimitiveType.intType   , args(0)))
      case Opcodes.F2L => DecompiledExpression(new CastExpr(PrimitiveType.longType  , args(0)))
      case Opcodes.F2D => DecompiledExpression(new CastExpr(PrimitiveType.doubleType, args(0)))
      case Opcodes.D2I => DecompiledExpression(new CastExpr(PrimitiveType.intType   , args(0)))
      case Opcodes.D2L => DecompiledExpression(new CastExpr(PrimitiveType.longType  , args(0)))
      case Opcodes.D2F => DecompiledExpression(new CastExpr(PrimitiveType.floatType , args(0)))
      case Opcodes.I2B => DecompiledExpression(new CastExpr(PrimitiveType.byteType  , args(0)))
      case Opcodes.I2C => DecompiledExpression(new CastExpr(PrimitiveType.charType  , args(0)))
      case Opcodes.I2S => DecompiledExpression(new CastExpr(PrimitiveType.shortType , args(0)))
      case Opcodes.LCMP => ???
      case Opcodes.FCMPL => ???
      case Opcodes.FCMPG => ???
      case Opcodes.DCMPL => ???
      case Opcodes.DCMPG => ???
      // JumpInsnNode
      case Opcodes.IFEQ => ???
      case Opcodes.IFNE => ???
      case Opcodes.IFLT => ???
      case Opcodes.IFGE => ???
      case Opcodes.IFGT => ???
      case Opcodes.IFLE => ???
      case Opcodes.IF_ICMPEQ => ???
      case Opcodes.IF_ICMPNE => ???
      case Opcodes.IF_ICMPLT => ???
      case Opcodes.IF_ICMPGE => ???
      case Opcodes.IF_ICMPGT => ???
      case Opcodes.IF_ICMPLE => ???
      case Opcodes.IF_ACMPEQ => ???
      case Opcodes.IF_ACMPNE => ???
      case Opcodes.GOTO => ???
      case Opcodes.JSR => throw new Exception(f"unsupported instruction: JSR: $node")
      // VarInsnNode
      case Opcodes.RET => throw new Exception(f"unsupported instruction: RET: $node")
      // TableSwitchInsnNode
      case Opcodes.TABLESWITCH => ???
      // LookupSwitch
      case Opcodes.LOOKUPSWITCH => ???
      // InsnNode
      case Opcodes.IRETURN => DecompiledStatement(new ReturnStmt(args(0)))
      case Opcodes.LRETURN => DecompiledStatement(new ReturnStmt(args(0)))
      case Opcodes.FRETURN => DecompiledStatement(new ReturnStmt(args(0)))
      case Opcodes.DRETURN => DecompiledStatement(new ReturnStmt(args(0)))
      case Opcodes.ARETURN => DecompiledStatement(new ReturnStmt(args(0)))
      case Opcodes.RETURN  => DecompiledStatement(new ReturnStmt(/*Nothing*/))
      // FieldInsnNode
      case Opcodes.GETSTATIC => ???
      case Opcodes.PUTSTATIC => ???
      case Opcodes.GETFIELD => DecompiledExpression(new FieldAccessExpr(args(0), ???, new SimpleName(node.asInstanceOf[FieldInsnNode].name)))
      case Opcodes.PUTFIELD => ???
      // MethodInsnNode
      case Opcodes.INVOKEVIRTUAL   => ??? //DecompiledExpression(new MethodCallExpr(???, ???, ???, ???))
      case Opcodes.INVOKESPECIAL   => ???
      case Opcodes.INVOKESTATIC    => ??? //DecompiledExpression(new MethodCallExpr(???, ???, ???, ???))
      case Opcodes.INVOKEINTERFACE => ??? //DecompiledExpression(new MethodCallExpr(???, ???, ???, ???))
      // InvokeDynamicInsnNode
      case Opcodes.INVOKEDYNAMIC => ???
      // TypeInsnNode
      case Opcodes.NEW => ???
      // IntInsnNode
      case Opcodes.NEWARRAY  =>
        val typ = node.asInstanceOf[IntInsnNode].operand match {
          case Opcodes.T_BOOLEAN => PrimitiveType.booleanType()
          case Opcodes.T_CHAR    => PrimitiveType.charType()
          case Opcodes.T_FLOAT   => PrimitiveType.floatType()
          case Opcodes.T_DOUBLE  => PrimitiveType.doubleType()
          case Opcodes.T_BYTE    => PrimitiveType.byteType()
          case Opcodes.T_SHORT   => PrimitiveType.shortType()
          case Opcodes.T_INT     => PrimitiveType.intType()
          case Opcodes.T_LONG    => PrimitiveType.longType()
        }
        DecompiledExpression(new ArrayCreationExpr(typ, new NodeList(new ArrayCreationLevel(args(0), new NodeList())), new ArrayInitializerExpr()))
      // TypeInsnNode
      case Opcodes.ANEWARRAY =>
        val typ = Descriptor.fieldDescriptor(asInstanceOf[TypeInsnNode].desc)
        DecompiledExpression(new ArrayCreationExpr(typ, new NodeList(new ArrayCreationLevel(args(0), new NodeList())), new ArrayInitializerExpr()))
      // InsnNode
      case Opcodes.ARRAYLENGTH => ???
      case Opcodes.ATHROW => DecompiledStatement(new ThrowStmt(args(0)))
      // TypeInsnNode
      case Opcodes.CHECKCAST => ??? // TODO: figure out: DecompiledExpression(new CastExpr(???, ???))
      case Opcodes.INSTANCEOF => DecompiledExpression(new InstanceOfExpr(args(0), Descriptor.classNameType(node.asInstanceOf[TypeInsnNode].desc)))
      // InsnNode
      case Opcodes.MONITORENTER => ???
      case Opcodes.MONITOREXIT => ???
      // MultiANewArrayInsnNode
      case Opcodes.MULTIANEWARRAY => DecompiledExpression(new ArrayCreationExpr(???, ???, ???))
      // JumpInsnNode
      case Opcodes.IFNULL => ???
      case Opcodes.IFNONNULL => ???
      // Synthetic instructions
      case _ =>
        node match {
          case node: LabelNode =>
            DecompiledStatement(new LabeledStmt(new SimpleName(node.getLabel.toString), /*TODO*/new EmptyStmt()))
          case node: FrameNode => ???
          case node: LineNumberNode => ???
          case _ => throw new Exception(f"unknown instruction type: $node")
        }
    }
  }
}
