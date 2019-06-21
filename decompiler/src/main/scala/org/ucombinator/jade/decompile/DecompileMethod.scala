package org.ucombinator.jade.decompile

import com.github.javaparser.ast.`type`.PrimitiveType
import com.github.javaparser.ast.expr._
import com.github.javaparser.ast.stmt._
import com.github.javaparser.ast.{ArrayCreationLevel, NodeList}
import org.objectweb.asm.Opcodes
import org.objectweb.asm.tree._

// TODO: typo in Opcodes.java: visiTableSwitchInsn -> visitTableSwitchInsn
object DecompileMethod {
  def decompileBody(classNode: ClassNode, node: MethodNode): BlockStmt = {
    node.instructions
    ???
  }

  def decompileInsn(node: AbstractInsnNode): Either[Statement, Expression] = {
    val args: Array[Expression] = ???
    node.getOpcode match {
      // InsnNode
      case Opcodes.NOP => Left(new EmptyStmt())
      case Opcodes.ACONST_NULL => Right(new NullLiteralExpr())
      case Opcodes.ICONST_M1 => Right(new IntegerLiteralExpr(-1))
      case Opcodes.ICONST_0 => Right(new IntegerLiteralExpr(0))
      case Opcodes.ICONST_1 => Right(new IntegerLiteralExpr(1))
      case Opcodes.ICONST_2 => Right(new IntegerLiteralExpr(2))
      case Opcodes.ICONST_3 => Right(new IntegerLiteralExpr(3))
      case Opcodes.ICONST_4 => Right(new IntegerLiteralExpr(4))
      case Opcodes.ICONST_5 => Right(new IntegerLiteralExpr(5))
        // TODO: Research idea distribution of iconst instructions in real code
        // TODO: hex form of constant in comment
      case Opcodes.LCONST_0 => Right(new LongLiteralExpr("0L"))
      case Opcodes.LCONST_1 => Right(new LongLiteralExpr("1L"))
      case Opcodes.FCONST_0 => Right(new DoubleLiteralExpr("0.0F"))
      case Opcodes.FCONST_1 => Right(new DoubleLiteralExpr("1.0F"))
      case Opcodes.FCONST_2 => Right(new DoubleLiteralExpr("2.0F"))
      case Opcodes.DCONST_0 => Right(new DoubleLiteralExpr("0.0D"))
      case Opcodes.DCONST_1 => Right(new DoubleLiteralExpr("1.0D"))
      // IntInsnNode
      case Opcodes.BIPUSH => ???
      case Opcodes.SIPUSH => ???
      // LdcInsnNode
      case Opcodes.LDC =>
        Right(node.asInstanceOf[LdcInsnNode].cst match {
          case cst: java.lang.Integer => new IntegerLiteralExpr(cst)
          case cst: java.lang.Float => ??? // new DoubleLiteralExpr(???) // TODO: float vs double
          case cst: java.lang.Long => new LongLiteralExpr(cst)
          case cst: java.lang.Double => new DoubleLiteralExpr(cst) // TODO: float vs double??
          case cst: java.lang.String => new StringLiteralExpr(cst)
          case cst: org.objectweb.asm.Type => new ClassExpr(???)
        })
      // VarInsnNode
      case Opcodes.ILOAD => Right(args(0))
      case Opcodes.LLOAD => Right(args(0))
      case Opcodes.FLOAD => Right(args(0))
      case Opcodes.DLOAD => Right(args(0))
      case Opcodes.ALOAD => Right(args(0))
      // TODO: use `|` patterns
      // InsnNode
      case Opcodes.IALOAD => Right(new ArrayAccessExpr(args(0), args(1)))
      case Opcodes.LALOAD => Right(new ArrayAccessExpr(args(0), args(1)))
      case Opcodes.FALOAD => Right(new ArrayAccessExpr(args(0), args(1)))
      case Opcodes.DALOAD => Right(new ArrayAccessExpr(args(0), args(1)))
      case Opcodes.AALOAD => Right(new ArrayAccessExpr(args(0), args(1)))
      case Opcodes.BALOAD => Right(new ArrayAccessExpr(args(0), args(1)))
      case Opcodes.CALOAD => Right(new ArrayAccessExpr(args(0), args(1)))
      case Opcodes.SALOAD => Right(new ArrayAccessExpr(args(0), args(1)))
      // VarInsnNode
      case Opcodes.ISTORE => Right(args(0))
      case Opcodes.LSTORE => Right(args(0))
      case Opcodes.FSTORE => Right(args(0))
      case Opcodes.DSTORE => Right(args(0))
      case Opcodes.ASTORE => Right(args(0))
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
      case Opcodes.IADD => Right(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.PLUS))
      case Opcodes.LADD => Right(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.PLUS))
      case Opcodes.FADD => Right(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.PLUS))
      case Opcodes.DADD => Right(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.PLUS))
      case Opcodes.ISUB => Right(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.MINUS))
      case Opcodes.LSUB => Right(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.MINUS))
      case Opcodes.FSUB => Right(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.MINUS))
      case Opcodes.DSUB => Right(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.MINUS))
      case Opcodes.IMUL => Right(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.MULTIPLY))
      case Opcodes.LMUL => Right(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.MULTIPLY))
      case Opcodes.FMUL => Right(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.MULTIPLY))
      case Opcodes.DMUL => Right(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.MULTIPLY))
      case Opcodes.IDIV => Right(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.DIVIDE))
      case Opcodes.LDIV => Right(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.DIVIDE))
      case Opcodes.FDIV => Right(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.DIVIDE))
      case Opcodes.DDIV => Right(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.DIVIDE))
      case Opcodes.IREM => Right(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.REMAINDER))
      case Opcodes.LREM => Right(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.REMAINDER))
      case Opcodes.FREM => Right(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.REMAINDER))
      case Opcodes.DREM => Right(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.REMAINDER))
      // TODO: UnaryExpr.Operator.BITWISE_COMPLEMENT
      case Opcodes.INEG => Right(new UnaryExpr(args(0), UnaryExpr.Operator.MINUS))
      case Opcodes.LNEG => Right(new UnaryExpr(args(0), UnaryExpr.Operator.MINUS))
      case Opcodes.FNEG => Right(new UnaryExpr(args(0), UnaryExpr.Operator.MINUS))
      case Opcodes.DNEG => Right(new UnaryExpr(args(0), UnaryExpr.Operator.MINUS))
      case Opcodes.ISHL  => Right(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.LEFT_SHIFT))
      case Opcodes.LSHL  => Right(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.LEFT_SHIFT))
      case Opcodes.ISHR  => Right(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.SIGNED_RIGHT_SHIFT))
      case Opcodes.LSHR  => Right(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.SIGNED_RIGHT_SHIFT))
      case Opcodes.IUSHR => Right(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.UNSIGNED_RIGHT_SHIFT))
      case Opcodes.LUSHR => Right(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.UNSIGNED_RIGHT_SHIFT))
      case Opcodes.IAND  => Right(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.BINARY_AND))
      case Opcodes.LAND  => Right(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.BINARY_AND))
      case Opcodes.IOR   => Right(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.BINARY_OR))
      case Opcodes.LOR   => Right(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.BINARY_OR))
      case Opcodes.IXOR  => Right(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.XOR))
      case Opcodes.LXOR  => Right(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.XOR))
      // IincInsnNode
      case Opcodes.IINC => ???
      // InsnNode
      case Opcodes.I2L => Right(new CastExpr(PrimitiveType.longType  , args(0)))
      case Opcodes.I2F => Right(new CastExpr(PrimitiveType.floatType , args(0)))
      case Opcodes.I2D => Right(new CastExpr(PrimitiveType.doubleType, args(0)))
      case Opcodes.L2I => Right(new CastExpr(PrimitiveType.intType   , args(0)))
      case Opcodes.L2F => Right(new CastExpr(PrimitiveType.floatType , args(0)))
      case Opcodes.L2D => Right(new CastExpr(PrimitiveType.doubleType, args(0)))
      case Opcodes.F2I => Right(new CastExpr(PrimitiveType.intType   , args(0)))
      case Opcodes.F2L => Right(new CastExpr(PrimitiveType.longType  , args(0)))
      case Opcodes.F2D => Right(new CastExpr(PrimitiveType.doubleType, args(0)))
      case Opcodes.D2I => Right(new CastExpr(PrimitiveType.intType   , args(0)))
      case Opcodes.D2L => Right(new CastExpr(PrimitiveType.longType  , args(0)))
      case Opcodes.D2F => Right(new CastExpr(PrimitiveType.floatType , args(0)))
      case Opcodes.I2B => Right(new CastExpr(PrimitiveType.byteType  , args(0)))
      case Opcodes.I2C => Right(new CastExpr(PrimitiveType.charType  , args(0)))
      case Opcodes.I2S => Right(new CastExpr(PrimitiveType.shortType , args(0)))
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
      case Opcodes.IRETURN => Left(new ReturnStmt(args(0)))
      case Opcodes.LRETURN => Left(new ReturnStmt(args(0)))
      case Opcodes.FRETURN => Left(new ReturnStmt(args(0)))
      case Opcodes.DRETURN => Left(new ReturnStmt(args(0)))
      case Opcodes.ARETURN => Left(new ReturnStmt(args(0)))
      case Opcodes.RETURN  => Left(new ReturnStmt(/*Nothing*/))
      // FieldInsnNode
      case Opcodes.GETSTATIC => ???
      case Opcodes.PUTSTATIC => ???
      case Opcodes.GETFIELD => Right(new FieldAccessExpr(args(0), ???, new SimpleName(node.asInstanceOf[FieldInsnNode].name)))
      case Opcodes.PUTFIELD => ???
      // MethodInsnNode
      case Opcodes.INVOKEVIRTUAL   => ??? //Right(new MethodCallExpr(???, ???, ???, ???))
      case Opcodes.INVOKESPECIAL   => ???
      case Opcodes.INVOKESTATIC    => ??? //Right(new MethodCallExpr(???, ???, ???, ???))
      case Opcodes.INVOKEINTERFACE => ??? //Right(new MethodCallExpr(???, ???, ???, ???))
      // InvokeDynamicInsnNode
      case Opcodes.INVOKEDYNAMIC => ???
      // TypeInsnNode
      case Opcodes.NEW => ???
      // IntInsnNode
      case Opcodes.NEWARRAY  => ??? //Right(new ArrayCreationExpr(???, new NodeList(new ArrayCreationLevel(???)), ???))
      // TypeInsnNode
      case Opcodes.ANEWARRAY => ??? //Right(new ArrayCreationExpr(???, new NodeList(new ArrayCreationLevel(???)), ???))
      // InsnNode
      case Opcodes.ARRAYLENGTH => ???
      case Opcodes.ATHROW => Left(new ThrowStmt(args(0)))
      // TypeInsnNode
      case Opcodes.CHECKCAST => ??? // TODO: figure out: Right(new CastExpr(???, ???))
      case Opcodes.INSTANCEOF => Right(new InstanceOfExpr(args(0), ???/*node.asInstanceOf[TypeInsnNode].desc*/))
      // InsnNode
      case Opcodes.MONITORENTER => ???
      case Opcodes.MONITOREXIT => ???
      // MultiANewArrayInsnNode
      case Opcodes.MULTIANEWARRAY => Right(new ArrayCreationExpr(???, ???, ???))
      // JumpInsnNode
      case Opcodes.IFNULL => ???
      case Opcodes.IFNONNULL => ???
      // Synthetic instructions
      case _ =>
        node match {
          case node: LabelNode =>
            Left(new LabeledStmt(new SimpleName(node.getLabel.toString), /*TODO*/new EmptyStmt()))
          case node: FrameNode => ???
          case node: LineNumberNode => ???
          case _ => throw new Exception(f"unknown instruction type: $node")
        }
    }
  }
}
