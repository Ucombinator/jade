package org.ucombinator.jade.decompile

//import java.lang.invoke.LambdaMetafactory

import scala.annotation.tailrec
import scala.jdk.CollectionConverters._

import com.github.javaparser.ast.ArrayCreationLevel
import com.github.javaparser.ast.NodeList
import com.github.javaparser.ast.`type`.ArrayType
import com.github.javaparser.ast.`type`.ClassOrInterfaceType
import com.github.javaparser.ast.`type`.PrimitiveType
import com.github.javaparser.ast.`type`.Type
import com.github.javaparser.ast.comments.BlockComment
import com.github.javaparser.ast.expr._
import com.github.javaparser.ast.stmt._
import org.objectweb.asm.Opcodes
import org.objectweb.asm.tree._
import org.ucombinator.jade.classfile.Descriptor
import org.ucombinator.jade.decompile.methodbody.ssa.SSA
import org.ucombinator.jade.decompile.methodbody.ssa.Var
import org.ucombinator.jade.util.JavaParser
import org.ucombinator.jade.util.Log

/*
Nestings

Compare
Compile

CommonLibrary
Generators
 */

/*
  scalafmt: {
    maxColumn = 300,
    align.preset = some,
    align.multiline = true,
    align.tokens.add = [{
      code = "extends", owner = "Defn.(Class|Trait|Object)",
    }],
  }
 */

sealed trait DecompiledInsn { def usesNextInsn = true /* TODO */ }
case class DecompiledStatement(statement: Statement, override val usesNextInsn: Boolean = true) extends DecompiledInsn
case class DecompiledExpression(expression: Expression)                                         extends DecompiledInsn
case object DecompiledMove /*(TODO)*/                                                           extends DecompiledInsn
case class DecompiledIf(labelNode: LabelNode, condition: Expression)                            extends DecompiledInsn
case class DecompiledGoto(labelNode: LabelNode)                                                 extends DecompiledInsn { override def usesNextInsn = false }
case class DecompiledSwitch(labels: Map[Int, LabelNode], default: LabelNode)                    extends DecompiledInsn { override def usesNextInsn = false }
case class DecompiledNew(descriptor: ClassOrInterfaceType)                                      extends DecompiledInsn
case class DecompiledMonitorEnter(expression: Expression)                                       extends DecompiledInsn
case class DecompiledMonitorExit(expression: Expression)                                        extends DecompiledInsn
case class DecompiledLabel(node: LabelNode)                                                     extends DecompiledInsn
case class DecompiledFrame(node: FrameNode)                                                     extends DecompiledInsn
case class DecompiledLineNumber(node: LineNumberNode)                                           extends DecompiledInsn
case class DecompiledUnsupported(insn: AbstractInsnNode)                                        extends DecompiledInsn

// TODO: typo in Opcodes.java: visiTableSwitchInsn -> visitTableSwitchInsn
// TODO: typo in javaparser BlockComment: can has -> can have
// TODO: Research idea: distribution of iconst instructions in real code
// TODO: add hex form of literals in a comment
// TODO: literals: float vs double
// TODO: use `|` patterns
// TODO: UnaryExpr.Operator.BITWISE_COMPLEMENT: 0: iload_1; 1: iconst_m1; 2: ixor
object DecompileInsn extends Log {
  def decompileVar(variable: Var): NameExpr = { new NameExpr(variable.name) }

  def decompileInsn(retVar: Var, insn: DecompiledInsn): Statement = {
    def comment(string: String): Statement = { JavaParser.setComment(new EmptyStmt(), new BlockComment(string)) }
    insn match {
      case DecompiledStatement(statement, _)                                 => statement
      case DecompiledExpression(expression)                                  => new ExpressionStmt(new AssignExpr(decompileVar(retVar), expression, AssignExpr.Operator.ASSIGN))
      case DecompiledMove /*(TODO)*/                                         => comment(f"TODO: move insn")
      case DecompiledIf(labelNode: LabelNode, condition: Expression)         => new IfStmt(condition, new BreakStmt(labelNode.toString), null)
      case DecompiledGoto(labelNode: LabelNode)                              => new BreakStmt(labelNode.toString) // TODO: use instruction number?
      case DecompiledSwitch(labels: Map[Int, LabelNode], default: LabelNode) => comment(f"Switch ${labels} ${default}")
      case DecompiledNew(descriptor: ClassOrInterfaceType)                   => comment(f"new ${descriptor}")
      case DecompiledMonitorEnter(expression)                                => comment(f"Monitor Enter: ${expression}")
      case DecompiledMonitorExit(expression)                                 => comment(f"Monitor Exit: ${expression}")
      case DecompiledLabel(node: LabelNode)                                  => comment(f"Label ${node}") // TODO: use instruction number?
      case DecompiledFrame(node: FrameNode)                                  => comment(f"Frame ${node}")
      case DecompiledLineNumber(node: LineNumberNode)                        => comment(f"Line number: ${node.line}")
      case DecompiledUnsupported(insn: AbstractInsnNode)                     => comment(f"Unsupported ${insn}")
    }
  }
  def decompileInsn(node: AbstractInsnNode, ssa: SSA): (Var, DecompiledInsn) = {
    val (retVar, argVars) = ssa.instructionArguments.getOrElse(node, (null, List()))
    val args: Array[Expression] = argVars.toArray.map(decompileVar)
    //val ret: Expression = decompileVar(retVar)
    def call(node: AbstractInsnNode): (MethodInsnNode, Array[Type], NodeList[Type]) = {
      val insn = node.asInstanceOf[MethodInsnNode]
      val (argumentTypes, _) = Descriptor.methodDescriptor(insn.desc)
      val typeArguments = new NodeList[Type]()
      (insn, argumentTypes, typeArguments)
    }
    def instanceCall(node: AbstractInsnNode): DecompiledInsn = {
      val (insn, argumentTypes, typeArguments) = call(node)
      DecompiledExpression(new MethodCallExpr( /*TODO: cast to insn.owner?*/ args(0), typeArguments, insn.name, new NodeList(argumentTypes.indices.map(i => args(i + 1)): _*)))
    }
    def staticCall(node: AbstractInsnNode): DecompiledInsn = {
      val (insn, argumentTypes, typeArguments) = call(node)
      val scope = new FieldAccessExpr(Descriptor.classNameExpr(insn.owner), /*TODO*/ new NodeList(), new SimpleName(insn.name))
      DecompiledExpression(new MethodCallExpr(scope, typeArguments, insn.name, new NodeList(argumentTypes.indices.map(args): _*)))
    }
    (
      retVar,
      node.getOpcode match {
        // InsnNode
        case Opcodes.NOP         => DecompiledStatement(new EmptyStmt())
        case Opcodes.ACONST_NULL => DecompiledExpression(new NullLiteralExpr())
        case Opcodes.ICONST_M1   => DecompiledExpression(new IntegerLiteralExpr("-1"))
        case Opcodes.ICONST_0    => DecompiledExpression(new IntegerLiteralExpr("0"))
        case Opcodes.ICONST_1    => DecompiledExpression(new IntegerLiteralExpr("1"))
        case Opcodes.ICONST_2    => DecompiledExpression(new IntegerLiteralExpr("2"))
        case Opcodes.ICONST_3    => DecompiledExpression(new IntegerLiteralExpr("3"))
        case Opcodes.ICONST_4    => DecompiledExpression(new IntegerLiteralExpr("4"))
        case Opcodes.ICONST_5    => DecompiledExpression(new IntegerLiteralExpr("5"))
        case Opcodes.LCONST_0    => DecompiledExpression(new LongLiteralExpr("0L"))
        case Opcodes.LCONST_1    => DecompiledExpression(new LongLiteralExpr("1L"))
        case Opcodes.FCONST_0    => DecompiledExpression(new DoubleLiteralExpr("0.0F"))
        case Opcodes.FCONST_1    => DecompiledExpression(new DoubleLiteralExpr("1.0F"))
        case Opcodes.FCONST_2    => DecompiledExpression(new DoubleLiteralExpr("2.0F"))
        case Opcodes.DCONST_0    => DecompiledExpression(new DoubleLiteralExpr("0.0D"))
        case Opcodes.DCONST_1    => DecompiledExpression(new DoubleLiteralExpr("1.0D"))
        // IntInsnNode
        case Opcodes.BIPUSH => DecompiledMove
        case Opcodes.SIPUSH => DecompiledMove
        // LdcInsnNode
        case Opcodes.LDC => DecompiledExpression(DecompileClass.decompileLiteral(node.asInstanceOf[LdcInsnNode].cst))
        // VarInsnNode
        case Opcodes.ILOAD => DecompiledExpression(args(0))
        case Opcodes.LLOAD => DecompiledExpression(args(0))
        case Opcodes.FLOAD => DecompiledExpression(args(0))
        case Opcodes.DLOAD => DecompiledExpression(args(0))
        case Opcodes.ALOAD => DecompiledExpression(args(0))
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
        case Opcodes.IASTORE => DecompiledExpression(new AssignExpr(new ArrayAccessExpr(args(0), args(1)), args(2), AssignExpr.Operator.ASSIGN))
        case Opcodes.LASTORE => DecompiledExpression(new AssignExpr(new ArrayAccessExpr(args(0), args(1)), args(2), AssignExpr.Operator.ASSIGN))
        case Opcodes.FASTORE => DecompiledExpression(new AssignExpr(new ArrayAccessExpr(args(0), args(1)), args(2), AssignExpr.Operator.ASSIGN))
        case Opcodes.DASTORE => DecompiledExpression(new AssignExpr(new ArrayAccessExpr(args(0), args(1)), args(2), AssignExpr.Operator.ASSIGN))
        case Opcodes.AASTORE => DecompiledExpression(new AssignExpr(new ArrayAccessExpr(args(0), args(1)), args(2), AssignExpr.Operator.ASSIGN))
        case Opcodes.BASTORE => DecompiledExpression(new AssignExpr(new ArrayAccessExpr(args(0), args(1)), args(2), AssignExpr.Operator.ASSIGN))
        case Opcodes.CASTORE => DecompiledExpression(new AssignExpr(new ArrayAccessExpr(args(0), args(1)), args(2), AssignExpr.Operator.ASSIGN))
        case Opcodes.SASTORE => DecompiledExpression(new AssignExpr(new ArrayAccessExpr(args(0), args(1)), args(2), AssignExpr.Operator.ASSIGN))
        case Opcodes.POP     => DecompiledMove
        case Opcodes.POP2    => DecompiledMove
        case Opcodes.DUP     => DecompiledMove
        case Opcodes.DUP_X1  => DecompiledMove
        case Opcodes.DUP_X2  => DecompiledMove
        case Opcodes.DUP2    => DecompiledMove
        case Opcodes.DUP2_X1 => DecompiledMove
        case Opcodes.DUP2_X2 => DecompiledMove
        case Opcodes.SWAP    => DecompiledMove
        case Opcodes.IADD    => DecompiledExpression(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.PLUS))
        case Opcodes.LADD    => DecompiledExpression(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.PLUS))
        case Opcodes.FADD    => DecompiledExpression(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.PLUS))
        case Opcodes.DADD    => DecompiledExpression(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.PLUS))
        case Opcodes.ISUB    => DecompiledExpression(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.MINUS))
        case Opcodes.LSUB    => DecompiledExpression(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.MINUS))
        case Opcodes.FSUB    => DecompiledExpression(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.MINUS))
        case Opcodes.DSUB    => DecompiledExpression(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.MINUS))
        case Opcodes.IMUL    => DecompiledExpression(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.MULTIPLY))
        case Opcodes.LMUL    => DecompiledExpression(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.MULTIPLY))
        case Opcodes.FMUL    => DecompiledExpression(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.MULTIPLY))
        case Opcodes.DMUL    => DecompiledExpression(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.MULTIPLY))
        case Opcodes.IDIV    => DecompiledExpression(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.DIVIDE))
        case Opcodes.LDIV    => DecompiledExpression(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.DIVIDE))
        case Opcodes.FDIV    => DecompiledExpression(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.DIVIDE))
        case Opcodes.DDIV    => DecompiledExpression(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.DIVIDE))
        case Opcodes.IREM    => DecompiledExpression(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.REMAINDER))
        case Opcodes.LREM    => DecompiledExpression(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.REMAINDER))
        case Opcodes.FREM    => DecompiledExpression(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.REMAINDER))
        case Opcodes.DREM    => DecompiledExpression(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.REMAINDER))
        case Opcodes.INEG    => DecompiledExpression(new UnaryExpr(args(0), UnaryExpr.Operator.MINUS))
        case Opcodes.LNEG    => DecompiledExpression(new UnaryExpr(args(0), UnaryExpr.Operator.MINUS))
        case Opcodes.FNEG    => DecompiledExpression(new UnaryExpr(args(0), UnaryExpr.Operator.MINUS))
        case Opcodes.DNEG    => DecompiledExpression(new UnaryExpr(args(0), UnaryExpr.Operator.MINUS))
        case Opcodes.ISHL    => DecompiledExpression(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.LEFT_SHIFT))
        case Opcodes.LSHL    => DecompiledExpression(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.LEFT_SHIFT))
        case Opcodes.ISHR    => DecompiledExpression(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.SIGNED_RIGHT_SHIFT))
        case Opcodes.LSHR    => DecompiledExpression(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.SIGNED_RIGHT_SHIFT))
        case Opcodes.IUSHR   => DecompiledExpression(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.UNSIGNED_RIGHT_SHIFT))
        case Opcodes.LUSHR   => DecompiledExpression(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.UNSIGNED_RIGHT_SHIFT))
        case Opcodes.IAND    => DecompiledExpression(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.BINARY_AND))
        case Opcodes.LAND    => DecompiledExpression(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.BINARY_AND))
        case Opcodes.IOR     => DecompiledExpression(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.BINARY_OR))
        case Opcodes.LOR     => DecompiledExpression(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.BINARY_OR))
        case Opcodes.IXOR    => DecompiledExpression(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.XOR))
        case Opcodes.LXOR    => DecompiledExpression(new BinaryExpr(args(0), args(1), BinaryExpr.Operator.XOR))
        // IincInsnNode
        // TODO: double check that iinc works (because it is a strange instruction)
        case Opcodes.IINC => DecompiledExpression(new BinaryExpr(args(0), new IntegerLiteralExpr(node.asInstanceOf[IincInsnNode].incr.toString), BinaryExpr.Operator.PLUS))
        // InsnNode
        case Opcodes.I2L   => DecompiledExpression(new CastExpr(PrimitiveType.longType, args(0)))
        case Opcodes.I2F   => DecompiledExpression(new CastExpr(PrimitiveType.floatType, args(0)))
        case Opcodes.I2D   => DecompiledExpression(new CastExpr(PrimitiveType.doubleType, args(0)))
        case Opcodes.L2I   => DecompiledExpression(new CastExpr(PrimitiveType.intType, args(0)))
        case Opcodes.L2F   => DecompiledExpression(new CastExpr(PrimitiveType.floatType, args(0)))
        case Opcodes.L2D   => DecompiledExpression(new CastExpr(PrimitiveType.doubleType, args(0)))
        case Opcodes.F2I   => DecompiledExpression(new CastExpr(PrimitiveType.intType, args(0)))
        case Opcodes.F2L   => DecompiledExpression(new CastExpr(PrimitiveType.longType, args(0)))
        case Opcodes.F2D   => DecompiledExpression(new CastExpr(PrimitiveType.doubleType, args(0)))
        case Opcodes.D2I   => DecompiledExpression(new CastExpr(PrimitiveType.intType, args(0)))
        case Opcodes.D2L   => DecompiledExpression(new CastExpr(PrimitiveType.longType, args(0)))
        case Opcodes.D2F   => DecompiledExpression(new CastExpr(PrimitiveType.floatType, args(0)))
        case Opcodes.I2B   => DecompiledExpression(new CastExpr(PrimitiveType.byteType, args(0)))
        case Opcodes.I2C   => DecompiledExpression(new CastExpr(PrimitiveType.charType, args(0)))
        case Opcodes.I2S   => DecompiledExpression(new CastExpr(PrimitiveType.shortType, args(0)))
        case Opcodes.LCMP  => DecompiledExpression(new MethodCallExpr(null, null, new SimpleName("<lcmp>"), new NodeList(args(0), args(1))))
        case Opcodes.FCMPL => DecompiledExpression(new MethodCallExpr(null, null, new SimpleName("<fcmpl>"), new NodeList(args(0), args(1))))
        case Opcodes.FCMPG => DecompiledExpression(new MethodCallExpr(null, null, new SimpleName("<fcmpg>"), new NodeList(args(0), args(1))))
        case Opcodes.DCMPL => DecompiledExpression(new MethodCallExpr(null, null, new SimpleName("<dcmpl>"), new NodeList(args(0), args(1))))
        case Opcodes.DCMPG => DecompiledExpression(new MethodCallExpr(null, null, new SimpleName("<dcmpg>"), new NodeList(args(0), args(1))))
        // JumpInsnNode
        case Opcodes.IFEQ      => DecompiledIf(node.asInstanceOf[JumpInsnNode].label, new BinaryExpr(args(0), new IntegerLiteralExpr("0"), BinaryExpr.Operator.EQUALS))
        case Opcodes.IFNE      => DecompiledIf(node.asInstanceOf[JumpInsnNode].label, new BinaryExpr(args(0), new IntegerLiteralExpr("0"), BinaryExpr.Operator.NOT_EQUALS))
        case Opcodes.IFLT      => DecompiledIf(node.asInstanceOf[JumpInsnNode].label, new BinaryExpr(args(0), new IntegerLiteralExpr("0"), BinaryExpr.Operator.LESS))
        case Opcodes.IFGE      => DecompiledIf(node.asInstanceOf[JumpInsnNode].label, new BinaryExpr(args(0), new IntegerLiteralExpr("0"), BinaryExpr.Operator.GREATER_EQUALS))
        case Opcodes.IFGT      => DecompiledIf(node.asInstanceOf[JumpInsnNode].label, new BinaryExpr(args(0), new IntegerLiteralExpr("0"), BinaryExpr.Operator.GREATER))
        case Opcodes.IFLE      => DecompiledIf(node.asInstanceOf[JumpInsnNode].label, new BinaryExpr(args(0), new IntegerLiteralExpr("0"), BinaryExpr.Operator.LESS_EQUALS))
        case Opcodes.IF_ICMPEQ => DecompiledIf(node.asInstanceOf[JumpInsnNode].label, new BinaryExpr(args(0), args(0), BinaryExpr.Operator.EQUALS))
        case Opcodes.IF_ICMPNE => DecompiledIf(node.asInstanceOf[JumpInsnNode].label, new BinaryExpr(args(0), args(0), BinaryExpr.Operator.NOT_EQUALS))
        case Opcodes.IF_ICMPLT => DecompiledIf(node.asInstanceOf[JumpInsnNode].label, new BinaryExpr(args(0), args(0), BinaryExpr.Operator.LESS))
        case Opcodes.IF_ICMPGE => DecompiledIf(node.asInstanceOf[JumpInsnNode].label, new BinaryExpr(args(0), args(0), BinaryExpr.Operator.GREATER_EQUALS))
        case Opcodes.IF_ICMPGT => DecompiledIf(node.asInstanceOf[JumpInsnNode].label, new BinaryExpr(args(0), args(0), BinaryExpr.Operator.GREATER))
        case Opcodes.IF_ICMPLE => DecompiledIf(node.asInstanceOf[JumpInsnNode].label, new BinaryExpr(args(0), args(0), BinaryExpr.Operator.LESS_EQUALS))
        case Opcodes.IF_ACMPEQ => DecompiledIf(node.asInstanceOf[JumpInsnNode].label, new BinaryExpr(args(0), args(0), BinaryExpr.Operator.EQUALS))
        case Opcodes.IF_ACMPNE => DecompiledIf(node.asInstanceOf[JumpInsnNode].label, new BinaryExpr(args(0), args(0), BinaryExpr.Operator.NOT_EQUALS))
        case Opcodes.GOTO      => DecompiledGoto(node.asInstanceOf[JumpInsnNode].label)
        case Opcodes.JSR       => DecompiledUnsupported(node)
        // VarInsnNode
        case Opcodes.RET => DecompiledUnsupported(node)
        // TableSwitchInsnNode
        case Opcodes.TABLESWITCH =>
          val insn = node.asInstanceOf[TableSwitchInsnNode]
          assert(insn.labels.size == insn.max - insn.min)
          DecompiledSwitch(insn.labels.asScala.zipWithIndex.map({ case (l, i) => insn.min + i -> l }).toMap, insn.dflt)
        // LookupSwitch
        case Opcodes.LOOKUPSWITCH =>
          val insn = node.asInstanceOf[LookupSwitchInsnNode]
          assert(insn.labels.size == insn.keys.size)
          DecompiledSwitch(insn.labels.asScala.zip(insn.keys.asScala).map({ case (l, i) => i.intValue() -> l }).toMap, insn.dflt)
        // InsnNode
        case Opcodes.IRETURN => DecompiledStatement(new ReturnStmt(args(0)), usesNextInsn = false)
        case Opcodes.LRETURN => DecompiledStatement(new ReturnStmt(args(0)), usesNextInsn = false)
        case Opcodes.FRETURN => DecompiledStatement(new ReturnStmt(args(0)), usesNextInsn = false)
        case Opcodes.DRETURN => DecompiledStatement(new ReturnStmt(args(0)), usesNextInsn = false)
        case Opcodes.ARETURN => DecompiledStatement(new ReturnStmt(args(0)), usesNextInsn = false)
        case Opcodes.RETURN  => DecompiledStatement(new ReturnStmt( /*Nothing*/ ), usesNextInsn = false)
        // FieldInsnNode
        case Opcodes.GETSTATIC => val insn = node.asInstanceOf[FieldInsnNode]; DecompiledExpression(new FieldAccessExpr(Descriptor.classNameExpr(insn.owner), /*TODO*/ new NodeList(), new SimpleName(insn.name)))
        case Opcodes.PUTSTATIC => val insn = node.asInstanceOf[FieldInsnNode]; DecompiledExpression(new AssignExpr(new FieldAccessExpr(Descriptor.classNameExpr(insn.owner), /*TODO*/ new NodeList(), new SimpleName(insn.name)), args(0), AssignExpr.Operator.ASSIGN))
        case Opcodes.GETFIELD  => val insn = node.asInstanceOf[FieldInsnNode]; DecompiledExpression(new FieldAccessExpr(args(0), /*TODO*/ new NodeList(), new SimpleName(insn.name)))
        case Opcodes.PUTFIELD  => val insn = node.asInstanceOf[FieldInsnNode]; DecompiledExpression(new AssignExpr(new FieldAccessExpr(args(0), /*TODO*/ new NodeList(), new SimpleName(insn.name)), args(1), AssignExpr.Operator.ASSIGN))
        // MethodInsnNode
        case Opcodes.INVOKEVIRTUAL   => instanceCall(node)
        case Opcodes.INVOKESPECIAL   => instanceCall(node) // TODO: only for <init> (new, this, and super)?
        case Opcodes.INVOKESTATIC    => staticCall(node)
        case Opcodes.INVOKEINTERFACE => instanceCall(node)
        // InvokeDynamicInsnNode
        case Opcodes.INVOKEDYNAMIC => ??? // TODO: lambda
        // TypeInsnNode
        case Opcodes.NEW => DecompiledNew(Descriptor.classNameType(node.asInstanceOf[TypeInsnNode].desc)) // TODO: pair with <init>
        // IntInsnNode
        case Opcodes.NEWARRAY =>
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
          DecompiledExpression(new ArrayCreationExpr(typ, new NodeList(new ArrayCreationLevel(args(0), new NodeList())), null))
        // TypeInsnNode
        case Opcodes.ANEWARRAY =>
          val typ = Descriptor.classNameType(node.asInstanceOf[TypeInsnNode].desc)
          DecompiledExpression(new ArrayCreationExpr(typ, new NodeList(new ArrayCreationLevel(args(0), new NodeList())), null))
        // InsnNode
        case Opcodes.ARRAYLENGTH => DecompiledExpression(new FieldAccessExpr(args(0), new NodeList(), new SimpleName("length")))
        case Opcodes.ATHROW      => DecompiledStatement(new ThrowStmt(args(0)), usesNextInsn = false)
        // TypeInsnNode
        case Opcodes.CHECKCAST  => DecompiledExpression(new CastExpr(Descriptor.classNameType(node.asInstanceOf[TypeInsnNode].desc), args(0))) // TODO: check if works
        case Opcodes.INSTANCEOF => DecompiledExpression(new InstanceOfExpr(args(0), Descriptor.classNameType(node.asInstanceOf[TypeInsnNode].desc)))
        // InsnNode
        case Opcodes.MONITORENTER => DecompiledMonitorEnter(args(0))
        case Opcodes.MONITOREXIT  => DecompiledMonitorExit(args(0))
        // MultiANewArrayInsnNode
        case Opcodes.MULTIANEWARRAY =>
          // TODO: use asm.Type functions
          val dims = node.asInstanceOf[MultiANewArrayInsnNode].dims
          @tailrec def unwrap(typ: Type, levels: Int = 0): (Type, Int) = {
            typ match {
              case typ: ArrayType => unwrap(typ.getComponentType, levels + 1)
              case _              => (typ, levels)
            }
          }
          val (typ, expectedDims) = unwrap(Descriptor.fieldDescriptor(node.asInstanceOf[MultiANewArrayInsnNode].desc))
          val levels = new NodeList(
            args.slice(0, dims).map(new ArrayCreationLevel(_, new NodeList())) ++
              (dims until expectedDims).map(_ => new ArrayCreationLevel(null, new NodeList())): _*
          )
          DecompiledExpression(new ArrayCreationExpr(typ, levels, /*TODO: initializer*/ null))
        // JumpInsnNode
        case Opcodes.IFNULL    => DecompiledIf(node.asInstanceOf[JumpInsnNode].label, new BinaryExpr(args(0), new NullLiteralExpr(), BinaryExpr.Operator.EQUALS))
        case Opcodes.IFNONNULL => DecompiledIf(node.asInstanceOf[JumpInsnNode].label, new BinaryExpr(args(0), new NullLiteralExpr(), BinaryExpr.Operator.NOT_EQUALS))
        // Synthetic instructions
        case _ =>
          node match {
            case node: LabelNode      => DecompiledLabel(node)
            case node: FrameNode      => DecompiledFrame(node)
            case node: LineNumberNode => DecompiledLineNumber(node)
            case _                    => throw new Exception(f"unknown instruction type: ${node}")
          }
      }
    )
  }

  /*
  //case class DecodedLambda(interface: SootClass, interfaceMethod: SootMethod, implementationMethod: SootMethod, captures: java.util.List[soot.Value])
  // https://cr.openjdk.java.net/~briangoetz/lambda/lambda-translation.html
  def decodeLambda(e: InvokeDynamicInsnNode): LambdaExpr = {
    // Arguments are the captured variables
    // Static (bootstrap?) arguments describe lambda

    def decodeSimple(): LambdaExpr = {
      // Step 1: Find `interface`, which is the type of the closure returned by the lambda.
      assert(e.getType.isInstanceOf[RefType])
      val interface = e.getType.asInstanceOf[RefType].getSootClass

      // Step 2: Find the method in `interface` that the lambda corresponds to.
      // Unfortunately, this is not already computed so we find it manually.
      // This is complicated by the fact that it may be in a super-class or
      // super-interface of `interface`.
      assert(e.bsmArgs(0).isInstanceOf[ClassConstant])
      val bytecodeSignature = e.bsmArgs(0).asInstanceOf[ClassConstant].getValue
      val types = Util.v().jimpleTypesOfFieldOrMethodDescriptor(bytecodeSignature)
      val paramTypes = java.util.Arrays.asList[Type](types:_*).subList(0, types.length - 1)
      val returnType = types(types.length - 1)

      def findMethod(klass: SootClass): SootMethod = {
        val m = klass.getMethodUnsafe(e.getMethod.getName, paramTypes, returnType)
        if (m != null) { return m }

        if (klass.hasSuperclass) {
          val m = findMethod(klass.getSuperclass)
          if (m != null) { return m }
        }

        for (i <- klass.getInterfaces.asScala) {
          val m = findMethod(i)
          if (m != null) { return m }
        }

        return null
      }

      val interfaceMethod = findMethod(interface)

      // Step 3: Find `implementationMethod`, which is the method implementing the lambda
      //   Because calling e.getMethodRef.resolve may throw missmatched `static` errors,
      //   we look for the method manually.
      assert(e.bsmArgs(1).isInstanceOf[soot.jimple.MethodHandle])
      val implementationMethodRef = e.getBootstrapArg(1).asInstanceOf[soot.jimple.MethodHandle].getMethodRef
      val implementationMethod = implementationMethodRef.declaringClass().getMethodUnsafe(implementationMethodRef.getSubSignature)

      // Step 4: Find the `captures` which are values that should be saved and passed
      //   to `implementationMethod` before any other arguments
      val captures = e.getArgs

      return DecodedLambda(interface, interfaceMethod, implementationMethod, captures)
    }

    def bootstrapArgIsInt(index: Int, i: Int): Boolean = {
      e.bsmArgs(index) match {
        case arg: Integer => arg == i
        case _ => false
      }
    }

    // Check that this dynamic invoke uses LambdaMetafactory
    assert(e.bsm.getOwner == "java.lang.invoke.LambdaMetafactory")
    val bootstrapMethod = e.bsm.getName
    if (bootstrapMethod == "metafactory") {
      assert(e.bsmArgs.length == 3)
      return decodeSimple()
    } else if (bootstrapMethod == "altMetafactory") {
      e.bsmArgs(3) match {
        case flags: Integer =>
          val bridges = (flags & LambdaMetafactory.FLAG_BRIDGES) != 0
          val markers = (flags & LambdaMetafactory.FLAG_MARKERS) != 0
          val isSimple =
            (bridges && !markers && bootstrapArgIsInt(4, 0)) ||
            (!bridges && markers && bootstrapArgIsInt(4, 0)) ||
            (bridges && markers && bootstrapArgIsInt(4, 0) && bootstrapArgIsInt(5, 0))
          if (isSimple) { decodeSimple() }
          else { throw new Exception("Unimplemented altMetafactory: e = " + e) }
        case _ => throw new Exception("Non-int flags passed to altMetafactory: e = " + e)
      }
    } else {
      throw new Exception("Soot.decodeLambda could not decode: e = " + e)
    }
  }
   */
}
