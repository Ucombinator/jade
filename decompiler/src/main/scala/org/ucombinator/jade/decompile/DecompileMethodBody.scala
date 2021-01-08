package org.ucombinator.jade.decompile

import java.io.{PrintWriter, StringWriter}

import com.github.javaparser.ast.body.{BodyDeclaration, ConstructorDeclaration, InitializerDeclaration, MethodDeclaration}
import com.github.javaparser.ast.comments.BlockComment
import com.github.javaparser.ast.expr.{ObjectCreationExpr, StringLiteralExpr}
import com.github.javaparser.ast.stmt.{BlockStmt, EmptyStmt, Statement, ThrowStmt}
import com.github.javaparser.ast.{Modifier, NodeList}
import org.objectweb.asm.tree.{ClassNode, MethodNode}
import org.objectweb.asm.util.{Textifier, TraceMethodVisitor}
import org.ucombinator.jade.asm.Insn
import org.ucombinator.jade.classfile.Descriptor
import org.ucombinator.jade.decompile.methodbody.ControlFlowGraph
import org.ucombinator.jade.decompile.methodbody.ssa.SSA
import org.ucombinator.jade.util.jgrapht.{Dominator, GraphViz}
import org.ucombinator.jade.util.{Errors, JavaParser, Logging, VFS}

import scala.jdk.CollectionConverters._

object DecompileMethodBody extends Logging {
  private def stubBody(message: String, comment: BlockComment): BlockStmt = {
    val statements = new NodeList[Statement](
      JavaParser.setComment(
        new ThrowStmt(
          new ObjectCreationExpr(
            null, Descriptor.classNameType("java.lang.UnsupportedOperationException"),
            new NodeList(new StringLiteralExpr(message)))),
        comment))
    if (true) { // TODO: option for generating compilable versus uncompilable stub bodies
      statements.add(JavaParser.setComment(new EmptyStmt(), new BlockComment(" The following is unreachable code so that this code generates a compilation error ")))
    }
    new BlockStmt(statements)
  }

  def decompileBodyStub(classNode: ClassNode, node: MethodNode): BlockStmt = {
    val instructions = {
      if (node.instructions.size() == 0) {
        "   <no instructions>" // TODO: check
      } else {
        val textifier = new Textifier()
        node.accept(new TraceMethodVisitor(null, textifier))
        val stringWriter = new StringWriter()
        textifier.print(new PrintWriter(stringWriter))
        stringWriter.toString
      }
    }
    stubBody(
      "Jade decompiler generated only a stub implementation",
      new BlockComment(f" This is a stub implementation generated by the Jade decompiler.\n" +
      f"         *\n" +
      f"         * Max Stack: ${node.maxStack}\n" +
      f"         * Max Locals: ${node.maxLocals}\n" +
      f"         * Instructions:\n" +
      instructions.lines.iterator.asScala.map("         *" + _).mkString("\n") + "\n" +
      f"         "))
  }
  def setDeclarationBody(declaration: BodyDeclaration[_ <: BodyDeclaration[_]], body: BlockStmt): Unit = {
    declaration match {
      case declaration: InitializerDeclaration => declaration.setBody(body)
      case declaration: ConstructorDeclaration => declaration.setBody(body)
      case declaration: MethodDeclaration => declaration.setBody(body)
      case declaration => Errors.unmatchedType(declaration)
    }
  }
  def decompileBody(owner: String, classNode: ClassNode, i: Int, method: MethodNode, j: Int, methods: Int, declaration: BodyDeclaration[_ <: BodyDeclaration[_]]): Unit = {
    this.logger.debug("!!!!!!!!!!!!")
    this.logger.info(f"Decompiling [${i + 1} of ${VFS.classes.size}] ${classNode.name} [${j + 1} of $methods] ${method.name} (signature = ${method.signature}, descriptor = ${method.desc})")

    if (method.instructions.size == 0) {
      // The method has no body as even methods with empty bodies have a `return` instruction
      this.logger.debug("**** Method is has no body ****")

      def warningBody(warning: String): BlockStmt = {
        if (false) { // TODO: option for fatal error vs uncompilable body vs compilable body
          this.logger.error(warning)
          throw new Exception(warning) // TODO
        } else {
          this.logger.warn(warning)
          stubBody(warning, null)
        }
      }

      declaration match { // TODO: use setDeclarationBody
        case declaration: InitializerDeclaration =>
          declaration.setBody(warningBody(f"No implementation for the static initializer for class ${classNode.name}"))
        case declaration: ConstructorDeclaration =>
          declaration.setBody(warningBody(f"No implementation for constructor ${classNode.name}(signature = ${method.signature}, descriptor = ${method.desc})"))
        case declaration: MethodDeclaration =>
          val modifiers = declaration.getModifiers
          // TODO: if !(*.sym)
          if (modifiers.contains(Modifier.abstractModifier) || modifiers.contains(Modifier.nativeModifier)) {
            declaration.setBody(null)
          } else {
            declaration.setBody(warningBody(s"No implementation for non-abstract, non-native method: ${classNode.name}.${method.name}(signature = ${method.signature}, descriptor = ${method.desc})"))
          }
        case declaration => Errors.unmatchedType(declaration)
      }
    } else {
      this.logger.debug(f"**** Method has a body with ${method.instructions.size} instructions ****")
      this.logger.debug("**** ControlFlowGraph ****")

      // loop via dominators (exit vs return is unclear)
      // if
      // switch via dominators
      // catch via dominators
      // synchronized: via CFG (problem: try{sync{try}}?)

      val cfg = ControlFlowGraph(owner, method)

      this.logger.debug("++++ cfg ++++\n" + GraphViz.toString(cfg))
      for (v <- cfg.graph.vertexSet().asScala) {
        this.logger.debug(f"v: ${cfg.graph.incomingEdgesOf(v).size()}: $v")
      }

      this.logger.debug("**** SSA ****")
      val ids = SSA(owner, method, cfg)

      this.logger.debug("++++ frames: " + ids.frames.length + " ++++")
      for (i <- 0 until method.instructions.size) {
        this.logger.debug(f"frame($i): ${ids.frames(i)}")
      }

      this.logger.debug("++++ results and arguments ++++")
      for (i <- 0 until method.instructions.size) {
          val insn = method.instructions.get(i)
        this.logger.debug(f"args($i): ${Insn.longString(method, insn)} --- ${ids.instructionArguments.get(insn)}")
      }

      this.logger.debug("++++ ssa map ++++")
      for ((key, value) <- ids.ssaMap) {
        this.logger.debug(s"ssa: $key -> $value")
      }

      this.logger.debug("**** Dominators ****")
      val doms = Dominator.dominatorTree(cfg.graphWithExceptions, cfg.entry)

      this.logger.debug("++++ dominator tree ++++\n"+
        GraphViz.toString(doms))

      this.logger.debug("++++ dominator nesting ++++\n" +
        GraphViz.nestingTree(cfg.graphWithExceptions, doms, cfg.entry))

      this.logger.debug("**** Loops ****")

      this.logger.debug("++++ loop heads ++++\n")
      // JEP 334: JVM Constants API: https://openjdk.java.net/jeps/334 

      this.logger.debug("++++ loop heads ++++\n")

      var statements = List[Statement]()
      for (insn <- method.instructions.toArray) {
        val (retVal, decompiled) = DecompileInsn.decompileInsn(insn, ids)
        statements = statements :+ DecompileInsn.decompileInsn(retVal, decompiled)
      }
      this.logger.debug("++++ statements ++++\n" + statements.mkString("\n"))
      setDeclarationBody(declaration, new BlockStmt(new NodeList[Statement](statements.asJava)))
    }
  }
}