package org.ucombinator.jade.decompile

import java.io.PrintWriter
import java.io.StringWriter

import scala.jdk.CollectionConverters._

import com.github.javaparser.ast.Modifier
import com.github.javaparser.ast.NodeList
import com.github.javaparser.ast.body.BodyDeclaration
import com.github.javaparser.ast.body.ConstructorDeclaration
import com.github.javaparser.ast.body.InitializerDeclaration
import com.github.javaparser.ast.body.MethodDeclaration
import com.github.javaparser.ast.comments.BlockComment
import com.github.javaparser.ast.expr.ObjectCreationExpr
import com.github.javaparser.ast.expr.StringLiteralExpr
import com.github.javaparser.ast.stmt.BlockStmt
import com.github.javaparser.ast.stmt.EmptyStmt
import com.github.javaparser.ast.stmt.Statement
import com.github.javaparser.ast.stmt.ThrowStmt
import org.objectweb.asm.tree.ClassNode
import org.objectweb.asm.tree.MethodNode
import org.objectweb.asm.util.Textifier
import org.objectweb.asm.util.TraceMethodVisitor
import org.ucombinator.jade.asm.Insn
import org.ucombinator.jade.classfile.Descriptor
import org.ucombinator.jade.analysis.ControlFlowGraph
import org.ucombinator.jade.decompile.DecompileStatements
import org.ucombinator.jade.analysis.Structure
import org.ucombinator.jade.analysis.StaticSingleAssignment
import org.ucombinator.jade.util.Errors
import org.ucombinator.jade.util.JavaParser
import org.ucombinator.jade.util.Log
import org.ucombinator.jade.jgrapht.Dominator
import org.ucombinator.jade.jgrapht.GraphViz

object DecompileMethodBody extends Log {
  private def stubBody(message: String, comment: BlockComment): BlockStmt = {
    val statements = new NodeList[Statement](
      JavaParser.setComment(
        new ThrowStmt(
          new ObjectCreationExpr(
            null,
            Descriptor.classNameType("java.lang.UnsupportedOperationException"),
            new NodeList(new StringLiteralExpr(message))
          )
        ),
        comment
      )
    )
    if (true) { // TODO: option for generating compilable versus uncompilable stub bodies
      statements.add(
        JavaParser.setComment(
          new EmptyStmt(),
          new BlockComment(" The following is unreachable code so that this code generates a compilation error ")
        )
      )
    }
    new BlockStmt(statements)
  }

  def decompileBodyStub(node: MethodNode): BlockStmt = {
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
      new BlockComment(
        f""" This is a stub implementation generated by the Jade decompiler.
           |         *
           |         * Max Stack: ${node.maxStack}
           |         * Max Locals: ${node.maxLocals}
           |         * Instructions:
           |${instructions.lines.iterator.asScala.map("         *" + _).mkString("\n")}
           |         """.stripMargin
      )
    )
  }
  def setDeclarationBody(declaration: BodyDeclaration[_ <: BodyDeclaration[_]], body: BlockStmt): Unit = {
    declaration match {
      case declaration: InitializerDeclaration => declaration.setBody(body); ()
      case declaration: ConstructorDeclaration => declaration.setBody(body); ()
      case declaration: MethodDeclaration => declaration.setBody(body); ()
      case declaration => Errors.unmatchedType(declaration)
    }
  }
  def decompileBody(
      classNode: ClassNode,
      method: MethodNode,
      declaration: BodyDeclaration[_ <: BodyDeclaration[_]]
  ): Unit = {
    if (method.instructions.size == 0) {
      // The method has no body as even methods with empty bodies have a `return` instruction
      this.log.debug("**** Method is has no body ****")

      def warningBody(warning: String): BlockStmt = {
        if (false) { // TODO: option for fatal error vs uncompilable body vs compilable body
          this.log.error(warning)
          throw new Exception(warning) // TODO
        } else {
          this.log.warn(warning)
          stubBody(warning, null)
        }
      }

      declaration match { // TODO: use setDeclarationBody
        case declaration: InitializerDeclaration =>
          declaration.setBody(warningBody(f"No implementation for the static initializer for class ${classNode.name}"))
          ()
        case declaration: ConstructorDeclaration =>
          declaration.setBody(
            warningBody(
              f"No implementation for constructor ${classNode.name}(signature = ${method.signature}, descriptor = ${method.desc})"
            )
          )
          ()
        case declaration: MethodDeclaration =>
          val modifiers = declaration.getModifiers
          // TODO: if !(*.sym)
          if (modifiers.contains(Modifier.abstractModifier) || modifiers.contains(Modifier.nativeModifier)) {
            declaration.setBody(null)
            ()
          } else {
            declaration.setBody(
              warningBody(
                f"No implementation for non-abstract, non-native method: ${classNode.name}.${method.name}(signature = ${method.signature}, descriptor = ${method.desc})"
              )
            )
            ()
          }
        case declaration => Errors.unmatchedType(declaration)
      }
    } else {
      this.log.debug(f"**** Method has a body with ${method.instructions.size} instructions ****")
      this.log.debug("**** ControlFlowGraph ****")

      // loop via dominators (exit vs return is unclear)
      // if
      // switch via dominators
      // catch via dominators
      // synchronized: via CFG (problem: try{sync{try}}?)

      val cfg = ControlFlowGraph(classNode.name, method)

      this.log.debug("++++ cfg ++++\n" + GraphViz.toString(cfg))
      for (v <- cfg.graph.vertexSet().asScala) {
        this.log.debug(f"v: ${cfg.graph.incomingEdgesOf(v).size()}: ${v}")
      }

      this.log.debug("**** SSA ****")
      val ssa = StaticSingleAssignment(classNode.name, method, cfg)

      this.log.debug(f"++++ frames: ${ssa.frames.length} ++++")
      for (i <- 0 until method.instructions.size) {
        this.log.debug(f"frame(${i}): ${ssa.frames(i)}")
      }

      this.log.debug("++++ results and arguments ++++")
      for (i <- 0 until method.instructions.size) {
        val insn = method.instructions.get(i)
        this.log.debug(f"args(${i}): ${Insn.longString(method, insn)} --- ${ssa.insnVars.get(insn)}")
      }

      this.log.debug("++++ ssa map ++++")
      for ((key, value) <- ssa.phiInputs) {
        this.log.debug(f"ssa: ${key} -> ${value}")
      }

      this.log.debug("**** Dominators ****")
      val doms = Dominator.dominatorTree(cfg.graphWithExceptions, cfg.entry)

      this.log.debug("++++ dominator tree ++++\n" + GraphViz.toString(doms))

      this.log.debug(
        "++++ dominator nesting ++++\n" + GraphViz.nestingTree(cfg.graphWithExceptions, doms, cfg.entry)
      )

      this.log.debug("**** Structure ****")
      val structure = Structure(cfg)

      // TODO: JEP 334: JVM Constants API: https://openjdk.java.net/jeps/334

      this.log.debug("**** Statement ****")
      val statement = DecompileStatements(cfg, ssa, structure)
      this.log.debug(statement.toString)
      setDeclarationBody(declaration, statement)

      // var statements = List[Statement]()
      // for (insn <- method.instructions.toArray) {
      //   val (retVal, decompiled) = DecompileInsn.decompileInsn(insn, ssa)
      //   statements = statements :+ DecompileInsn.decompileInsn(retVal, decompiled)
      // }
      // this.log.debug("++++ statements ++++\n" + statements.mkString("\n"))
      // setDeclarationBody(declaration, new BlockStmt(new NodeList[Statement](statements.asJava)))
    }
  }
}
