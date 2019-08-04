package org.ucombinator.jade.decompile

import java.io.{PrintWriter, StringWriter}

import com.github.javaparser.ast.body.{BodyDeclaration, ConstructorDeclaration, InitializerDeclaration, MethodDeclaration}
import com.github.javaparser.ast.comments.BlockComment
import com.github.javaparser.ast.expr.NullLiteralExpr
import com.github.javaparser.ast.stmt.{BlockStmt, ThrowStmt}
import com.github.javaparser.ast.{Modifier, NodeList}
import org.objectweb.asm.tree.{ClassNode, MethodNode}
import org.objectweb.asm.util.{Textifier, TraceMethodVisitor}
import org.ucombinator.jade.asm.Insn
import org.ucombinator.jade.decompile.method.ControlFlowGraph
import org.ucombinator.jade.decompile.method.ssa.SSA
import org.ucombinator.jade.util.jgrapht.{Dominator, GraphViz}
import org.ucombinator.jade.util.{JavaParser, Logging, VFS}

import scala.collection.JavaConverters._

object DecompileBody extends Logging {
  def decompileBodyStub(classNode: ClassNode, node: MethodNode): BlockStmt = {
    val textifier = new Textifier()
    node.accept(new TraceMethodVisitor(null, textifier))
    val stringWriter = new StringWriter()
    textifier.print(new PrintWriter(stringWriter))
    val comment = new BlockComment( // TODO: message depends on if there are zero instructions
      f" This is a stub implementation for the following instructions:\n${stringWriter.toString}")
    new BlockStmt(new NodeList(JavaParser.setComment(new ThrowStmt(new NullLiteralExpr()), comment)))
  }

  def decompileBody(owner: String, classNode: ClassNode, i: Int, method: MethodNode, j: Int, methods: Int, declaration: BodyDeclaration[_ <: BodyDeclaration[_]]): Unit = {
    this.logger.debug("!!!!!!!!!!!!")
    this.logger.info(f"Decompiling [${i + 1} of ${VFS.classes.size}] ${classNode.name} [${j + 1} of $methods] ${method.name} (signature = ${method.signature}, descriptor = ${method.desc})")

    if (method.instructions.size == 0) {
      this.logger.debug("**** Method is empty ****")

      def warningBody(warning: String): BlockStmt = {
        new BlockStmt(new NodeList(JavaParser.setComment(new ThrowStmt(new NullLiteralExpr()), new BlockComment(f" Decompilation warning: $warning"))))
      }

      declaration match {
        case declaration: InitializerDeclaration =>
          val warning = f"no implementation in static initializer for class ${classNode.name}"
          this.logger.warn(warning)
          declaration.setBody(warningBody(warning))
        case declaration: ConstructorDeclaration =>
          // TODO: factor out code (but setbody is not same for method)
          val warning = f"no implementation in constructor ${classNode.name}(signature = ${method.signature}, descriptor = ${method.desc})"
          this.logger.warn(warning)
          declaration.setBody(warningBody(warning))
        case declaration: MethodDeclaration =>
          val modifiers = declaration.getModifiers
          // TODO: if !(*.sym)
          if (modifiers.contains(Modifier.abstractModifier) || modifiers.contains(Modifier.nativeModifier)) {
            declaration.setBody(null)
          } else {
            val warning = s"no implementation in non-abstract, non-native method: ${classNode.name}.${method.name}(signature = ${method.signature}, descriptor = ${method.desc})"
            this.logger.warn(warning)
            declaration.setBody(warningBody(warning))
          }
      }
    } else {
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
    }
  }
}
