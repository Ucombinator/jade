package org.ucombinator.jade.main

import java.io.{PrintWriter, StringWriter}
import java.nio.file.Path

import com.github.javaparser.ast.CompilationUnit
import org.objectweb.asm.ClassReader
import org.objectweb.asm.tree._
import org.objectweb.asm.util.{Textifier, TraceClassVisitor}
import org.ucombinator.jade.decompile.DecompileClass
import org.ucombinator.jade.decompile.method.ControlFlowGraph
import org.ucombinator.jade.decompile.method.ssa.SSA
import org.ucombinator.jade.util.{Logging, VFS}
import org.ucombinator.jade.util.asm.Insn
import org.ucombinator.jade.util.jgrapht.{Dominator, GraphViz}

import scala.collection.JavaConverters._

// TODO: nested class?
// TODO: error message
// TODO: load flag
// TODO: support stdin for files to decompile
// TODO: support `/` to select jar components (and recursive on those components)
// TODO: (if flag enabled) report unsupported file type
case object Decompile extends Logging {
  private lazy val asmLogger = childLogger("asm")
  private lazy val javaLogger = childLogger("java")
  private lazy val methodsLogger = childLogger("methods")

  def main(paths: List[Path]): Unit = {
    for (path <- paths) {
      VFS.get0(path)
    }
    for ((name, (path, classReader)) <- VFS.classes) {
      decompileClassFile(name, path.toString, classReader)
    }
  }

  def decompileClassFile(name: String, owner: String, cr: ClassReader): (ClassNode, CompilationUnit) = {
    this.logger.info(f"Decompiling $name from $owner") // TODO: name use "." instead of "/" and "$"
    val classNode = new ClassNode
    cr.accept(classNode, 0)

    if (classNode.name == null) { return (null, null) } // TODO
    this.logger.debug(classNode.name)

    this.asmLogger.whenDebugEnabled({
      val stringWriter = new StringWriter()
      classNode.accept(new TraceClassVisitor(null, new Textifier(), new PrintWriter(stringWriter)))
      this.asmLogger.debug(stringWriter.toString)
    })

    val compilationUnit = DecompileClass.decompile(classNode)

    this.javaLogger.debug(compilationUnit.toString)

    // TODO: classNode.sourceFile, classNode.sourceDebug
    // TODO: classNode.outerClass, classNode.outerMethod, classNode.outerMethodDesc
    // TODO: Inner classes
    val inners: List[InnerClassNode] = classNode.innerClasses.asScala.toList

    inners.foreach(c => this.methodsLogger.debug(c.name))

    for (method <- classNode.methods.asScala) {
      decompileMethod(owner, classNode, method)
    }

    (classNode, compilationUnit)
  }

  def decompileMethod(owner: String, classNode: ClassNode, method: MethodNode): Unit = {
    // TODO: abstract and native
    // TODO: signature .sym and has no method body
    // TODO: identify extent of exception handlers (basically things dominated by exception handler entry)
    this.methodsLogger.debug("!!!!!!!!!!!!")
    this.methodsLogger.debug(f"method: ${method.name} ${method.signature} ${method.desc}")

    if (method.instructions.size == 0) {
      // TODO: abstract/native vs signature (cl.sym)
      this.methodsLogger.debug("**** Method is empty ****")
    } else {
      this.methodsLogger.debug("**** ControlFlowGraph ****")

      val cfg = ControlFlowGraph(owner, method)

      this.methodsLogger.debug(GraphViz.toString(cfg))
      for (v <- cfg.graph.vertexSet().asScala) {
        this.methodsLogger.debug(f"v: ${cfg.graph.incomingEdgesOf(v).size()}: $v")
      }

      this.methodsLogger.debug("**** SSA ****")
      val ids = SSA(owner, method, cfg)

      this.methodsLogger.debug("**** Dominators ****")
      val doms = Dominator.dominatorTree(cfg.graphWithExceptions, cfg.entry)

      this.methodsLogger.debug("frames: " + ids.frames.length)
      for (i <- 0 until method.instructions.size) {
        this.methodsLogger.debug(f"frame($i): ${ids.frames(i)}")
      }

      this.methodsLogger.debug("results and arguments")
      for (i <- 0 until method.instructions.size) {
        val insn = method.instructions.get(i)
        this.methodsLogger.debug(f"args($i): ${Insn.longString(method, insn)} --- ${ids.instructionArguments.get(insn)}")
      }

      this.methodsLogger.debug("ssa")
      for ((key, value) <- ids.ssaMap) {
        this.methodsLogger.debug(s"ssa: $key -> $value")
      }

      this.methodsLogger.debug("doms")
      //this.methodsLogger.debug(GraphViz.toString(doms))
      this.methodsLogger.debug(Dominator.dominatorNesting(cfg.graphWithExceptions, doms, cfg.entry))
    }
  }
}
