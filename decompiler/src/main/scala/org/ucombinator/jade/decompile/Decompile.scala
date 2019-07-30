package org.ucombinator.jade.decompile

import java.io.{PrintWriter, StringWriter}
import java.nio.file.Path

import com.github.javaparser.ast.CompilationUnit
import org.objectweb.asm.ClassReader
import org.objectweb.asm.tree.{ClassNode, InnerClassNode, MethodNode}
import org.objectweb.asm.util.{Textifier, TraceClassVisitor}
import org.ucombinator.jade.decompile.method.ControlFlowGraph
import org.ucombinator.jade.decompile.method.ssa.SSA
import org.ucombinator.jade.util.asm.Insn
import org.ucombinator.jade.util.jgrapht.{Dominator, GraphViz}
import org.ucombinator.jade.util.{Logging, VFS}

import scala.collection.JavaConverters._

// TODO: nested class?
// TODO: error message
// TODO: load flag
// TODO: support stdin for files to decompile
// TODO: skip over ct.jar as it is just signatures.  Maybe don't skip second load if it is better.
case object Decompile extends Logging {
  private val asmLogger = childLogger("asm")
  private val javaLogger = childLogger("java")
  private val methodsLogger = childLogger("methods") // TODO: rename to methodLogger?
  private val domLogger = childLogger("dom")

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
    // TODO: put number (e.g., [n of m]) of class
    val classNode = new ClassNode
    cr.accept(classNode, 0)

    if (classNode.name == null) { return (null, null) } // TODO
    this.logger.debug("class name: " + classNode.name)

    this.asmLogger.whenDebugEnabled({
      val stringWriter = new StringWriter()
      classNode.accept(new TraceClassVisitor(null, new Textifier(), new PrintWriter(stringWriter)))
      this.asmLogger.debug("++++ asm ++++\n" + stringWriter.toString)
    })

    val compilationUnit = DecompileClass.decompileClass(classNode)

    this.javaLogger.debug("++++ decompile class ++++\n" + compilationUnit.toString)

    // TODO: classNode.sourceFile, classNode.sourceDebug
    // TODO: classNode.outerClass, classNode.outerMethod, classNode.outerMethodDesc
    // TODO: Inner classes
    val inners: List[InnerClassNode] = classNode.innerClasses.asScala.toList

    inners.foreach(c => this.methodsLogger.debug("inner class: " + c.name))

    for (method <- classNode.methods.asScala) {
      decompileMethod(owner, classNode, method)
    }

    (classNode, compilationUnit)
  }

  def decompileMethod(owner: String, classNode: ClassNode, method: MethodNode): Unit = {
    this.methodsLogger.debug("!!!!!!!!!!!!")
    this.methodsLogger.info(f"Decompiling ${classNode.name}.${method.name} (signature = ${method.signature}, descriptor = ${method.desc})")
    // TODO: put number (e.g., [n of m]) of method (and class)
    // TODO: abstract and native
    // TODO: signature .sym and has no method body
    // TODO: identify extent of exception handlers (basically things dominated by exception handler entry)

    if (method.instructions.size == 0) {
      // TODO: abstract/native vs signature (cl.sym)
      this.methodsLogger.debug("**** Method is empty ****")
    } else {
      this.methodsLogger.debug("**** ControlFlowGraph ****")

      val cfg = ControlFlowGraph(owner, method)

      this.methodsLogger.debug("++++ cfg ++++\n" + GraphViz.toString(cfg))
      for (v <- cfg.graph.vertexSet().asScala) {
        this.methodsLogger.debug(f"v: ${cfg.graph.incomingEdgesOf(v).size()}: $v")
      }

      this.methodsLogger.debug("**** SSA ****")
      val ids = SSA(owner, method, cfg)

      this.methodsLogger.debug("++++ frames: " + ids.frames.length + " ++++")
      for (i <- 0 until method.instructions.size) {
        this.methodsLogger.debug(f"frame($i): ${ids.frames(i)}")
      }

      this.methodsLogger.debug("++++ results and arguments ++++")
      for (i <- 0 until method.instructions.size) {
          val insn = method.instructions.get(i)
        this.methodsLogger.debug(f"args($i): ${Insn.longString(method, insn)} --- ${ids.instructionArguments.get(insn)}")
      }

      this.methodsLogger.debug("++++ ssa map ++++")
      for ((key, value) <- ids.ssaMap) {
        this.methodsLogger.debug(s"ssa: $key -> $value")
      }

      this.domLogger.debug("**** Dominators ****")
      val doms = Dominator.dominatorTree(cfg.graphWithExceptions, cfg.entry)

      this.domLogger.debug("++++ dominator tree ++++\n"+
        GraphViz.toString(doms))

      this.domLogger.debug("++++ dominator nesting ++++\n" +
        GraphViz.nestingTree(cfg.graphWithExceptions, doms, cfg.entry))
    }
  }
}
