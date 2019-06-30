package org.ucombinator.jade.main

import java.io.PrintWriter
import java.nio.file.Path

import com.github.javaparser.ast.CompilationUnit
import org.objectweb.asm.ClassReader
import org.objectweb.asm.tree._
import org.objectweb.asm.util.{Textifier, TraceClassVisitor}
import org.ucombinator.jade.decompile.DecompileClass
import org.ucombinator.jade.decompile.method.ControlFlowGraph
import org.ucombinator.jade.decompile.method.ssa.SSA
import org.ucombinator.jade.util.VFS
import org.ucombinator.jade.util.asm.Insn
import org.ucombinator.jade.util.jgrapht.{Dominator, GraphViz}

import scala.collection.JavaConverters._

// TODO: nested class?
// TODO: error message
// TODO: load flag
// TODO: support stdin for files to decompile
// TODO: support `/` to select jar components (and recursive on those components)
// TODO: (if flag enabled) report unsupported file type
case class Decompile(printAsm: Boolean, printJavaParser: Boolean, printMethods: Boolean) {
  def main(paths: List[Path]): Unit = {
    for (path <- paths) {
      VFS.get0(path)
    }
    for ((name, (path, classReader)) <- VFS.classes) {
      decompileClassFile(path.toString, classReader)
    }
  }

  def decompileClassFile(owner: String, cr: ClassReader): (ClassNode, CompilationUnit) = {
    val classNode = new ClassNode
    cr.accept(classNode, 0)

    if (classNode.name == null) { return (null, null) } // TODO
    println(classNode.name)

    if (printAsm) {
      val traceClassVisitor = new TraceClassVisitor(null, new Textifier(), new PrintWriter(System.out))
      classNode.accept(traceClassVisitor)
    }

    val compilationUnit = DecompileClass.decompile(classNode)

    if (printJavaParser) {
      println(compilationUnit)
    }

    // TODO: classNode.sourceFile, classNode.sourceDebug
    // TODO: classNode.outerClass, classNode.outerMethod, classNode.outerMethodDesc
    // TODO: Inner classes
    val inners: List[InnerClassNode] = classNode.innerClasses.asScala.toList
    if (printMethods) {
      inners.foreach { c =>
        println(c.name)
      }
    }

    for (method <- classNode.methods.asScala) {
      decompileMethod(owner, classNode, method)
    }

    (classNode, compilationUnit)
  }

  def decompileMethod(owner: String, classNode: ClassNode, method: MethodNode): Unit = {
    // TODO: abstract and native
    // TODO: signature .sym and has no method body
    // TODO: identify extent of exception handlers (basically things dominated by exception handler entry)
    if (printMethods) {
      println("!!!!!!!!!!!!")
      println(f"method: ${method.name} ${method.signature} ${method.desc}")
    }
    if (method.instructions.size == 0) {
      // TODO: abstract/native vs signature (cl.sym)
      if (printMethods) {
        //println("**** Method is empty ****")
      }
    } else {
      if (printMethods) {
        println("**** ControlFlowGraph ****")
      }
      val cfg = ControlFlowGraph(owner, method)
      if (printMethods) {
        println(GraphViz.toString(cfg))
        for (v <- cfg.graph.vertexSet().asScala) {
          println(f"v: ${cfg.graph.incomingEdgesOf(v).size()}: $v")
        }
        println("**** SSA ****")
      }
      val ids = SSA(owner, method, cfg)

      if (printMethods) {
        println("**** Dominators ****")
      }

      val doms = Dominator.dominatorTree(cfg.graphWithExceptions, cfg.entry)

      if (printMethods) {
        println("frames: " + ids.frames.length)
        for (i <- 0 until method.instructions.size) {
          println(f"frame($i): ${ids.frames(i)}")
        }

        println("results and arguments")
        for (i <- 0 until method.instructions.size) {
          val insn = method.instructions.get(i)
          println(f"args($i): ${Insn.longString(method, insn)} --- ${ids.instructionArguments.get(insn)}")
        }

        println("ssa")
        for ((key, value) <- ids.ssaMap) {
          println(s"ssa: $key -> $value")
        }

        println("doms")
        //println(GraphViz.toString(doms))
        println(Dominator.dominatorNesting(cfg.graphWithExceptions, doms, cfg.entry))
      }
    }
  }
}
