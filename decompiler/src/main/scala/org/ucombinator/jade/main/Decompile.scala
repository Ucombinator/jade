package org.ucombinator.jade.main

import org.objectweb.asm.ClassReader
import org.objectweb.asm.tree._
import org.objectweb.asm.util.{Textifier, TraceClassVisitor}
import org.ucombinator.jade.util.asm.Instructions
import org.ucombinator.jade.decompile.method.ControlFlowGraph
import org.ucombinator.jade.decompile.method.ssa.SSA
import java.io.{File, PrintWriter}
import java.nio.file.Files

import org.ucombinator.jade.decompile.DecompileClass

import scala.collection.JavaConverters._

object Decompile {
  def main(printAsm: Boolean, printJavaParser: Boolean, printMethods: Boolean, fileNames: List[File]): Unit = {
    for (fileName <- fileNames) {
      println(fileName)
      main(printAsm, printJavaParser, printMethods, fileName)
    }
  }

  def main(printAsm: Boolean, printJavaParser: Boolean, printMethods: Boolean, fileName: File): Unit = {
    require(fileName != null, "the given class file name is actually `null`!")

    val byteArray = Files.readAllBytes(fileName.toPath) //Full path class name

    val cn = new ClassNode
    val cr = new ClassReader(byteArray)

    cr.accept(cn, 0)

    if (printAsm) {
      val traceClassVisitor = new TraceClassVisitor(null, new Textifier(), new PrintWriter(System.out))
      cn.accept(traceClassVisitor)
    }

    val cu = DecompileClass.asmToJavaParser(cn)
    if (printJavaParser) {
      println(cu)
    }

    if (printMethods) {
      // TODO: cn.sourceFile, cn.sourceDebug
      // TODO: cn.outerClass, cn.outerMethod, cn.outerMethodDesc
      // TODO: Inner classes
      val inners: List[InnerClassNode] = cn.innerClasses.asScala.toList
      inners.foreach { c =>
        println(c.name)
      }

      for (method <- cn.methods.asScala) {
        println("!!!!!!!!!!!!")
        println(f"method: ${method.name} ${method.signature} ${method.desc}")
        println("**** ControlFlowGraph ****")
        val cfg = ControlFlowGraph(fileName.toString, method)
        for (v <- cfg.graph.vertexSet().asScala) {
          println(f"v: ${method.instructions.indexOf(v)} ${cfg.graph.incomingEdgesOf(v).size()}: $v")
        }
        println("**** SSA ****")
        val ids = SSA(fileName.toString, method, cfg)

        println("frames: " + ids.frames.length)
        for (i <- 0 until method.instructions.size) {
          println(f"frame($i): ${ids.frames(i)}")
        }

        println("results and arguments")
        for (i <- 0 until method.instructions.size) {
          val insn = method.instructions.get(i)
          println(f"args($i): ${Instructions.longInsnString(method.instructions, insn)} --- ${ids.instructionArguments.get(insn)}")
        }

        println("ssa")
        for ((key, value) <- ids.ssaMap) {
          println(s"ssa: $key -> $value")
        }
      }
    }
  }
}
