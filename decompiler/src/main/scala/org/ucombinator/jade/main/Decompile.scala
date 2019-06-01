package org.ucombinator.jade.main

import org.objectweb.asm.ClassReader
import org.objectweb.asm.tree._
import org.objectweb.asm.util.{Textifier, TraceClassVisitor}
import org.ucombinator.jade.util.asm.Instructions
import org.ucombinator.jade.decompile.method.ControlFlowGraph
import org.ucombinator.jade.decompile.method.ssa.SSA
import java.io.{ByteArrayInputStream, ByteArrayOutputStream, PrintWriter}
import java.nio.file.{Files, Path}
import java.util.stream.Collectors
import java.util.zip.{ZipEntry, ZipInputStream}

import com.github.javaparser.ast.CompilationUnit
import org.ucombinator.jade.decompile.DecompileClass

import scala.collection.JavaConverters._

case class Decompile(printAsm: Boolean, printJavaParser: Boolean, printMethods: Boolean) {
  type Result = List[(Path, List[String], ClassNode, CompilationUnit)]
  def main(paths: List[Path]): Unit = {
    for (path <- paths) {
      decompilePath(path)
    }
  }
  def decompilePath(path: Path): Result = {
    if (Files.isRegularFile(path)) {
      decompileBytes(path, List(), Files.readAllBytes(path))
    } else if (Files.isDirectory(path)) {
      val children =
        for (p <- Files.list(path).collect(Collectors.toList[Path]).asScala)
        yield { decompilePath(p) }
      children.toList.flatten
    } else {
      println(f"skipping $path") // TODO
      List()
    }
  }
  def decompileBytes(path: Path, subpath: List[String], bytes: Array[Byte]): Result = {
    if (bytes.startsWith(Array(0x50, 0x4b, 0x03, 0x04).map(_.toByte))) {
      decompileZip(path, subpath, bytes, 0) // zip file
    } else if (bytes.startsWith(Array(0x4a, 0x4d, 0x01, 0x00, 0x50, 0x4b, 0x03, 0x04).map(_.toByte))) {
      decompileZip(path, subpath, bytes, 4) // jmod file
    } else if (bytes.startsWith(Array(0xCA, 0xFE, 0xBA, 0xBE).map(_.toByte))) {
      List(decompileClassFile(path, subpath, bytes)) // class file
    } else {
      List() // unsupported file type
    }
  }
  def decompileZip(path: Path, subpath: List[String], bytes: Array[Byte], offset: Int): Result = {
    val zipInputStream = new ZipInputStream(new ByteArrayInputStream(bytes, offset, bytes.length - offset))
    // TODO: multi-version jar
    // TODO: figure out jmod files
    // not using jar file because jmod files are zip files
    // new Manifest
    val array = new Array[Byte](4 * 1024)
    var entry: ZipEntry = null
    var entries: Result = List()
    // TODO: use yeild?
    while ({entry = zipInputStream.getNextEntry; entry != null}) {
      if (!entry.isDirectory) {
        val builder = new ByteArrayOutputStream()
        var len = -1
        while ({len = zipInputStream.read(array); len != -1}) {
          builder.write(array, 0, len)
        }
        val bytes = builder.toByteArray
        entries ++= decompileBytes(path, subpath :+ entry.getName, bytes)
      }
    }
    zipInputStream.close()
    entries
  }

  def decompileClassFile(path: Path, subpath: List[String], bytes: Array[Byte]): (Path, List[String], ClassNode, CompilationUnit) = {
    val owner = (path.toString :: subpath).mkString("!")

    val classNode = new ClassNode
    val cr = new ClassReader(bytes)
    cr.accept(classNode, 0)

    if (classNode.name == null) { return (path, subpath, null, null) }

    if (printAsm) {
      val traceClassVisitor = new TraceClassVisitor(null, new Textifier(), new PrintWriter(System.out))
      classNode.accept(traceClassVisitor)
    }

    val compilationUnit = DecompileClass.decompile(classNode)

    if (printJavaParser) {
      println(compilationUnit)
    }

    if (printMethods) {
      // TODO: classNode.sourceFile, classNode.sourceDebug
      // TODO: classNode.outerClass, classNode.outerMethod, classNode.outerMethodDesc
      // TODO: Inner classes
      val inners: List[InnerClassNode] = classNode.innerClasses.asScala.toList
      inners.foreach { c =>
        println(c.name)
      }

      for (method <- classNode.methods.asScala) {
        // TODO: identify extent of exception handlers (basically things dominated by exception handler entry)
        println("!!!!!!!!!!!!")
        println(f"method: ${method.name} ${method.signature} ${method.desc}")
        println("**** ControlFlowGraph ****")
        val cfg = ControlFlowGraph(owner, method)
        for (v <- cfg.graph.vertexSet().asScala) {
          println(f"v: ${method.instructions.indexOf(v)} ${cfg.graph.incomingEdgesOf(v).size()}: $v")
        }
        println("**** SSA ****")
        val ids = SSA(owner, method, cfg)

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

    (path, subpath, classNode, compilationUnit)
  }
}
