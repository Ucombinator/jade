package org.ucombinator.jade.main

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, IOException, PrintWriter}
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{Files, Path, Paths}
import java.util.stream.Collectors
import java.util.zip.{ZipEntry, ZipInputStream}

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
import scala.collection.mutable

sealed trait LoadState

object LoadState {
  case object Want extends LoadState
  case object DoNotWant extends LoadState
  case class Jar(bytes: Array[Byte]) extends LoadState
  case class Class(bytes: Array[Byte]) extends LoadState
}

// TODO: nested class?

case class LoadTree(var state: LoadState, children: mutable.Map[String, LoadTree])

object Decompile {
  private val ZIP_SIGNATURE = Array(0x50, 0x4b, 0x03, 0x04).map(_.toByte)
  private val JMOD_SIGNATURE = Array(0x4a, 0x4d, 0x01, 0x00, 0x50, 0x4b, 0x03, 0x04).map(_.toByte)
  private val CLASS_SIGNATURE = Array(0xCA, 0xFE, 0xBA, 0xBE).map(_.toByte)
}

// TODO: support stdin for files to decompile
// TODO: support `/` to select jar components (and recursive on those components)
case class Decompile(printAsm: Boolean, printJavaParser: Boolean, printMethods: Boolean) {

  val classReaders = mutable.Map[String, ClassReader]()

//  type Result = List[(Path, List[String], ClassNode, CompilationUnit)]
  def main(paths: List[Path]): Unit = {
//    for (path <- paths) {
//      loadPath(path, List())
//    }
    for (path <- paths) {
      println()
      println(f"++++++ $path")
      VFS.get0(path)
    }
  }
  def loadPath(path: Path, rest: List[Path]): Unit = {
    // TODO: atomic operations on files
    val attributes = try {
      Files.readAttributes(path, classOf[BasicFileAttributes])
    } catch {
      case ioe: IOException =>
        // Maybe the user is using nested path syntax
        path.getParent match {
          case null => // Could not find the file
            return // TODO: info warning
          case p => // Nested path syntax
            loadPath(path.getParent, path.getFileName :: rest)
            return
        }
    }
    if (attributes.isRegularFile) {
      loadBytes(path, rest, Files.readAllBytes(path))
    } else if (attributes.isDirectory) {
      if (rest.nonEmpty) {
        // TODO: desired path does not exist
        // Do nothing
      } else {
        for (p <- Files.list(path).collect(Collectors.toList[Path]).asScala) {
          loadPath(p, rest)
        }
      }
    } else {
      // TODO: check rest.nonEmpty
      println(f"skipping $path ($rest)") // TODO
    }
  }
  /*
  def loadPath(path: Path): Unit = {
    if (Files.isRegularFile(path)) {
      loadBytes(path, List(), Files.readAllBytes(path))
    } else if (Files.isDirectory(path)) {
      // TODO: `try` for directory list
      for (p <- Files.list(path).collect(Collectors.toList[Path]).asScala) {
        loadPath(p)
      }
    } else { // TODO if exists
      println(f"skipping $path") // TODO
      List()
    }
  }
   */
  def loadBytes(path: Path, rest: List[Path], bytes: Array[Byte]): Unit = {
    if (bytes.startsWith(Decompile.ZIP_SIGNATURE)) {
      loadZip(path, rest, bytes, 0) // zip file
    } else if (bytes.startsWith(Decompile.JMOD_SIGNATURE)) {
      loadZip(path, rest, bytes, 4) // jmod file
    } else if (bytes.startsWith(Decompile.CLASS_SIGNATURE)) {
      loadClass(path, rest, bytes) // class file
    } else {
      // TODO: (if flag enabled) report unsupported file type
    }
  }

  def loadClass(path: Path, rest: List[Path], bytes: Array[Byte]): Unit = {
    if (rest.nonEmpty) {
      // TODO: error message
      // For nested classes, use A$B (i.e., so you name the actual file)
      println(f"")
    } else {
      val classReader = new ClassReader(bytes)
      classReaders += classReader.getClassName -> classReader
    }
    // TODO: recur on nested class names
    // TODO: load flag
  }

  def loadZip(path: Path, rest: List[Path], bytes: Array[Byte], offset: Int): Unit = {
    // TODO: (if flag enabled) println(f"zip: $path $subpath")
    val zipInputStream = new ZipInputStream(new ByteArrayInputStream(bytes, offset, bytes.length - offset))
    // TODO: multi-version jar
    // TODO: figure out jmod files
    // NOTE: using ZipInputStream instead of JarInputStream so we can also handle `.jmod` files
    // NOTE: using ZipInputStream instead of ZipFile so we can also handle recursive zip/jar files
    val array = new Array[Byte](4 * 1024)
    var entry: ZipEntry = null
    while ({entry = zipInputStream.getNextEntry; entry != null}) {
      if (!entry.isDirectory) {
        val builder = new ByteArrayOutputStream()
        var len = -1
        while ({len = zipInputStream.read(array); len != -1}) {
          builder.write(array, 0, len)
        }
        loadBytes(path, rest :+ Paths.get(entry.getName), builder.toByteArray)
      }
    }
    zipInputStream.close()
  }

  def decompileClassFile(path: Path, subpath: List[String], bytes: Array[Byte]): (Path, List[String], ClassNode, CompilationUnit) = {
    val owner = (path.toString :: subpath).mkString("!")

    val classNode = new ClassNode
    val cr = new ClassReader(bytes)
    cr.accept(classNode, 0)

    if (classNode.name == null) { return (path, subpath, null, null) }
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

    (path, subpath, classNode, compilationUnit)
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
