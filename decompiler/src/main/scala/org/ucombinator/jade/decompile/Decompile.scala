package org.ucombinator.jade.decompile

import java.io.{PrintWriter, StringWriter}
import java.nio.file.Path

import com.github.javaparser.ast.CompilationUnit
import com.github.javaparser.ast.body.BodyDeclaration
import org.objectweb.asm.ClassReader
import org.objectweb.asm.tree.{ClassNode, MethodNode}
import org.objectweb.asm.util.{Textifier, TraceClassVisitor}
import org.ucombinator.jade.util.{Log, VFS}

import scala.jdk.CollectionConverters._
import scala.collection.mutable

// TODO: nested class?
// TODO: error message
// TODO: load flag
// TODO: support stdin for files to decompile
// TODO: skip over ct.jar as it is just signatures.  Maybe don't skip second load if it is better.
case object Decompile extends Log {
  private val asmLog = childLog("asm")

  val classes = mutable.Map[CompilationUnit, ClassNode]()
  val methods = mutable.Map[BodyDeclaration[_ <: BodyDeclaration[_]], (ClassNode, MethodNode)]()

  def main(paths: List[Path]): Unit = {
    for (path <- paths) {
      VFS.get0(path)
    }
    for (((name, readers), i) <- VFS.classes.zipWithIndex) {
      for ((path, classReader) <- readers) { // TODO: pick "best" classReader
        // Decompile class structure
        val compilationUnit = decompileClassFile(name, path.toString, classReader, i)

        // Decompile method bodies
        for (typ <- compilationUnit.getTypes.iterator().asScala) {
          val members = typ.getMembers.iterator().asScala.flatMap(x => Decompile.methods.get(x).map((_, x))).toList
          for ((((classNode, methodNode), bodyDeclaration), j) <- members.zipWithIndex) {
            DecompileMethodBody.decompileBody(path.toString, classNode, i, methodNode, j, members.size, bodyDeclaration)
          }
        }

        this.log.debug(f"compilationUnit\n${compilationUnit}")
      }
    }
  }

  def decompileClassFile(name: String, owner: String, cr: ClassReader, i: Int): CompilationUnit = {
    // TODO: name use "." instead of "/" and "$"
    this.log.info(f"Decompiling [${i + 1} of ${VFS.classes.size}] ${name} from ${owner}")
    val classNode = new ClassNode
    cr.accept(classNode, 0) // TODO: ClassReader.EXPAND_FRAMES

    if (classNode.name == null) { return null } // TODO
    this.log.debug("class name: " + classNode.name)

    this.asmLog.whenDebugEnabled({
      val stringWriter = new StringWriter()
      classNode.accept(new TraceClassVisitor(null, new Textifier(), new PrintWriter(stringWriter)))
      this.asmLog.debug("++++ asm ++++\n" + stringWriter.toString)
    })

    DecompileClass.decompileClass(classNode)
  }
}
