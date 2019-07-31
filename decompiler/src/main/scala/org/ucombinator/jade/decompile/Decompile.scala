package org.ucombinator.jade.decompile

import java.io.{PrintWriter, StringWriter}
import java.nio.file.Path

import com.github.javaparser.ast.CompilationUnit
import com.github.javaparser.ast.body.BodyDeclaration
import org.objectweb.asm.ClassReader
import org.objectweb.asm.tree.{ClassNode, MethodNode}
import org.objectweb.asm.util.{Textifier, TraceClassVisitor}
import org.ucombinator.jade.util.{Logging, VFS}

import scala.collection.JavaConverters._
import scala.collection.mutable

// TODO: nested class?
// TODO: error message
// TODO: load flag
// TODO: support stdin for files to decompile
// TODO: skip over ct.jar as it is just signatures.  Maybe don't skip second load if it is better.
case object Decompile extends Logging {
  private val asmLogger = childLogger("asm")

  val classes = mutable.Map[CompilationUnit, ClassNode]()
  val methods = mutable.Map[BodyDeclaration[_ <: BodyDeclaration[_]], (ClassNode, MethodNode)]()

  def main(paths: List[Path]): Unit = {
    for (path <- paths) {
      VFS.get0(path)
    }
    for (((name, readers), i) <- VFS.classes.zipWithIndex) {
      for ((path, classReader) <- readers) { // TODO: pick "best"
        val (classNode, compilationUnit) = decompileClassFile(name, path.toString, classReader, i)
        for (typ <- compilationUnit.getTypes.iterator().asScala) {
          val members = typ.getMembers.iterator().asScala.flatMap(Decompile.methods.get)
          for (((classNode, methodNode), j) <- members.zipWithIndex) {
            DecompileBody.decompileBody(path.toString, classNode, i, methodNode, j, members.size)
          }
        }
      }
    }
  }

  def decompileClassFile(name: String, owner: String, cr: ClassReader, i: Int): (ClassNode, CompilationUnit) = {
    this.logger.info(f"Decompiling [${i + 1} of ${VFS.classes.size}] $name from $owner") // TODO: name use "." instead of "/" and "$"
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

    (classNode, compilationUnit)
  }
}
