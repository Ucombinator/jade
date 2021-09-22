package org.ucombinator.jade.decompile

import java.io.PrintWriter
import java.io.StringWriter
import java.nio.file.Path

import scala.collection.mutable
import scala.jdk.CollectionConverters._

import com.github.javaparser.ast.CompilationUnit
import com.github.javaparser.ast.body.BodyDeclaration
import org.objectweb.asm.ClassReader
import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Opcodes
import org.objectweb.asm.commons.AnalyzerAdapter
import org.objectweb.asm.tree.ClassNode
import org.objectweb.asm.tree.MethodNode
import org.objectweb.asm.util.Textifier
import org.objectweb.asm.util.TraceClassVisitor
import org.ucombinator.jade.util.Log
import org.ucombinator.jade.util.VFS

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
    for (((name, readers), classIndex) <- VFS.classes.zipWithIndex) {
      for ((path, classReader) <- readers) { // TODO: pick "best" classReader
        // TODO: why don't we combine the class and method passes?
        // Decompile class structure
        val compilationUnit = decompileClassFile(name, path.toString, classReader, classIndex)

        // Decompile method bodies
        for (typ <- compilationUnit.getTypes.iterator().asScala) {
          val members = typ.getMembers.iterator().asScala.flatMap(x => Decompile.methods.get(x).map((_, x))).toList
          for ((((classNode, methodNode), bodyDeclaration), methodIndex) <- members.zipWithIndex) {
            this.log.debug("!!!!!!!!!!!!")
            this.log.info(
              f"Decompiling [${classIndex + 1} of ${VFS.classes.size}] ${classNode.name} [${methodIndex + 1} of ${members.size}] ${methodNode.name} (signature = ${methodNode.signature}, descriptor = ${methodNode.desc})"
            )
            DecompileMethodBody.decompileBody(classNode, methodNode, bodyDeclaration)
          }
        }

        this.log.debug(f"compilationUnit\n${compilationUnit}")
      }
    }
  }

  def decompileClassFile(name: String, owner: String, cr: ClassReader, i: Int): CompilationUnit = {
    // TODO: name use "." instead of "/" and "$"
    this.log.info(f"Decompiling [${i + 1} of ${VFS.classes.size}] ${name} from ${owner}")
    val log = this.log
    val classNode = new ClassNode(Opcodes.ASM9) {
      override def visitMethod(
          access: Int,
          name: String,
          descriptor: String,
          signature: String,
          exceptions: Array[String]
      ): MethodVisitor = {
        if (true) {
          super.visitMethod(access, name, descriptor, signature, exceptions)
        } else {
          var aa: AnalyzerAdapter = null
          aa = new AnalyzerAdapter(
            owner,
            access,
            name,
            descriptor,
            new MethodVisitor(Opcodes.ASM9, super.visitMethod(access, name, descriptor, signature, exceptions)) {
              override def visitInsn(opcode: Int): Unit = {
                log.info("AA LOCALS: " + aa.locals)
                log.info("AA STACK: " + aa.stack)
                super.visitInsn(opcode)
              }
            }
          )
          aa
        }
      }
    }
    cr.accept(classNode, ClassReader.EXPAND_FRAMES) // TODO: Do we actually need ClassReader.EXPAND_FRAMES?

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
