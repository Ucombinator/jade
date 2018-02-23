package org.ucombinator.jade

import java.nio.file.{Files, Path, Paths}
import java.util.jar.JarFile
import java.util.jar.JarEntry


class Decompiler(val jarFile: JarFile, val classFiles: Iterator[JarEntry]) {
  def decompileToFiles() : Unit =
    for ((k, grp) <- classFilesGroupedBySourceFile) {
      val bytes: List[Byte] = decompile(sortClassFilesByLineNumber(grp))
      val path: Path = Paths.get(k)
      Files.write(path, bytes.toArray)
    }

  lazy val classFilesGroupedBySourceFile: Map[String, Set[JarEntry]] = {
    // TODO: body

    // TODO: replace mock
    Map("a" -> Set.empty)
  }

  private def sortClassFilesByLineNumber(st: Set[JarEntry])
  : List[JarEntry] = {
    // 2. sort by line number
    classFiles.toList.sortBy(getStartLineNumber)
  }

  private def getStartLineNumber(jarEntry: JarEntry)
  : Long = {
    // TODO: Replace the placeholder
    3.toLong
  }

  private def decompile(classFiles: List[JarEntry])
  : List[Byte] =
    classFiles.flatMap(decompileOne)

  // TODO: All details
  private def decompileOne(jarEntry: JarEntry): List[Byte] = {
    // Use ASM
    // class file --> jvm instructions or ASM class file representations
    // jvm instructions --> decompiled java code
    // TODO: replace placeholder
    List(1.toByte)
  }
}
