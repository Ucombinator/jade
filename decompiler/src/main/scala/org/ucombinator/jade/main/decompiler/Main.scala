package org.ucombinator.jade.main.decompiler

import java.io.File
import java.nio.file.{Files, Path, Paths}
import java.util.jar.{JarEntry, JarFile}

import scala.collection.JavaConverters._

object Main {
  def main(appName: String, destinationDirectoryName: String): Unit = {
    // TODO: accept a directory that includes .class files
    validityCheck(appName, destinationDirectoryName)

    val projectDirectory: Path = {
      val appBaseName = appName.split("/").last.stripSuffix(".jar")

      Paths.
        get(destinationDirectoryName, appBaseName).
        toAbsolutePath
    }

    createProjectDirectory(projectDirectory.toFile)
    val jar = new JarFile(appName)
    processJar(jar, projectDirectory)
  }


  private def validityCheck(appName: String, destinationDirectoryName: String)
  : Unit = {
    require(appName.endsWith(".jar"), "The first argument is NOT a .jar file!")

    val app = new File(appName)

    require(app.exists, s"The $appName file CANNOT be found!")
    require(app.canRead, s"The $appName file CANNOT be read!")

    val destinationDirectory = new File(destinationDirectoryName)

    require(destinationDirectory.exists, "The given destination (the second argument) does NOT exist!")
    require(destinationDirectory.isDirectory, "The given destination (the second argument) is NOT a directory!")
    require(destinationDirectory.canWrite, "The given destination directory (the second argument) is NOT writable!")
  }

  private def createProjectDirectory(directory: File)
  : Unit = {
    // TODO: Ask the user to decide what to do about this exist same name folder
    //if (directory.exists) { FileUtils.deleteDirectory(directory) }
    assert(directory.mkdir(), s"CANNOT create folder $directory!")
  }

  private def processJar(jarFile: JarFile, projectDirectory: Path)
  : Unit = {
    val (directories, files) = jarFile.entries.asScala.partition(_.isDirectory)
    cloneJarFileDirectoryStructure(directories, projectDirectory)

    val (classFiles, nonClassFiles) = files.partition(_.getName.endsWith(".class"))
    cloneNonClassFilesInJar(jarFile, nonClassFiles, projectDirectory.toString)

    val decompiler = new org.ucombinator.jade.main.decompiler.Main(jarFile, classFiles)
    decompiler.decompileToFiles()
  }

  private def cloneJarFileDirectoryStructure(directories: Iterator[JarEntry], outputDirectory: Path)
  : Unit =
    for (d <- directories) {
      // TODO: Create this annotation
      // @CanIgnoreReturnValue
      Files.createDirectory(Paths.get(outputDirectory.toString, d.getName))
    }

  private def cloneNonClassFilesInJar(jarFile: JarFile, nonClassFiles: Iterator[JarEntry], outputDirectoryName: String)
  : Unit =
      for (d <- nonClassFiles;
           path = Paths.get(outputDirectoryName, d.getName)) {
        Files.copy(jarFile.getInputStream(d), path, java.nio.file.StandardCopyOption.REPLACE_EXISTING)
      }
}

class Main(val jarFile: JarFile, val classFiles: Iterator[JarEntry]) {
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
