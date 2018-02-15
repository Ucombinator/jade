package org.ucombinator.jade

import java.io.File
import java.nio.file.{Files, Path, Paths}
import java.util.jar.{JarEntry, JarFile}

import org.apache.commons.io.FileUtils

import scala.collection.JavaConverters._

class MainConf(args: Seq[String]) extends JadeScallopConf(args = args) {
  val help = opt[Unit](short = 'h', descr = "show this help message")(HelpConverter)

  val jarFile = trailArg[String]()
  val destinationFolder = trailArg[String]()

  verify()
}

object Main {
  def main(args: Array[String])
  : Unit = {
    val conf = new MainConf(args)
    println("jarFile: " + conf.jarFile())
    println("destinationFolder: " + conf.destinationFolder())

    // TODO: accept a directory that includes .class files
    require(args.length == 2, "Usage: <this program> <.jar file> <destination folder>")
    val appName: String = args(0)
    val destinationDirectoryName: String = args(1)
    validityCheck(appName, destinationDirectoryName)

    val projectDirectory: Path = {
      val appBaseName = appName.split("/").last.stripSuffix(".jar")

      Paths.
        get(destinationDirectoryName, appBaseName).
        toAbsolutePath
    }

    createProjectDirectory(projectDirectory.toFile)
    val jarFile = new JarFile(appName)
    processJar(jarFile, projectDirectory)
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
    if (directory.exists) { FileUtils.deleteDirectory(directory) }
    assert(directory.mkdir(), s"CANNOT create folder $directory!")
  }

  private def processJar(jarFile: JarFile, projectDirectory: Path)
  : Unit = {
    val (directories, files) = jarFile.entries.asScala.partition(_.isDirectory)
    cloneJarFileDirectoryStructure(directories, projectDirectory)

    val (classFiles, nonClassFiles) = files.partition(_.getName.endsWith(".class"))
    cloneNonClassFilesInJar(jarFile, nonClassFiles, projectDirectory.toString)

    val decompiler: Decompiler = new Decompiler(jarFile, classFiles)
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
