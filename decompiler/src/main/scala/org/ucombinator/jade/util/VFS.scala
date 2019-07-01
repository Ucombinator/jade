package org.ucombinator.jade.util

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, IOException}
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{Files, Path}
import java.util.stream.Collectors
import java.util.zip.{ZipEntry, ZipInputStream}

import org.objectweb.asm.ClassReader

import scala.collection.JavaConverters._
import scala.collection.SortedMap

// This code is more complicated than one would think due to a few things.
// The first is supporting treating compressed files as directories.
// The second is recursively loading directory contents.
// The third is trying to minimize the number of filesystem operations.  (Not currently implemented.)
// The logic for these features interact with each other in subtle ways.

// TODO: http, https
// TODO: accumulate errors
// TODO: gzip, tar, bzip2

sealed trait ReadResult
case class RDirectory(children: /*Sorted*/List[PathPosition]) extends ReadResult
case class RFile(bytes: Array[Byte]) extends ReadResult
case object ROther extends ReadResult
case object RNotExist extends ReadResult

sealed trait FileTree
case class FileTreeFile(bytes: Array[Byte]) extends FileTree
case class FileTreeDirectory(children: SortedMap[String, FileTree]) extends FileTree

class PathPosition(basePath: Path, abovePath: List[String], belowPath: /*Reversed*/List[String], fileTree: Option[FileTree]) {
  assert(abovePath.isEmpty || belowPath.isEmpty)
  def apply[A](above: => A, same: => A, below: => A): A = {
    if (abovePath.nonEmpty) { above }
    else if (belowPath.nonEmpty) { below }
    else { same }
  }
  def read: ReadResult = {
    fileTree match {
      case None =>
        val attributes = try {
          Files.readAttributes(path, classOf[BasicFileAttributes])
        } catch {
          case _: IOException => return RNotExist
        }
        if (attributes.isDirectory) {
          if (abovePath.nonEmpty) {
            RDirectory(List())
          } else {
            val children: List[Path] = Files.list(path).collect(Collectors.toList[Path]).asScala.toList.sortBy(_.toString)
            RDirectory(children.map(c => new PathPosition(basePath, List(), c.getFileName.toString :: belowPath, None)))
          }
        } else if (attributes.isRegularFile) {
          RFile(Files.readAllBytes(path))
        } else {
          ROther
        }
      case Some(t) =>
        t match {
          case FileTreeFile(bytes) => RFile(bytes)
          case FileTreeDirectory(children) =>
            abovePath match {
              case p :: ps =>
                RDirectory(children.toList.filter(_._1 == p).map(ft => new PathPosition(basePath.resolve(ft._1), ps, belowPath, Some(ft._2))))
              case List() =>
                RDirectory(children.toList.map(ft => new PathPosition(basePath, List(), ft._1 :: belowPath, Some(ft._2))))
            }
        }
    }
  }
  def path: Path = { belowPath.foldRight(basePath)((s,p) => p.resolve(s) ) }
  def parent: Option[PathPosition] = {
    fileTree match {
      case None =>
        assert(belowPath.isEmpty)
        val parent = path.getParent
        if (parent == null) { None }
        else { Some(new PathPosition(parent, path.getFileName.toString :: abovePath, belowPath, None))}
      case Some(_) => None
    }
  }
  def withFileTree(fileTree: FileTree): PathPosition = {
    new PathPosition(basePath, abovePath, belowPath, Some(fileTree))
  }
}

object VFS {
  private val ZIP_SIGNATURE = Array(0x50, 0x4b, 0x03, 0x04).map(_.toByte)
  private val CLASS_SIGNATURE = Array(0xCA, 0xFE, 0xBA, 0xBE).map(_.toByte)
  private val JMOD_SIGNATURE = Array(0x4a, 0x4d, 0x01, 0x00, 0x50, 0x4b, 0x03, 0x04).map(_.toByte)
  private val JMOD_OFFSET = 4
  def option[A,B](o: Option[A], f: A => B, d: B): B = {
    o match {
      case None => d
      case Some(a) => f(a)
    }
  }
  def oneOrZero[A,B](xs: List[A], f: A => Unit, d: => Unit): Unit = {
    xs match {
      case List() => d
      case List(x) => f(x)
      case _ => error
    }
  }
  def error: Unit = {
    println("TODO: error")
  }
  def ignore: Unit = {
    println("TODO: ignore")
  }
  def add(path: List[String], fileTree: FileTree, bytes: Array[Byte]): FileTree = {
    path match {
      case List() =>
        fileTree match {
          case null => FileTreeFile(bytes)
          case _ => throw new Exception("TODO: bad add")
        }
      case p :: ps =>
        fileTree match {
          case FileTreeFile(_) => throw new Exception("TODO: bad add 2")
          case null => FileTreeDirectory(SortedMap(p -> add(ps, null, bytes)))
          case FileTreeDirectory(children) =>
            FileTreeDirectory(children + (p -> add(ps, children.get(p).orNull, bytes)))
        }
    }
  }
  def readZip(bytes: Array[Byte], offset: Int = 0): FileTree = {
    val zipInputStream = new ZipInputStream(new ByteArrayInputStream(bytes, offset, bytes.length - offset))
    // TODO: multi-version jar
    // NOTE: using ZipInputStream instead of JarInputStream so we can also handle `.jmod` files
    // NOTE: using ZipInputStream instead of ZipFile so we can also handle recursive zip/jar files
    var fileTree: FileTree = FileTreeDirectory(SortedMap())
    val array = new Array[Byte](4 * 1024)
    var entry: ZipEntry = null
    while ({entry = zipInputStream.getNextEntry; entry != null}) {
      if (!entry.isDirectory) {
        val builder = new ByteArrayOutputStream()
        var len = -1
        while ({len = zipInputStream.read(array); len != -1}) {
          builder.write(array, 0, len)
        }
        // NOTE: officially the separator is "/", but some compression software uses "\"
        fileTree = add(entry.getName.split(Array('/', '\\')).toList, fileTree, builder.toByteArray)
      }
    }
    zipInputStream.close()
    fileTree
  }
  def readJmod(bytes: Array[Byte]): FileTree = { readZip(bytes, JMOD_OFFSET) }
  var classes = Map[String, (String, ClassReader)]()
  def load(path: Path, bytes: Array[Byte]): Unit = {
    assert(bytes.startsWith(CLASS_SIGNATURE))
    val classReader = new ClassReader(bytes)
    classes.get(classReader.getClassName) match {
      case None => classes += classReader.getClassName -> ((path.toString, classReader))
      case Some(_) => ignore // TODO
    }
  }
  def get(path: PathPosition): Unit = {
    path.read match {
      case RFile(bytes) =>
        if (bytes.startsWith(CLASS_SIGNATURE)) { path(error, load(path.path, bytes), load(path.path, bytes)) }
        else if (bytes.startsWith(ZIP_SIGNATURE)) { get(path.withFileTree(readZip(bytes))) }
        else if (bytes.startsWith(JMOD_SIGNATURE)) { get(path.withFileTree(readJmod(bytes))) }
        else { path(error, error, ignore) }
      case RDirectory(children) =>
        // Note: one if zip, zero if fs or zip and not found or zip and directory is empty
        path(oneOrZero(children,get,error), children.foreach(get), children.foreach(get))
      case ROther =>
        path(error, error, ignore)
      case RNotExist =>
        path(option(path.parent,get,error), option(path.parent,get,error), error)
    }
  }
  def get0(path: Path): Unit = {
    get(new PathPosition(path, Nil, Nil, None))
  }
}
