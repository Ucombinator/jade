package org.ucombinator.jade.util

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, IOException}
import java.nio.file.{Files, Path}
import java.nio.file.attribute.BasicFileAttributes
import java.util.stream.Collectors
import java.util.zip.{ZipEntry, ZipInputStream}

import scala.collection.JavaConverters._
import scala.collection.SortedMap

// This code is more complicated than one would think due to a few things.
// The first is supporting treating compressed files as directories.
// The second is recursively loading directory contents.
// The third is trying to minimize the number of filesystem operations.
// The logic for these features interact with each other in "interesting" ways.

// TODO: http, https
// TODO: accumulate errors
// TODO: gzip, tar, bzip2

sealed trait ReadResult
case class RDirectory(children: /*Sorted*/List[PathPosition]) extends ReadResult
case class RFile(bytes: Array[Byte]) extends ReadResult
case object ROther extends ReadResult
case object RNotExist extends ReadResult

sealed trait PathPosition {
  def apply[A](above: => A, same: => A, below: => A): A
  def read: ReadResult
  def path: Path
  def parent: Option[PathPosition]
  def withZipTree(zipTree: ZipTree): PathPosition
}
case class FsPathPosition(basePath: Path, abovePath: List[String], belowPath: /*Reversed*/List[String]) extends PathPosition {
  assert(abovePath.isEmpty || belowPath.isEmpty)
  def apply[A](above: => A, same: => A, below: => A): A = {
    if (abovePath.nonEmpty) { above }
    else if (belowPath.nonEmpty) { below }
    else { same }
  }
  def read: ReadResult = {
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
        RDirectory(children.map(c => new FsPathPosition(basePath, List(), c.getFileName.toString :: belowPath)))
      }
    } else if (attributes.isRegularFile) {
      RFile(Files.readAllBytes(path))
    } else {
      ROther
    }
  }
  def path: Path = { belowPath.foldRight(basePath)((s,p) => p.resolve(s) ) }
  def parent: Option[PathPosition] = {
    assert(belowPath.isEmpty)
    val parent = path.getParent
    if (parent == null) { None }
    else { Some(new FsPathPosition(parent, path.getFileName.toString :: abovePath, belowPath))}
  }
  def withZipTree(zipTree: ZipTree): PathPosition = {
    new ZipPathPosition(basePath, abovePath, belowPath, zipTree)
  }
}
case class ZipPathPosition(basePath: Path, abovePath: List[String], belowPath: /*Reversed*/List[String], zipTree: ZipTree) extends PathPosition {
  def apply[A](above: => A, same: => A, below: => A): A = {
    if (abovePath.nonEmpty) { above }
    else if (belowPath.nonEmpty) { below }
    else { same }
  }
  def read: ReadResult = {
    zipTree match {
      case ZipTreeFile(bytes) => RFile(bytes)
      case ZipTreeDirectory(children) =>
        abovePath match {
          case p :: ps =>
            RDirectory(children.toList.filter(_._1 == p).map(zt => new ZipPathPosition(basePath.resolve(zt._1), ps, belowPath, zt._2)))
          case List() =>
            RDirectory(children.toList.map(zt => new ZipPathPosition(basePath, List(), zt._1 :: belowPath, zt._2)))
        }
    }
  }
  def path: Path = { belowPath.foldRight(basePath)((s,p) => p.resolve(s)) }
  def parent: Option[PathPosition] = { None }
  def withZipTree(zipTree: ZipTree): PathPosition = {
    new ZipPathPosition(basePath, abovePath, belowPath, zipTree)
  }
}

sealed trait ZipTree
case class ZipTreeFile(bytes: Array[Byte]) extends ZipTree
case class ZipTreeDirectory(children: /*Sorted*/SortedMap[String, ZipTree]) extends ZipTree

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
  def add(path: List[String], zipTree: ZipTree, bytes: Array[Byte]): ZipTree = {
    path match {
      case List() =>
        zipTree match {
          case null => ZipTreeFile(bytes)
          case _ => throw new Exception("TODO: bad add")
        }
      case p :: ps =>
        zipTree match {
          case ZipTreeFile(_) => throw new Exception("TODO: bad add 2")
          case null => ZipTreeDirectory(SortedMap(p -> add(ps, null, bytes)))
          case ZipTreeDirectory(children) =>
            ZipTreeDirectory(children + (p -> add(ps, children.get(p).orNull, bytes)))
        }
    }
  }
  def readZip(bytes: Array[Byte], offset: Int = 0): ZipTree = {
    val zipInputStream = new ZipInputStream(new ByteArrayInputStream(bytes, offset, bytes.length - offset))
    // TODO: multi-version jar
    // NOTE: using ZipInputStream instead of JarInputStream so we can also handle `.jmod` files
    // NOTE: using ZipInputStream instead of ZipFile so we can also handle recursive zip/jar files
    var zipTree: ZipTree = ZipTreeDirectory(SortedMap())
    val array = new Array[Byte](4 * 1024)
    var entry: ZipEntry = null
    while ({entry = zipInputStream.getNextEntry; entry != null}) {
      if (!entry.isDirectory) {
        val builder = new ByteArrayOutputStream()
        var len = -1
        while ({len = zipInputStream.read(array); len != -1}) {
          builder.write(array, 0, len)
        }
        zipTree = add(entry.getName.split(Array('/', '\\')).toList, zipTree, builder.toByteArray)
      }
      // TODO: both "/" and "\" could be the separator
    }
    zipInputStream.close()
    zipTree
  }
  def readJmod(bytes: Array[Byte]): ZipTree = { readZip(bytes, JMOD_OFFSET) }
  def load(path: Path, bytes: Array[Byte]): Unit = {
    println(f"load: $path")
    // TODO
  }
    // fs:
    // file ->
    //   class -> error message | load | load
    //   zip -> zip | zip | zip
    //   other -> error message | error message | ignore
    // directory -> error message | fs(down) | fs(down)
    // other -> error message | error message | ignore
    // not exist -> fs(up) | fs(up) | error message
    // zip:
    // file ->
    //   class -> error message | load | load
    //   zip -> zip | zip | zip
    //   other -> error message | error message | ignore
    // directory -> zip | zip | zip
    // other -> error message | error message | ignore
    // not exist -> error message | error message | error message
  def get(path: PathPosition): Unit = {
    println(f"path: $path")
    path.read match {
      case RFile(bytes) =>
        if (bytes.startsWith(CLASS_SIGNATURE)) { path(error, load(path.path, bytes), load(path.path, bytes)) }
        else if (bytes.startsWith(ZIP_SIGNATURE)) { get(path.withZipTree(readZip(bytes))) }
        else if (bytes.startsWith(JMOD_SIGNATURE)) { get(path.withZipTree(readJmod(bytes))) }
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
    get(new FsPathPosition(path, Nil, Nil))
  }
}
