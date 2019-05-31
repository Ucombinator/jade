package org.ucombinator.jade.main

object DownloadSpecification {
  def main(specification: String, version: Int, chapter: Int): Unit = {
    val url = f"https://docs.oracle.com/javase/specs/$specification/se$version/html/$specification-$chapter.html"
    val source = scala.io.Source.fromURL(url)
    println(source.mkString)
    source.close()
  }
}
