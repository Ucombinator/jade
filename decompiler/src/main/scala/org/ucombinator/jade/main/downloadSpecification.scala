package org.ucombinator.jade.main.downloadSpecification

object Main {
  def main(specification: String, version: Int, chapter: Int): Unit = {
    val url = f"https://docs.oracle.com/javase/specs/$specification/se$version/html/jls-$chapter.html"
    val source = scala.io.Source.fromURL(url)
    println(source.mkString)
    source.close()
  }
}
