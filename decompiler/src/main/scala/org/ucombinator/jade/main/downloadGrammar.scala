package org.ucombinator.jade.main.downloadGrammar

object Main {
  val versions = Map(
    //"6" -> "https://docs.oracle.com/javase/specs/jls/se6/html/syntax.html",
    //"7" -> "https://docs.oracle.com/javase/specs/jls/se7/html/jls-18.html",
    "8" -> "https://docs.oracle.com/javase/specs/jls/se8/html/jls-19.html",
    "9" -> "https://docs.oracle.com/javase/specs/jls/se9/html/jls-19.html",
    "10" -> "https://docs.oracle.com/javase/specs/jls/se10/html/jls-19.html",
    "11" -> "https://docs.oracle.com/javase/specs/jls/se11/html/jls-19.html",
    "12" -> "https://docs.oracle.com/javase/specs/jls/se12/html/jls-19.html")

  def main(versionOrUrl: String): Unit = {
    val url = versions.getOrElse(versionOrUrl, versionOrUrl)
    println(scala.io.Source.fromURL(url).mkString)
  }
}
