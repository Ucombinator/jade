package org.ucombinator.jade.main.extractGrammar

import org.jsoup.Jsoup
import org.jsoup.nodes.{Element, Node, TextNode}

import scala.collection.JavaConverters._

object Main {
  def main(): Unit = {
    val html = scala.io.Source.fromInputStream(System.in).mkString
    val document = Jsoup.parse(html)
    for (d <- document.select(".productionset").asScala) {
      if (!d.attr("title").contains("Lexical Structure")) {
        for (p <- d.select(".production").asScala) {
          println(f"\n\n${p.select(".lhs").text()}")
          oneOf = false
          process(p.select(".rhs").first.childNodes.asScala)
        }
      }
    }
    println()
  }

  var oneOf = false
  def nl: String = if (oneOf) { "\n" } else { "" }

  def process(nodes: Iterable[Node]): Unit = {
    for (node <- nodes) {
      node match {
        case node : TextNode =>
          node.text.trim match {
            case "" => /* Do nothing */
            case "(one of)" => oneOf = true
            case t => print(f" ${t}${nl}")
          }
        case node : Element =>
          node.tagName match {
            case "a" => print(f" ${node.text}${nl}")
            case "br" => if (!oneOf) { println() }
            case "code" => print(f" '${node.text}'${nl}")
            case "pre" => process(node.childNodes.asScala)
            case tag => throw new Exception(f"Unknown tag type: ${tag}")
          }
        case node => throw new Exception(f"Unknown node type ${node.outerHtml}")
      }
    }
  }
}
