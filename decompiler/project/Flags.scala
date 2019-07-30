import org.jsoup.Jsoup
import org.jsoup.nodes.Element

import scala.collection.JavaConverters._
import scala.collection.mutable

// Code for generating `Flags.txt` and `Flags.scala`
object Flags {
  def javaSpec(spec: String, version: Int, chapter: Int): String = {
    if (version < 9) {
      throw new Exception(f"The specified version must be at least 9, but version is $version.")
    }
    val url = f"https://docs.oracle.com/javase/specs/$spec/se$version/html/$spec-$chapter.html"
    val source: scala.io.Source = try {
      scala.io.Source.fromURL(url)
      } catch {
        case e: Throwable => throw new Exception(f"Download of java specification failed")
      }
    val string = source.mkString
    source.close()
    string
  }

  def table(html: String): String = {
    val builder = StringBuilder.newBuilder
    val document = Jsoup.parse(html)

    builder.append(f"# Do not edit this file by hand.  It is generated by `sbt flagsTable`.\n")
    builder.append(f"\n")
    builder.append(f"# Kind      Name             Value  Keyword      Description\n")

    {
      val tables = List[(String, String)](
        "Class" -> "Class access and property modifiers",
        "Field" -> "Field access and property flags",
        "Method" -> "Method access and property flags",
        "NestedClass" -> "Nested class access and property flags")
      for ((kind, tableSummary) <- tables) {
        val mutable.Buffer(table) = document.select(f"""table[summary="$tableSummary"]""").asScala
        for (row <- table.select("tbody > tr").asScala.toList) {
          val List(accName, value, description) = row.select("td").asScala.toList
          val keyword =
            """(Declared|Marked|Marked or implicitly) <code class="literal">(.*)</code>""".r
              .findFirstMatchIn(description.childNodes().asScala.mkString) match {
            case Some(regexMatch) => regexMatch.group(2)
            case None => "-"
          }
          // USE: width formatters in printout to align columns
          builder.append(f"$kind%-11s ${accName.text}%-16s ${value.text} $keyword%-12s ${description.text}\n")
        }
        builder.append("\n")
      }
    }

    {
      val lists = List[(String, String)](
        "Parameter" -> "access_flags",
        "Module" -> "module_flags",
        "Requires" -> "requires_flags",
        "Exports" -> "exports_flags",
        "Opens" -> "opens_flags")
      for ((kind, codeLiteral) <- lists) {
        val mutable.Buffer(list) = document.select(
          f"dd:has(div[class=variablelist] dl) > p:matchesOwn(The value of the) > code[class=literal]:matchesOwn(^$codeLiteral$$)").asScala
        val rows = list.parent.nextElementSibling.child(0).children().asScala.grouped(2)
        for (mutable.Buffer(row: Element, description: Element) <- rows.toList) {
          val regexMatch = """(0x[0-9]*) \(([A-Z_]*)\)""".r.findPrefixMatchOf(row.text).get
          val value = regexMatch.group(1)
          val accName = regexMatch.group(2)
          val keyword = if (accName == "ACC_TRANSITIVE") { "transitive" } else { "-" }
          builder.append(f"$kind%-11s $accName%-16s $value $keyword%-12s ${description.text}\n")
        }
        builder.append("\n")
      }
    }

    builder.mkString.replaceAll("\n\n$", "\n")
  }

  private case class FlagInfo(kind: String, accName: String, value: Int, keyword: Option[String], description: String)

  def code(table: String): String = {
    val flagInfos =
      (for (line <- table.lines if !line.matches("\\s*#.*") && !line.matches("\\s*")) yield {
        val Array(kind, accName, value, keyword, description) = line.split(" +", 5)
        val k = if (keyword == "-") { None } else { Some(keyword) }
        val intValue = java.lang.Integer.parseInt(value.substring(2), 16)
        FlagInfo(kind, accName, intValue, k, description)
      }).toList

    val flagExtensions = flagInfos.map(x => x.accName -> x.kind).groupBy(_._1).mapValues(_.map(_._2))
    val uniqueFlagInfos = flagInfos.groupBy(_.accName).toList.map(_._2.head).sortBy(_.accName).sortBy(_.value)
    val flagInfoGroups = mutable.LinkedHashMap[String, List[FlagInfo]]()
    for (m <- flagInfos) {
      flagInfoGroups += m.kind -> (flagInfoGroups.getOrElse(m.kind, List()) :+ m)
    }

    val builder = StringBuilder.newBuilder

    builder.append(f"// Do not edit this file by hand.  It is generated by `sbt`.\n")
    builder.append("package org.ucombinator.jade.classfile\n")
    builder.append("\n")
    builder.append("import com.github.javaparser.ast.Modifier\n")
    builder.append("import com.github.javaparser.ast.NodeList\n")
    builder.append("\n")

    builder.append(
    """sealed trait Flag {
      |  def value: Int
      |  def keyword: Option[Modifier.Keyword]
      |  def valueAsString: String = f"0x$value%04x"
      |  def modifier: Option[Modifier] = keyword.map(new Modifier(_))
      |}
      |
      |""".stripMargin)

    for (kind <- flagInfos.map(_.kind).distinct) {
      builder.append(f"sealed trait ${kind}Flag extends Flag\n")
    }
    builder.append("\n")

    builder.append(
      """object Flags {
        |  def toModifiers(flags: List[Flag]): NodeList[Modifier] = {
        |    import scala.collection.JavaConverters._
        |    new NodeList(flags.flatMap(_.modifier).asJava)
        |  }
        |
        |  private def fromInt[T](mapping: List[(Int, T)]): Int => List[T] = {
        |    int: Int => {
        |      val maskedInt = int & 0xFFFF // Ignore ASM specific flags, which occur above bit 16
        |      val result = mapping.filter(pair => (pair._1 & maskedInt) != 0)
        |      val intResult = result.map(_._1).fold(0)(_ | _)
        |      assert(maskedInt == intResult, f"flag parsing error: want 0x$int%x, got 0x$intResult%x")
        |      result.map(_._2)
        |    }
        |  }
        |
        |""".stripMargin)

    for (flagsInfo <- uniqueFlagInfos) {
      val keyword = flagsInfo.keyword.map("Modifier.Keyword." ++ _.toUpperCase)
      builder.append(
        f"  case object ${flagsInfo.accName} extends Flag with " + flagExtensions(flagsInfo.accName).map(_ + "Flag").mkString(" with ") + " {\n" +
        f"    override val value = 0x${flagsInfo.value}%04x\n" +
        f"    override val keyword = $keyword\n" +
        f"  }\n")
    }
    builder.append("\n")

    for ((kind, flagInfosForKind) <- flagInfoGroups) {
      assert(flagInfosForKind.map(_.value) == flagInfosForKind.map(_.value).distinct)
      builder.append(f"  private val ${kind}Mapping = List[(Int, ${kind}Flag)](\n")
      for (flagInfo <- flagInfosForKind.sortBy(_.value)) {
        builder.append(f"    (/*0x${flagInfo.value}%04x*/ ${flagInfo.accName}.value, ${flagInfo.accName}), // ${flagInfo.description}\n")
      }
      builder.append("  )\n")
    }
    builder.append("\n")

    for ((kind, flagInfosForKind) <- flagInfoGroups) {
      val name = kind.substring(0,1).toLowerCase() + kind.substring(1) + "Flags"
      builder.append(f"  val $name: Int => List[${kind}Flag] = fromInt(${kind}Mapping)\n")
    }
    builder.append("}\n")

    builder.mkString
  }
}
