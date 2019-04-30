package org.ucombinator.jade.main.javaFileTypes

import scala.collection.mutable
import scala.util.parsing.combinator._
import java.io._


case class Grammar(nonTerminals: List[NonTerminalDeclaration])

sealed trait CompoundProduction

case class NonTerminalDeclaration(name: String, productions: List[CompoundProduction]) extends CompoundProduction
case class NonTerminal(name: String) extends CompoundProduction
case class Terminal(name: String) extends CompoundProduction
case class Sequence(elements: List[CompoundProduction]) extends CompoundProduction
case class Repeat(prod: CompoundProduction) extends CompoundProduction
case class Optional(prod: CompoundProduction) extends CompoundProduction
case class OneOf(elements:List[CompoundProduction]) extends CompoundProduction

object Main {
  def main(): Unit = {
    val input = scala.io.Source.fromInputStream(System.in).mkString
    val grammar: Grammar.ParseResult[List[NonTerminalDeclaration]] = Grammar.parseString(input)
    grammar match {
      case Grammar.Success(matched, _) => convertGrammarToAST(matched)
      case Grammar.Failure(msg, _) => println(f"failure $msg")
      case Grammar.Error(msg, _) => println(f"error $msg")
    }
  }

  def toCamelCase(string: String): String = {
    Character.toLowerCase(string.charAt(0)) + string.substring(1)
  }

  def toPascalCase(string: String): String = {
    Character.toUpperCase(string.charAt(0)) + string.substring(1)
  }

  def generateRepetition(rep: Repeat, caseClassElements: mutable.HashMap[String, String]): Unit = {
    val seq: Sequence = rep.prod.asInstanceOf[Sequence]
    for (element <- seq.elements) {
      element match {
        case nonTerminal: NonTerminal =>
          //remove any existing entry with same name, since we'll add the same var as a list
          //TODO: merge these duplicate variables as one single non empty list
          if (caseClassElements.contains(nonTerminal.name)) {
            caseClassElements.remove(nonTerminal.name)
          }
          caseClassElements += nonTerminal.name -> f"List[${nonTerminal.name}]"
        case repeat: Repeat => generateRepetition(repeat, caseClassElements)
        case optional: Optional => generateOptional(optional, caseClassElements)
        case _ => None
      }
    }
  }

  def generateOptional(optional: Optional, caseClassElements: mutable.HashMap[String, String]): Unit = {
    val seq: Sequence = optional.prod.asInstanceOf[Sequence]
    for (element <- seq.elements) {
      element match {
        case nonTerminal: NonTerminal =>
          //remove any existing entry with same name, since we'll add the same var as a list
          if (caseClassElements.contains(nonTerminal.name)) {
            caseClassElements.remove(nonTerminal.name)
          }
          caseClassElements += nonTerminal.name -> f"Option[${nonTerminal.name}]"
        case repeat: Repeat => generateRepetition(repeat, caseClassElements)
        case optional: Optional => generateOptional(optional, caseClassElements)
        case _ => None
      }
    }
  }

  def buildAstTypes(nonTerminals: List[NonTerminalDeclaration], traits: mutable.Set[String], caseClassExtend: mutable.HashMap[String, Set[String]], caseObjectExtend: mutable.HashMap[String, Set[String]]): Unit = {
    for (nonTerminal <- nonTerminals) {
      nonTerminal.productions match {
        case List(oneOf@OneOf(_)) =>
          if (!(traits contains nonTerminal.name)) {
            traits += nonTerminal.name
          }

          for (sequence: Sequence <- oneOf.elements.asInstanceOf[List[Sequence]]) {
            for (seqElement <- sequence.elements) {
              seqElement match {
                case nonTerminalElement: NonTerminal =>
                  if (caseClassExtend.getOrElse(nonTerminalElement.name, None) != None) {
                    caseClassExtend += nonTerminalElement.name -> (caseClassExtend(nonTerminalElement.name) + nonTerminal.name)
                  } else {
                    caseClassExtend += nonTerminalElement.name -> Set(nonTerminal.name)
                  }
                case terminalElement: Terminal =>
                  val terminalElementName = toPascalCase(terminalElement.name)
                  if (caseObjectExtend.getOrElse(terminalElementName, None) != None) {
                    caseObjectExtend += terminalElementName -> (caseObjectExtend(terminalElementName) + nonTerminal.name)
                  } else {
                    caseObjectExtend += terminalElementName -> Set(nonTerminal.name)
                  }
                case _ => None
              }
            }
          }
        case sequences: List[Sequence] =>
          if (sequences.forall(containsOnlyNonTerminals(_))) {
            if (!(traits contains nonTerminal.name)) {
              traits += nonTerminal.name
            }
            for (sequence <- sequences) {
              val elements: List[NonTerminal] = sequence.elements.asInstanceOf[List[NonTerminal]]
              if (caseClassExtend.getOrElse(elements.head.name, None) != None) {
                caseClassExtend += elements.head.name -> (caseClassExtend(elements.head.name) + nonTerminal.name)
              } else {
                caseClassExtend += elements.head.name -> Set(nonTerminal.name)
              }
            }
          }
      }
    }
  }

  def printTraits(traits: mutable.Set[String], printWriter: PrintWriter): Unit = {
    for (traitName <- traits) {
      printWriter.write(f"sealed trait $traitName\n\n")
    }
  }


  def printCaseObjects(caseObjectExtend: mutable.HashMap[String, Set[String]], printWriter: PrintWriter): Unit = {
    for ((key, value) <- caseObjectExtend) {
      printWriter.write(f"case object $key extends ${value.mkString(" with ")}\n\n")
    }
  }

  def printCaseClasses(nonTerminals: List[NonTerminalDeclaration], extend: mutable.HashMap[String, Set[String]], printWriter: PrintWriter): Unit = {
    for (nonTerminal <- nonTerminals) {
      nonTerminal.productions match {
        case List(OneOf(_)) => None
        case sequences: List[Sequence] =>
          if (!sequences.forall(containsOnlyNonTerminals(_))) {
            printWriter.write(f"case class ${nonTerminal.name}(\n")
            val caseClassElements = mutable.HashMap[String, String]()

            for (sequence <- sequences) {
              for (element <- sequence.elements) {

                element match {
                  case nonTerminal: NonTerminal => caseClassElements += nonTerminal.name -> nonTerminal.name
                  case rep: Repeat => generateRepetition(rep, caseClassElements)
                  case opt: Optional => generateOptional(opt, caseClassElements)
                  case _ => None
                }
              }
            }

            for((key,value) <- caseClassElements){
              if(key.equalsIgnoreCase("finally")){
                printWriter.write(f"  $key: $value,\n")
              }else{
                printWriter.write(f"  ${toCamelCase(key)}: $value,\n")
              }
            }

            printWriter.write(")")
            if (extend.getOrElse(nonTerminal.name, None) != None) {
              printWriter.write(" extends ")
              printWriter.write(extend(nonTerminal.name).mkString(" with "))
            }
            printWriter.write("\n\n")
          }
        case _ => None
      }
    }
  }

  def convertGrammarToAST(nonTerminals: List[NonTerminalDeclaration]): Unit = {
    val traits = mutable.Set[String]()
    val caseClassExtend = mutable.HashMap[String, Set[String]]()
    val caseObjectExtend = mutable.HashMap[String, Set[String]]()
    val printWriter = new PrintWriter(System.out)
    //builds the above maps from the nonTerminal grammar
    buildAstTypes(nonTerminals,traits, caseClassExtend, caseObjectExtend)
    //print each of the generated types
    printTraits(traits, printWriter)
    printCaseObjects(caseObjectExtend, printWriter)
    printCaseClasses(nonTerminals, caseClassExtend, printWriter)
    printWriter.close()
  }

  def containsOnlyNonTerminals(sequence: Sequence): Boolean = {
    sequence match {
      case Sequence(List(NonTerminal(_))) => true
      case _ => false
    }
  }

}

object Grammar extends RegexParsers {
  override val skipWhitespace = false

  def eol: Parser[Unit] = sys.props("line.separator") ^^ { _ => Unit }
  def s:   Parser[Unit] = """ *""".r  ^^ { _ => Unit }
  def s1:  Parser[Unit] = """ +""".r  ^^ { _ => Unit }
  def ws:  Parser[Unit] = """\s*""".r ^^ { _ => Unit }

  def name:    Parser[String] = regex("""[A-Z][a-zA-Z]*""".r)
  def literal: Parser[String] = "'" ~> """[^']+""".r <~ "'"

  def term : Parser[CompoundProduction] =
    literal                    ^^ { Terminal } |
    name                       ^^ { NonTerminal } |
    "{" ~ s ~> expr <~ s ~ "}" ^^ { Repeat } |
    "[" ~ s ~> expr <~ s ~ "]" ^^ { Optional }

  def expr: Parser[Sequence] = repsep(term, s1) ^^ { Sequence }

  def production : Parser[CompoundProduction] = s1 ~> expr <~ s

  def declaration: Parser[NonTerminalDeclaration] =
    (name <~ s ~ ":" ~ s) ~ rep(eol ~> production) ^^
      { case name ~ productions => NonTerminalDeclaration(name, productions) }

  def declarationList: Parser[List[NonTerminalDeclaration]] =
    repsep(declaration, eol ~ (s ~ eol).*)

  def grammar: Parser[List[NonTerminalDeclaration]] =
    phrase(ws ~> declarationList <~ ws)

  def parseString(input: String) : ParseResult[List[NonTerminalDeclaration]] = {
    // TODO: allow `#` comments
    parse(grammar, input)
  }
}