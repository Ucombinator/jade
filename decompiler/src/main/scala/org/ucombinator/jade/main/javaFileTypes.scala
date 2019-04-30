package org.ucombinator.jade.main.javaFileTypes

import scala.collection.mutable
import scala.util.parsing.combinator._
import java.io._

case class Grammar(declarations: List[Declaration])
case class Declaration(name: String, productions: List[Production])
case class Production(exprs: List[Expr])

sealed trait Expr
case class NonTerminal(name: String) extends Expr
case class Terminal(name: String) extends Expr
case class Repeat(prod: List[Expr]) extends Expr
case class Optional(prod: List[Expr]) extends Expr

object Main {
  def main(): Unit = {
    val input = scala.io.Source.fromInputStream(System.in).mkString
    val grammar: Grammar.ParseResult[List[Declaration]] = Grammar.parseString(input)
    grammar match {
      case Grammar.Error(msg, _) => println(f"error $msg")
      case Grammar.Failure(msg, _) => println(f"failure $msg")
      case Grammar.Success(declarations, _) =>
        val traits = mutable.Set[String]()
        val caseClassExtend = mutable.HashMap[String, Set[String]]()
        val caseObjectExtend = mutable.HashMap[String, Set[String]]()
        //builds the above maps from the nonTerminal grammar
        for (d <- declarations) {
          buildAstTypes(d, traits, caseClassExtend, caseObjectExtend)
        }
        //print each of the generated types
        val printWriter = new PrintWriter(System.out)
        printTraits(traits, printWriter)
        printCaseObjects(caseObjectExtend, printWriter)
        for (d <- declarations) {
          printCaseClasses(d, caseClassExtend, printWriter)
        }
        printWriter.close()
    }
  }

  def toCamelCase(string: String): String = {
    Character.toLowerCase(string.charAt(0)) + string.substring(1)
  }

  def toPascalCase(string: String): String = {
    Character.toUpperCase(string.charAt(0)) + string.substring(1)
  }

  def buildAstTypes(declaration: Declaration, traits: mutable.Set[String], caseClassExtend: mutable.HashMap[String, Set[String]], caseObjectExtend: mutable.HashMap[String, Set[String]]): Unit = {
    val productions = declaration.productions
    containsOnlyNonTerminals(productions) match {
      case Some(nonTerminals) =>
        traits += declaration.name
        for (nonTerminal: NonTerminal <- nonTerminals) {
          if (caseClassExtend.getOrElse(nonTerminal.name, None) != None) {
            caseClassExtend += nonTerminal.name -> (caseClassExtend(nonTerminal.name) + declaration.name)
          } else {
            caseClassExtend += nonTerminal.name -> Set(declaration.name)
          }
        }
      case None => /* Do nothing */
    }
  }

  def containsOnlyNonTerminals(productions: List[Production]): Option[List[NonTerminal]] = {
    productions.foldRight[Option[List[NonTerminal]]](Some(List())) {
      case (Production(List(nt : NonTerminal)), Some(nts)) => Some(nt :: nts)
      case _ => None
    }
  }

  def generateRepetition(rep: Repeat, caseClassElements: mutable.HashMap[String, String]): Unit = {
    for (element <- rep.prod) {
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
    for (element <- optional.prod) {
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

  def printCaseClasses(nonTerminal: Declaration, extend: mutable.HashMap[String, Set[String]], printWriter: PrintWriter): Unit = {
    val productions = nonTerminal.productions
    containsOnlyNonTerminals(productions) match {
      case Some(_) => /* Do nothing */
      case None =>
        printWriter.write(f"case class ${nonTerminal.name}(\n")
        val caseClassElements = mutable.HashMap[String, String]()

        for (production <- productions) {
          for (element <- production.exprs) {

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

  def term : Parser[Expr] =
    literal                    ^^ { Terminal } |
    name                       ^^ { NonTerminal } |
    "{" ~ s ~> list <~ s ~ "}" ^^ { Repeat } |
    "[" ~ s ~> list <~ s ~ "]" ^^ { Optional }

  def list: Parser[List[Expr]] = repsep(term, s1)
  def expr: Parser[Production] = list ^^ { Production }

  def production : Parser[Production] = s1 ~> list <~ s ^^ { Production }

  def declaration: Parser[Declaration] =
    (name <~ s ~ ":" ~ s) ~ rep(eol ~> production) ^^
      { case name ~ productions => Declaration(name, productions) }

  def declarationList: Parser[List[Declaration]] =
    repsep(declaration, eol ~ (s ~ eol).*)

  def grammar: Parser[List[Declaration]] =
    phrase(ws ~> declarationList <~ ws)

  def parseString(input: String) : ParseResult[List[Declaration]] = {
    // TODO: allow `#` comments
    parse(grammar, input)
  }
}