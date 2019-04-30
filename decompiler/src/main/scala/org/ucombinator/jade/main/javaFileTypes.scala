package org.ucombinator.jade.main.javaFileTypes

import scala.collection.mutable
import scala.util.parsing.combinator._

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
    val declarations = grammar match {
      case Grammar.Error(msg, _) => throw new Exception(f"parse error: $msg")
      case Grammar.Failure(msg, _) => throw new Exception(f"parse failure: $msg")
      case Grammar.Success(declarations, _) => declarations
    }
    val traits = mutable.Set[String]()
    val caseClassExtend = mutable.Map[String, Set[String]]().withDefaultValue(Set())
    //builds the above maps from the declaration grammar
    for (d <- declarations) {
      buildAstTypes(d, traits, caseClassExtend)
    }
    //print each of the generated types
    printTraits(traits)
    for (d <- declarations) {
      printCaseClasses(d, caseClassExtend)
    }
  }

  def buildAstTypes(declaration: Declaration, traits: mutable.Set[String], caseClassExtend: mutable.Map[String, Set[String]]): Unit = {
    val productions = declaration.productions
    containsOnlyNonTerminals(productions) match {
      case Some(nonTerminals) =>
        traits += declaration.name
        for (nonTerminal <- nonTerminals) {
          caseClassExtend(nonTerminal.name) += declaration.name
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

  def printTraits(traits: mutable.Set[String]): Unit = {
    for (traitName <- traits) {
      print(f"sealed trait $traitName\n\n")
    }
  }

  def toCamelCase(string: String): String = {
    Character.toLowerCase(string.charAt(0)) + string.substring(1)
  }

  def toPascalCase(string: String): String = {
    Character.toUpperCase(string.charAt(0)) + string.substring(1)
  }

  def generateRepetition(rep: Repeat, caseClassElements: mutable.Map[String, String]): Unit = {
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

  def generateOptional(optional: Optional, caseClassElements: mutable.Map[String, String]): Unit = {
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

  def printCaseClasses(declaration: Declaration, extend: mutable.Map[String, Set[String]]): Unit = {
    containsOnlyNonTerminals(declaration.productions) match {
      case Some(_) => /* Do nothing */
      case None =>
        print(f"case class ${declaration.name}(\n")
        val caseClassElements = mutable.Map[String, String]()

        for (production <- declaration.productions) {
          for (expr <- production.exprs) {
            expr match {
              case expr: NonTerminal => caseClassElements += expr.name -> expr.name
              case expr: Repeat => generateRepetition(expr, caseClassElements)
              case expr: Optional => generateOptional(expr, caseClassElements)
              case _ => /* Do nothing */
            }
          }
        }

        for((key,value) <- caseClassElements){
          if(key.equalsIgnoreCase("finally")){
            print(f"  $key: $value,\n")
          }else{
            print(f"  ${toCamelCase(key)}: $value,\n")
          }
        }

        print(")")
        if (extend.getOrElse(declaration.name, None) != None) {
          print(" extends ")
          print(extend(declaration.name).mkString(" with "))
        }
        print("\n\n")
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