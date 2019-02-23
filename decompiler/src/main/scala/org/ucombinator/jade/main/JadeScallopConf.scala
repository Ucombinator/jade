package org.ucombinator.jade.main

import org.rogach.scallop._

import scala.collection.immutable

abstract class JadeSubcommand(name: String) extends Subcommand(name) with JadeScallopConf {
  def run(): Unit
}

trait JadeScallopConf extends ScallopConf {
  val help = opt[Unit](short = 'h', descr = "show this help message")(HelpConverter)

  // In `--help` information, show default values for arguments to options
  appendDefaultToDescription = true

  // Ensure that `--help` information is displayed every time there is an
  // error during option parsing.
  val oldErrorMessageHandler = errorMessageHandler
  errorMessageHandler = { message =>
    Console.withOut(Console.err) { printHelp(); println() }
    oldErrorMessageHandler(message)
  }

  // Change the default prefix to "no-"
  override def toggle(
    name: String = null,
    default: => Option[Boolean] = None,
    short: Char = '\u0000',
    noshort: Boolean = false,
    prefix: String = "no-",
    descrYes: String = "",
    descrNo: String = "",
    hidden: Boolean = false) =
    super.toggle(
      name = name,
      default = default,
      short = short,
      noshort = noshort,
      prefix = prefix,
      descrYes = descrYes,
      descrNo = descrNo,
      hidden = hidden)

  // Options that take an argument from a fixed list
  def enum[A](
    name: String = null,
    short: Char = '\u0000',
    descr: String = "",
    default: Option[String] = None,
    validate: A => Boolean = (_:A) => true,
    required: Boolean = false,
    argName: String = "arg",
    argType: String = "argument",
    hidden: Boolean = false,
    noshort: Boolean = false,
    elems: immutable.ListMap[String, A],
    conv: ValueConverter[A] = null): ScallopOption[A] = {
    opt[A](
      name = name,
      short = short,
      descr = descr + "; one of " + elems.keys.mkString("'", "', '", "'") +
        (default match {
          case None => ""
          case Some(d) => s"; default: '$d'"
        }),
      default = default.map(elems(_)),
      validate = validate,
      required = required,
      argName = argName,
      hidden = hidden,
      noshort = noshort)(
      conv = if (conv != null) { conv } else { enumConverter(argType, elems) })
  }

  private def enumConverter[A](name: String, elems: Map[String, A]) = {
    def conv(s: String): A =
      elems.getOrElse(s, throw new IllegalArgumentException(s"bad $name `$s` (expected one of: %s)" format elems.keys.mkString(" ")))
    // TODO: allow `handler` to be specified
    val handler: PartialFunction[Throwable, Either[String, Option[A]]] = {
      case e: IllegalArgumentException => Left(e.getMessage)
    }

    singleArgConverter[A](conv, handler)
  }
}

/**
  * A manual converter to handle calls to `--help`. This implementation allows
  * the `--help` option to be given at any point in the options -- not only at
  * the very beginning of the list of arguments.
  */
private object HelpConverter extends ValueConverter[Unit] {
  // Override the default `parse` method so that any instance of `--help` is
  // handled appropriately, i.e. a `Help` is thrown.
  override def parse(s: List[(String, List[String])]): Either[String, Option[Unit]] = s match {
    case Nil  => Right(None)
    case _    => throw exceptions.Help("")
  }
  val tag = scala.reflect.runtime.universe.typeTag[Unit]
  val argType = org.rogach.scallop.ArgType.FLAG
}
