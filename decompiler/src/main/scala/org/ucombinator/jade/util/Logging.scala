package org.ucombinator.jade.util

import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.classic.{BasicConfigurator, Level, LoggerContext, PatternLayout, Logger => LogbackLogger}
import ch.qos.logback.core.ConsoleAppender
import ch.qos.logback.core.encoder.LayoutWrappingEncoder
import com.typesafe.scalalogging.{Logger => ScalaLogger}
import org.slf4j.{LoggerFactory, Logger => Slf4jLogger}

import scala.collection.JavaConverters._

// TODO: slf4j.detectLoggerNameMismatch
trait Logging {
  @transient // TODO: why transient?
  protected lazy val logger: ScalaLogger = {
    val name = getClass.getName
      .replaceAll("^org.ucombinator.jade.", "")
      .replace('$', '.')
      .replace("..", ".")
      .replaceAll(".$", "")
    ScalaLogger (LoggerFactory.getLogger(name))
  }
  def childLogger(name: String): ScalaLogger  = {
    ScalaLogger(LoggerFactory.getLogger(logger.underlying.getName + "." + name))
  }
}

object Logging {
  def logger(name: String): LogbackLogger = {
    val modifiedName = if (name.isEmpty) { Slf4jLogger.ROOT_LOGGER_NAME } else { name }
    LoggerFactory.getLogger(modifiedName).asInstanceOf[LogbackLogger]
  }

  def tryLoad(string: String): Boolean = {
    try { Class.forName(string) } catch { case e: ClassNotFoundException => false }
    true
  }
  def init(string: String): Unit = {
    // TODO: pass if package exists
    var s = "org.ucombinator.jade." + string // TODO: allow omitting for non-jade classes using "." as a prefix
    println(f"s: $s")
    while (!tryLoad(s)) {
      if (s.contains(".")) {
        s = s.replaceAll("""\.[^.]+$""", "")
      } else {
        println(f"fail")
        return
      }
      println(f"s: $s")
    }
    println(f"succeed")
    for (l <- LoggerFactory.getLogger(Slf4jLogger.ROOT_LOGGER_NAME).asInstanceOf[LogbackLogger].getLoggerContext.getLoggerList.asScala) {
      if (string == l.getName || l.getName.startsWith(string + ".")) {
        println(f"exists")
      }
    }
    println(f"not exists")
  }

  var callerDepth = 0

  class Config() extends BasicConfigurator  {
    override def configure(loggerContext: LoggerContext): Unit = {
      val consoleAppender = new ConsoleAppender[ILoggingEvent]
      consoleAppender.setContext(loggerContext)
      consoleAppender.setName("console")
      val layoutWrappingEncoder = new LayoutWrappingEncoder[ILoggingEvent]
      layoutWrappingEncoder.setContext(loggerContext)

      val patternLayout = new PatternLayout()
      patternLayout.setPattern(f"%%highlight(%%level %%-5logger:) %%message%%n%%caller{$callerDepth}")

      patternLayout.setContext(loggerContext)
      patternLayout.start()
      layoutWrappingEncoder.setLayout(patternLayout)

      consoleAppender.setEncoder(layoutWrappingEncoder)
      consoleAppender.start()

      val rootLogger = loggerContext.getLogger(Slf4jLogger.ROOT_LOGGER_NAME)
      rootLogger.addAppender(consoleAppender)
      rootLogger.setLevel(Level.INFO)
    }
  }
}
