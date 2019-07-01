package org.ucombinator.jade.util

import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.classic.{BasicConfigurator, Level, LoggerContext, PatternLayout}
import ch.qos.logback.core.ConsoleAppender
import ch.qos.logback.core.encoder.LayoutWrappingEncoder
import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory

trait Logging {
  @transient // TODO: why transient?
  protected lazy val logger: Logger = {
    val name = getClass.getName
      .replaceAll("^org.ucombinator.jade.", "")
      .replace('$', '.')
      .replace("..", ".")
      .replaceAll(".$", "")
    Logger(LoggerFactory.getLogger(name))
  }
  def childLogger(name: String): com.typesafe.scalalogging.Logger = {
    com.typesafe.scalalogging.Logger(LoggerFactory.getLogger(logger.underlying.getName + "." + name))
  }
}

object Logging {
  def logger(name: String): ch.qos.logback.classic.Logger = {
    val name2 = if (name.isEmpty) { org.slf4j.Logger.ROOT_LOGGER_NAME } else { name }
    LoggerFactory.getLogger(name2).asInstanceOf[ch.qos.logback.classic.Logger]
  }

  // TODO: command to list all loggers
  // TODO: better error reporting when there is a typo in the logger name on the command line
  val displayCaller = false // TODO: command line option for this
  class Config() extends BasicConfigurator  {

    override def configure(lc: LoggerContext): Unit = {
      val ca = new ConsoleAppender[ILoggingEvent]
      ca.setContext(lc)
      ca.setName("console")
      val encoder = new LayoutWrappingEncoder[ILoggingEvent]
      encoder.setContext(lc)

      val layout = new PatternLayout()
      layout.setPattern("%highlight(%level) %-5logger: %message%n" + (if (displayCaller) { "%caller" } else { "" }))

      layout.setContext(lc)
      layout.start()
      encoder.setLayout(layout)

      ca.setEncoder(encoder)
      ca.start()

      val rootLogger = lc.getLogger(org.slf4j.Logger.ROOT_LOGGER_NAME)
      rootLogger.addAppender(ca)
      rootLogger.setLevel(Level.INFO)
    }
  }
}
