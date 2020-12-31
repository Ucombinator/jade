package org.ucombinator.jade.util

import java.util.jar.JarFile

import ch.qos.logback.classic.pattern.NamedConverter
import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.classic.{BasicConfigurator, Level, LoggerContext, PatternLayout, Logger => LogbackLogger}
import ch.qos.logback.core.ConsoleAppender
import ch.qos.logback.core.encoder.LayoutWrappingEncoder
import ch.qos.logback.core.pattern.color.ANSIConstants.{BOLD, DEFAULT_FG, RED_FG}
import ch.qos.logback.core.pattern.color.ForegroundCompositeConverterBase
import com.typesafe.scalalogging.{Logger => ScalaLogger}
import org.slf4j.{LoggerFactory, Logger => Slf4jLogger}
import org.ucombinator.jade.main.Main

import scala.jdk.CollectionConverters._

// TODO: slf4j.detectLoggerNameMismatch
// TODO: Ensure Logging can extends only objects?
// TODO: lowercase/case insensitive logger names
trait Logging {
  protected val logger: ScalaLogger = {
    val name = getClass.getName
      .replace('$', '.')
      .replace("..", ".")
      .replaceAll(".$", "")
    ScalaLogger (LoggerFactory.getLogger(name))
  }
  def childLogger(name: String): ScalaLogger  = {
    ScalaLogger(LoggerFactory.getLogger(logger.underlying.getName + "." + name))
  }
}

object Logging extends Logging {
  def getLogger(name: String): LogbackLogger = {
    val modifiedName = if (name.isEmpty) { Slf4jLogger.ROOT_LOGGER_NAME } else { name }
    LoggerFactory.getLogger(modifiedName).asInstanceOf[LogbackLogger]
  }

  def listLoggers(): Unit = {
    // See https://stackoverflow.com/questions/320542/how-to-get-the-path-of-a-running-jar-file
    // Note: toURI is required in order to handle special characters
    val jar = new java.io.File(classOf[Main].getProtectionDomain.getCodeSource.getLocation.toURI).getPath
    this.logger.debug(f"jar: $jar")

    for (entry <- new JarFile(jar).entries().asScala) {
      if (entry.getName.endsWith(".class")) {
        try {
          Class.forName(entry.getName.
            replaceAll("\\.class$", "").
            replaceAll("/", "."))
        } catch {
          case e: Throwable => /* Ignored */
            this.logger.debug(s"skipping: ${entry.getName}")
        }
      }
    }

    for (l <- LoggerFactory.getLogger(Slf4jLogger.ROOT_LOGGER_NAME).asInstanceOf[LogbackLogger].getLoggerContext.getLoggerList.asScala) {
      println(l.getName)
    }
  }

  var callerDepth = 0

  val prefix = "org.ucombinator.jade." // TODO: derive automatically

  class LoggerConverter extends NamedConverter {
    override protected def getFullyQualifiedName(event: ILoggingEvent): String = {
      val name = event.getLoggerName
      if (name.startsWith(prefix)) { name.stripPrefix(prefix) }
      else { "." + name}
    }
  }

  class HighlightingCompositeConverter extends ForegroundCompositeConverterBase[ILoggingEvent] {
    override protected def getForegroundColorCode(event: ILoggingEvent): String = {
      val level = event.getLevel
      level.toInt match {
        case Level.ERROR_INT =>
          BOLD + RED_FG
        case Level.WARN_INT =>
          RED_FG
        case Level.INFO_INT =>
          DEFAULT_FG // Was `BLUE_FG`, but that is hard to read on a black background
        case _ =>
          DEFAULT_FG
      }
    }
  }

  class Config extends BasicConfigurator  {
    override def configure(loggerContext: LoggerContext): Unit = {
      val patternLayout = new PatternLayout()
      patternLayout.getInstanceConverterMap.put("logger", classOf[LoggerConverter].getName)
      patternLayout.getInstanceConverterMap.put("highlight", classOf[HighlightingCompositeConverter].getName)
      patternLayout.setPattern(f"%%highlight(%%-5level %%logger:) %%message%%n%%caller{$callerDepth}")
      patternLayout.setContext(loggerContext)
      patternLayout.start()

      val layoutWrappingEncoder = new LayoutWrappingEncoder[ILoggingEvent]
      layoutWrappingEncoder.setContext(loggerContext)
      layoutWrappingEncoder.setLayout(patternLayout)

      val consoleAppender = new ConsoleAppender[ILoggingEvent]
      consoleAppender.setContext(loggerContext)
      consoleAppender.setName("console")
      consoleAppender.setEncoder(layoutWrappingEncoder)
      consoleAppender.start()

      val rootLogger = loggerContext.getLogger(Slf4jLogger.ROOT_LOGGER_NAME)
      rootLogger.addAppender(consoleAppender)
      rootLogger.setLevel(Level.INFO)
    }
  }
}
