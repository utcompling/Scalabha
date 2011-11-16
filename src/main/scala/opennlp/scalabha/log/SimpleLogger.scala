package opennlp.scalabha.log

import java.io.BufferedWriter


class SimpleLogger(name: String, creationLogLevel: Int, output: BufferedWriter) {
  private var warnings = 0
  private var errors = 0
  var logLevel = creationLogLevel

  private def print(message: String) {
    output.write(message)
    output.flush()
  }

  def debug(message: String) {
    if (logLevel >= SimpleLogger.DEBUG) {
      print("%s: [DEBUG] %s".format(name, message))
    }
  }

  def trace(message: String) {
    if (logLevel >= SimpleLogger.TRACE) {
      print("%s: [INFO] %s".format(name, message))
    }
  }

  def warn(message: String) {
    if (logLevel >= SimpleLogger.WARN) {
      print("%s: [WARN] %s".format(name, message))
    }
    warnings += 1
  }

  def err(message: String) {
    if (logLevel >= SimpleLogger.ERR) {
      print("%s: [ERR] %s".format(name, message))
    }
    errors += 1
  }

  def info(message: String) {
    if (logLevel >= SimpleLogger.INFO) {
      print("%s: [ERR] %s".format(name, message))
    }
  }

  def summary(message: String) {
    print("%s: [SUMMARY] %s".format(name, message))
  }

  def getStats(): (Int, Int) = {
    (warnings, errors)
  }
}

object SimpleLogger {
  val NONE = 0
  val INFO = 1
  val ERR = 1
  val WARN = 2
  val DEBUG = 3
  val TRACE = 4
}
