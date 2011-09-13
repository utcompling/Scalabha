package org.scalabha.log

import java.io.BufferedWriter


class SimpleLogger(name: String, logLevel: Int, output: BufferedWriter) {
  private var warnings = 0
  private var errors = 0

  private def print (message:String){
    output.write(message)
      output.flush()
  }

  def info(message: String) {
    if (logLevel >= SimpleLogger.INFO) {
      print("%s: [INFO] %s".format(name,message))
    }
  }

  def warn(message: String) {
    if (logLevel >= SimpleLogger.WARN) {
      print("%s: [WARN] %s".format(name,message))
    }
    warnings += 1
  }

  def err(message: String) {
    if (logLevel >= SimpleLogger.ERR) {
      print("%s: [ERR] %s".format(name,message))
    }
    errors += 1
  }

  def getStats(): (Int, Int) = {
    (warnings, errors)
  }
}

object SimpleLogger {
  val NONE = 0
  val ERR = NONE+1
  val WARN = ERR+1
  val INFO = WARN+1
}