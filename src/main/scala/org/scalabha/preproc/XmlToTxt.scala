package org.scalabha.preproc

import scala.xml._
import org.clapper.argot.ArgotParser._
import org.scalabha.log.SimpleLogger
import java.io.{OutputStreamWriter, BufferedWriter}
import org.clapper.argot.{ArgotUsageException, ArgotParser, ArgotConverters}

object XmlToTxt {

  import ArgotConverters._

  val parser = new ArgotParser("org.scalabha.preproc.Tokenizer", preUsage = Some("Version 0.0"))
  val help = parser.flag[Boolean](List("h", "help"), "print help")
  val input = parser.option[String](List("i", "input"), "FILE", "input file to tokenize")
  val log = new SimpleLogger(XmlToTxt.getClass.toString, SimpleLogger.WARN, new BufferedWriter(new OutputStreamWriter(System.err)))

  def main(args: Array[String]) {
    try {
      parser.parse(args)

      if (help.value.isDefined) {
        parser.usage()
      }

      if (input.value.isDefined) {
        val xmlTree = XML.load(input.value.get)
        println(xmlTree)
        val y = xmlTree\"file"
        val z = (y\"@languages").text.split(",").toList
        y\"data" foreach {(dataNode) =>
          dataNode\"unit" foreach {(unit) =>
            unit\"text" foreach {(text) =>
              println("%d: %s".format(z.indexOf((text\"@langid").text), text.text)) //TODO distribute the output into different files
            }
          }
        }
        println(y)
      }

      log.summary("Warnings,Errors: %s\n".format(log.getStats()))
    }
    catch {
      case e: ArgotUsageException =>
        println(e.message)
    }
  }

}