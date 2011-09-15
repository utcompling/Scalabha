package org.scalabha.preproc

import scala.xml._
import org.clapper.argot.ArgotParser._
import org.scalabha.log.SimpleLogger
import org.clapper.argot.{ArgotUsageException, ArgotParser, ArgotConverters}
import java.io._

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
        val xmlTree = XML.load(new InputStreamReader(new FileInputStream(new File(input.value.get)), "ISO-8859-1")) \ "file"
        val languages = (xmlTree \ "@languages").text.split(",").toList
        val defaultOutputWriter = new OutputStreamWriter(new FileOutputStream(
              new File(input.value.get.replace(".xml", "") + ".unknownLanguage.txt")), "ISO-8859-1")
        val langToFile: Map[String, OutputStreamWriter] =
          (for (lang <- languages) yield (lang,
            new OutputStreamWriter(new FileOutputStream(
              new File("%s.%s.txt".format(input.value.get.replace(".xml", ""), lang))), "ISO-8859-1")
            )).toMap.withDefault(x=>defaultOutputWriter)

        xmlTree \ "data" foreach {
          (dataNode) =>
            dataNode \ "unit" foreach {
              (unit) =>
                unit \ "text" foreach {
                  (text) =>
                    langToFile((text \ "@langid").text).write(text.text+"\n")
                }
            }
        }
        for ((name,ostream) <- langToFile){
          ostream.close()
        }
        defaultOutputWriter.close()
      }

      log.summary("Warnings,Errors: %s\n".format(log.getStats()))
    }
    catch {
      case e: ArgotUsageException =>
        println(e.message)
    }
  }

}