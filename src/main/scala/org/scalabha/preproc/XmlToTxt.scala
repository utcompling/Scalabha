package org.scalabha.preproc

import scala.xml._
import org.clapper.argot.ArgotParser._
import org.scalabha.log.SimpleLogger
import org.clapper.argot.{ArgotUsageException, ArgotParser, ArgotConverters}
import java.io._
import scala.sys.process._

object XmlToTxt {

  import ArgotConverters._

  val parser = new ArgotParser("org.scalabha.preproc.Tokenizer", preUsage = Some("Version 0.0"))
  val help = parser.flag[Boolean](List("h", "help"), "print help")
  val input = parser.option[String](List("i", "input"), "FILE", "input file to tokenize")
  val tokenize = parser.flag[Boolean](List("t", "tokenize"), "Also output tokenized text")
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
            )).toMap.withDefault(x => defaultOutputWriter)

        var flatTextNodes = false
        var textSentenceNodes = false
        xmlTree \ "data" foreach {
          (dataNode) =>
            dataNode \ "unit" foreach {
              (unit) =>
                unit \ "text" foreach {
                  (text) =>
                    val lang = (text \ "@langid").text
                    // Support both:
                    // - <text>blah blah</text>
                    val r = text.text
                    val s = text \ "sentence"
                    if (s.length == 0) {
                      flatTextNodes = true
                      langToFile(lang).write(text.text + "\n")
                    } else {
                      textSentenceNodes = true
                    }
                    // - <text><sentence>blah.</sentence><sentence>blah.</sentence></text>
                    text \ "sentence" foreach {
                      (sentence) =>
                        langToFile(lang).write(sentence.text + "\n")
                    }
                }
            }
        }
        if (flatTextNodes) {
          log.warn("Detected flat text nodes. The <text><sentence/></text> is recommended.\n")
        }
        if (flatTextNodes && textSentenceNodes) {
          log.warn("Detected both flat text nodes and <text><sentence/></text> hierarchy. Mixing these styles is _not_ recommended.\n")
        }

        for ((name, ostream) <- langToFile) {
          ostream.close()
        }
        defaultOutputWriter.close()

        if (tokenize.value.isDefined) {
          val defaultXFileName = input.value.get.replace(".xml", "") + ".unknownLanguage"
          for (lang <- languages) {
            val fileName = "%s.%s".format(input.value.get.replace(".xml", ""), lang)
            (new File(fileName+".txt")) #> "normalize-text-standalone.pl" #| "tokenize-text.pl" #> (new File(fileName+".tok"))!
          }
        }
      }

      log.summary("Warnings,Errors: %s\n".format(log.getStats()))
    }
    catch {
      case e: ArgotUsageException =>
        println(e.message)
    }
  }

}