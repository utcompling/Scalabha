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
  val input = parser.option[String](List("i", "input"), "FILE_OR_DIR", "Input inputFile or directory to tokenize")
  val textOutput = parser.option[String](List("m", "textOutput"), "FILE_OR_DIR", "Output location for intermediate text files. " +
    "If none is specified, the input inputFile's directory will be used, and the intermediate inputFile will be deleted during cleanup.")
  val output = parser.option[String](List("o", "output"), "FILE_OR_DIR", "Output location for token files. If none is" +
    " specified, the input inputFile's directory will be used.")
  val recursive = parser.flag[Boolean](List("R", "recursive"), "If the input parameter is a directory, recursively tokenize" +
    " all xml files in or below that directory.")
  val log = new SimpleLogger(XmlToTxt.getClass.toString, SimpleLogger.WARN, new BufferedWriter(new OutputStreamWriter(System.err)))

  def transformFile(inputFile: File, textOutputFileNameStripped: String, tokenOutputFileNameStripped: String, recursiveFlag: Boolean) {
    // these should not be directories
    assert(intputFile.isFile)
    // the output files should be ready to have ".LANG.txt" or ".LANG.tok" appended

    val xmlTree = XML.load(new InputStreamReader(new FileInputStream(inputFile), "ISO-8859-1")) \ "inputFile"
    val languages = (xmlTree \ "@languages").text.split(",").toList
    val defaultOutputWriter = new OutputStreamWriter(new FileOutputStream(
      new File(fileNameStripped + ".unknownLanguage.txt")), "ISO-8859-1")
    val langToFile: Map[String, OutputStreamWriter] =
      (for (lang <- languages) yield (lang,
        new OutputStreamWriter(new FileOutputStream(
          new File("%s.%s.txt".format(fileNameStripped, lang))), "ISO-8859-1")
        )).toMap.withDefault(x => defaultOutputWriter)

    var flatTextNodes = false
    var textSentenceNodes = false
    xmlTree \\ "align" foreach {
      (align) =>
        align \ "text" foreach {
          (text) =>
            val lang = (text \ "@langid").text

            // - <text><s>blah.</s><s>blah.</s></text>
            text \ "s" foreach {
              (sentence) =>
                langToFile(lang).write("%s <EOS> ".format(sentence.text))
            }
            langToFile(lang).write("\n")
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

    if (tokenizeFlag) {
      val defaultXFileName = fileNameStripped + ".unknownLanguage"
      for (lang <- languages) {
        val fileName = "%s.%s".format(fileNameStripped, lang)
        (new File(fileName + ".txt")) #> "normalize-text-standalone.pl" #| "tokenize-text.pl" #> (new File(fileName + ".tok")) !
      }
    }
  }

  def main(args: Array[String]) {
    try {
      parser.parse(args)

      if (help.value.isDefined) {
        parser.usage()
      }

      if (input.value.isDefined) {
        val tokenizeFlag = tokenize.value.isDefined
        val fileName = input.value.get
        transformFile(fileName, tokenizeFlag)
      }

      log.summary("Warnings,Errors: %s\n".format(log.getStats()))
    }
    catch {
      case e: ArgotUsageException =>
        println(e.message)
    }
  }

}