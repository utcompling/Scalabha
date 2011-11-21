package opennlp.scalabha.preproc

import scala.xml._
import org.clapper.argot.ArgotParser._
import opennlp.scalabha.log.SimpleLogger
import org.clapper.argot.{ArgotUsageException, ArgotParser, ArgotConverters}
import java.io._
import scala.sys.process._
import org.xml.sax.SAXParseException
import opennlp.scalabha.util.FileUtils
import java.util.regex.Pattern
import util.matching.Regex
import ArgotConverters._

object X2TXT {
  val parser = new ArgotParser(this.getClass.getName, preUsage = Some("Version 0.0"))
  val help = parser.flag[Boolean](List("h", "help"), "print help")
  val input = parser.option[String](List("x", "xml-input"), "FILE_OR_DIR", "Input inputFile or directory to tokenize")
  val textOutput = parser.option[String](List("t", "text-output"), "FILE_OR_DIR", "Output location for intermediate text files. " +
    "If none is specified, the input inputFile's directory will be used.")
  /*val output = parser.option[String](List("o", "output"), "FILE_OR_DIR", "Output location for token files. If none is" +
    " specified, the input inputFile's directory will be used.")
  val recursive = parser.flag[Boolean](List("R", "recursive"), "If the input parameter is a directory, recursively tokenize" +
    " all xml files in or below that directory.")
  val skipRegex = parser.option[String](List("skip"), "REGEX", "Skip files whose absolute path matches this regex.")
*/
  val debug = parser.flag[Boolean](List("d", "debug"), "Assert this flag if you want to see ridicuous quantities of output.")


  var log: SimpleLogger = new SimpleLogger(
    this.getClass.getName,
    SimpleLogger.WARN,
    new BufferedWriter(new OutputStreamWriter(System.err)))

  def apply(inputFile: File, textFile: File) {
    log.debug("Started file transform\n")
    assert(inputFile.isFile, "input file is not a file.")
    // the output files should be ready to have ".LANG.txt" or ".LANG.tok" appended

    //ensure the appropriate parent dirs exist

    log.debug("Making parent directories and text file\n")
    new File(textFile.getParent).mkdirs()
    log.debug("%s -> %s.{langs...}.txt\n".format(inputFile.getPath, textFile.getPath))

    try {
      log.debug("Loading XML\n")
      val xmlTree = XML.load(new InputStreamReader(new FileInputStream(inputFile), "UTF-8")) \ "file"
      val languages = (xmlTree \ "@languages").text.split(",").toList
      log.debug("Opening output streams\n")
      var defaultFile = new File(textFile.getPath + ".unknownLanguage.txt")
      val defaultTextOutputWriter = new OutputStreamWriter(new FileOutputStream(
        defaultFile), "UTF-8")
      val langToFile: Map[String, OutputStreamWriter] =
        (for (lang <- languages) yield (lang,
          new OutputStreamWriter(new FileOutputStream(
            new File("%s.%s.txt".format(textFile.getPath, lang))), "UTF-8")
          )).toMap.withDefault(x => defaultTextOutputWriter)

      log.debug("Parsing XML\n")
      var flatTextNodes = false
      var textSentenceNodes = false
      xmlTree \\ "align" foreach {
        (align) =>
          val textNodes = (align \ "text")
          val langToTextNodeStringList = textNodes.map(textNode => (
            (textNode \ "@langid").text,
            (textNode \ "s").map(sentenceNode => "%s <EOS>".format(sentenceNode.text.replaceAll("\\n"," "))).mkString(" ")
            )).toList
          val langToTextNodeString = langToTextNodeStringList.toMap
          var missingLangs = List[String]()
          for ((lang, file) <- langToFile) {
            if (langToTextNodeString.contains(lang)) {
              file.write(langToTextNodeString(lang) + "\n")
            } else {
              missingLangs = lang :: missingLangs
              file.write("\n")
            }
          }
          if (missingLangs.length > 0) {
            log.err("In file %s, missing language%s \"%s\" in the following align node. All align nodes must contain a single text node for each language:\n%s\n\n\n".format(inputFile.getName, if (missingLangs.length > 1) "s" else "", missingLangs.mkString(","), align.toString()))
          }
          if (langToTextNodeString.size != langToTextNodeStringList.length) {
            // then there was a key that appeared twice.
            log.err("In file %s, there is more than one text node for a language. All align nodes must contain a single text node for each language:\n%s\n\n\n".format(inputFile.getName, align.toString()))
          }
      }
      if (flatTextNodes) {
        log.warn("Detected flat text nodes. The <text><sentence/></text> is recommended.\n")
      }
      if (flatTextNodes && textSentenceNodes) {
        log.warn("Detected both flat text nodes and <text><sentence/></text> hierarchy. Mixing these styles is _not_ recommended.\n")
      }

      log.debug("Closing streams\n")
      for ((name, ostream) <- langToFile) {
        ostream.close()
      }
      defaultTextOutputWriter.close()
      if (defaultFile.length() == 0) {
        defaultFile.delete()
      }
    } catch {
      case e: SAXParseException =>
        log.err("Malformed XML in input file: %s, column: %s, line: %s, message: %s\n".format(inputFile.getAbsolutePath,
          e.getColumnNumber, e.getLineNumber, e.getMessage))
        return
      case e: Exception =>
        log.err("Caught an error: %s".format(e.getMessage))
        return
    }
    log.debug("Exiting file transform\n")
  }

  def applyDir(inputDir: File, textDir: File) {
    assert(inputDir.isDirectory)
    for (child <- inputDir.listFiles().sorted) {
      if (child.isDirectory) {
        val pathDescentStep = child.getName
        applyDir(child, new File(textDir, pathDescentStep))
      } else if (child.isFile && child.getName.endsWith(".xml")) {
        apply(child, new File(textDir, child.getName.substring(0, child.getName.length() - 4)))
      }
    }
  }

  def main(args: Array[String]) {
    var warnings = 0
    var errors = 0
    try {
      parser.parse(args)

      if (help.value.isDefined) {
        parser.usage()
      }
      if (debug.value.isDefined) {
        log.logLevel = SimpleLogger.DEBUG
      }
      val inputFile = input.value match {
        case Some(filename) => new File(filename).getAbsoluteFile
        case None => parser.usage("You must specify an input file")
      }
      val textFile = textOutput.value match {
        case Some(filename) => new File(filename)
        case None => parser.usage("You must specify a text file")
      }
      if (inputFile.isFile) {
        apply(inputFile, textFile)
      } else if (inputFile.isDirectory) {
        applyDir(inputFile, textFile)
      } else {
        parser.usage("input file must be a regular file")
      }
      val (transformWarnings,transformErrors) = log.getStats()
      warnings = transformWarnings
      errors = transformErrors
      log.summary("Warnings,Errors: %s\n".format((warnings,errors)))
    }
    catch {
      case e: ArgotUsageException =>
        println(e.message)
    }
    System.exit(errors)
  }

}
