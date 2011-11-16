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
  val parser = new ArgotParser("opennlp.scalabha.preproc.Tokenizer", preUsage = Some("Version 0.0"))
  val help = parser.flag[Boolean](List("h", "help"), "print help")
  val input = parser.option[String](List("x", "xml-input"), "FILE_OR_DIR", "Input inputFile or directory to tokenize")
  val textOutput = parser.option[String](List("t", "text-output"), "FILE_OR_DIR", "Output location for intermediate text files. " +
    "If none is specified, the input inputFile's directory will be used, and the intermediate inputFile will be deleted during cleanup.")
  /*val output = parser.option[String](List("o", "output"), "FILE_OR_DIR", "Output location for token files. If none is" +
    " specified, the input inputFile's directory will be used.")
  val recursive = parser.flag[Boolean](List("R", "recursive"), "If the input parameter is a directory, recursively tokenize" +
    " all xml files in or below that directory.")
  val debug = parser.flag[Boolean](List("d", "debug"), "Assert this flag if you want to see ridicuous quantities of output.")
  val skipRegex = parser.option[String](List("skip"), "REGEX", "Skip files whose absolute path matches this regex.")
*/


  var log: SimpleLogger = new SimpleLogger(
    this.getClass.getName,
    SimpleLogger.TRACE,
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

      log.debug("Closing streams\n")
      for ((name, ostream) <- langToFile) {
        ostream.close()
      }
      defaultTextOutputWriter.close()
      if (defaultFile.length() == 0){
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
        applyDir(child, new File(textDir,pathDescentStep))
      } else if (child.isFile && child.getName.endsWith(".xml")) {
        apply(child,new File(textDir,child.getName.substring(0,child.getName.length()-4)))
      }
    }
  }

  def main(args: Array[String]) {

    try {
      parser.parse(args)

      if (help.value.isDefined) {
        parser.usage()
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
      /*
      if (inputFile.isDirectory && recursive.value.isDefined) {
        log.debug("Main: doing recursive option\n")
        // then recursively descend and transform all files
        // treat the output files as directories and reconstruct the descent tree as a tree rooted there.
        transformDirectoryRecursive(inputFile, "", textFileName)
      } else if (inputFile.isDirectory) {
        log.debug("Main: doing directory option\n")
        // then just loop over all the files in inputFile
        // treat the output files as directories and create all the output files there.
        transformDirectory(inputFile, "", textFileName)
      } else {
        log.debug("Main: doing single file option\n")

        // then just transform inputFile
        // treat the output files as files and write them out.
        val textOutputFileNameStripped = FileUtils.getStrippedOutputFileName(
          (if (textOutput.value.isDefined) textOutput.value.get else inputFile.getParent), "",
          inputFile.getName.replaceFirst(".xml$", ""))
        val tokenOutputFileNameStripped = FileUtils.getStrippedOutputFileName(
          (if (output.value.isDefined) output.value.get else inputFile.getParent), "",
          inputFile.getName.replaceFirst(".xml$", ""))
        transformFile(inputFile, textOutputFileNameStripped, tokenOutputFileNameStripped, skipFiles, log)

      }*/

      log.summary("Warnings,Errors: %s\n".format(log.getStats()))
    }
    catch {
      case e: ArgotUsageException =>
        println(e.message)
    }
  }

}
