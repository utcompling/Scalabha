package org.scalabha.preproc

import scala.xml._
import org.clapper.argot.ArgotParser._
import org.scalabha.log.SimpleLogger
import org.clapper.argot.{ArgotUsageException, ArgotParser, ArgotConverters}
import java.io._
import scala.sys.process._
import org.xml.sax.SAXParseException
import org.scalabha.util.FileUtils
import java.util.regex.Pattern
import util.matching.Regex

object XmlToTok {

  import ArgotConverters._

  var log: SimpleLogger = new SimpleLogger(
    XmlToTok.getClass.toString,
    SimpleLogger.INFO,
    new BufferedWriter(new OutputStreamWriter(System.err)))

  def transformFile(inputFile: File, textOutputFileNameStripped: String,
                    tokenOutputFileNameStripped: String, skipFiles: String, log: SimpleLogger) {
    if (inputFile.getName.matches(skipFiles)) {
      log.info("File name %s matches skip regex: %s. Skipping...\n".format(inputFile.getName, skipFiles))
    } else {
      log.debug("Started file transform\n")
      assert(inputFile.isFile, "input file is not a file.")
      // the output files should be ready to have ".LANG.txt" or ".LANG.tok" appended
      assert(!tokenOutputFileNameStripped.endsWith(".xml") && !textOutputFileNameStripped.endsWith(".xml"))

      //ensure the appropriate parent dirs exist

      log.debug("Making parent directories\n")
      new File(FileUtils.getPathParent(textOutputFileNameStripped)).mkdirs()
      new File(FileUtils.getPathParent(tokenOutputFileNameStripped)).mkdirs()

      log.info("%s -> %s.*.txt -> %s.*.tok\n".format(inputFile.getPath, textOutputFileNameStripped, tokenOutputFileNameStripped))

      try {
        log.debug("Loading XML\n")
        val xmlTree = XML.load(new InputStreamReader(new FileInputStream(inputFile), "UTF-8")) \ "file"
        val languages = (xmlTree \ "@languages").text.split(",").toList
        log.debug("Opening output streams\n")
        val defaultTextOutputWriter = new OutputStreamWriter(new FileOutputStream(
          new File(textOutputFileNameStripped + ".unknownLanguage.txt")), "UTF-8")
        val langToFile: Map[String, OutputStreamWriter] =
          (for (lang <- languages) yield (lang,
            new OutputStreamWriter(new FileOutputStream(
              new File("%s.%s.txt".format(textOutputFileNameStripped, lang))), "UTF-8")
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

        log.debug("Piping text to tokenizer\n")
        for (lang <- languages) {
          val opt = (if (Set("eng", "mlg", "kin", "fra").contains(lang)) " -" + lang else "")
          (new File("%s.%s.txt".format(textOutputFileNameStripped, lang))) #>
            ("normalize-text-standalone.pl" + opt) #|
            ("tokenize-text.pl" + opt) #>
            (new File("%s.%s.tok".format(tokenOutputFileNameStripped, lang))) !
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
  }

  def transformDirectory(inputDirectory: File, newSubdirectories: String,
                         textOutputOption: Option[String], tokenOutputOption: Option[String], skipFiles: String, log: SimpleLogger) {
    for (inputFile <- inputDirectory.listFiles if (inputFile.isFile && inputFile.getName.endsWith("xml"))) {
      val textOutputFileNameStripped = FileUtils.getStrippedOutputFileName(
        (if (textOutputOption.isDefined) textOutputOption.get else inputFile.getParent),
        newSubdirectories, inputFile.getName.replaceFirst(".xml$", ""))
      val tokenOutputFileNameStripped = FileUtils.getStrippedOutputFileName(
        (if (tokenOutputOption.isDefined) tokenOutputOption.get else inputFile.getParent),
        newSubdirectories, inputFile.getName.replaceFirst(".xml$", ""))
      transformFile(inputFile, textOutputFileNameStripped, tokenOutputFileNameStripped, skipFiles, log)
    }
  }

  def transformDirectoryRecursive(inputDirectory: File, newSubdirectories: String,
                                  textOutputOption: Option[String], tokenOutputOption: Option[String], skipFiles: String, log: SimpleLogger) {
    // first, transform all the xml files at the current level
    transformDirectory(inputDirectory, newSubdirectories, textOutputOption, tokenOutputOption, skipFiles, log)
    // then do the same for all the child directories
    for (inputSubDirectory <- inputDirectory.listFiles() if (inputSubDirectory.isDirectory)) {
      transformDirectoryRecursive(inputSubDirectory, newSubdirectories + FileUtils.FILE_SEPARATOR + inputSubDirectory.getName,
        textOutputOption, tokenOutputOption, skipFiles, log)
    }
  }

  def main(args: Array[String]) {
    val parser = new ArgotParser("org.scalabha.preproc.Tokenizer", preUsage = Some("Version 0.0"))
    val help = parser.flag[Boolean](List("h", "help"), "print help")
    val input = parser.option[String](List("i", "input"), "FILE_OR_DIR", "Input inputFile or directory to tokenize")
    val textOutput = parser.option[String](List("m", "textOutput"), "FILE_OR_DIR", "Output location for intermediate text files. " +
      "If none is specified, the input inputFile's directory will be used, and the intermediate inputFile will be deleted during cleanup.")
    val output = parser.option[String](List("o", "output"), "FILE_OR_DIR", "Output location for token files. If none is" +
      " specified, the input inputFile's directory will be used.")
    val recursive = parser.flag[Boolean](List("R", "recursive"), "If the input parameter is a directory, recursively tokenize" +
      " all xml files in or below that directory.")
    val debug = parser.flag[Boolean](List("d", "debug"), "Assert this flag if you want to see ridicuous quantities of output.")
    val skipRegex = parser.option[String](List("skip"), "REGEX", "Skip files whose absolute path matches this regex.")

    try {
      parser.parse(args)

      if (help.value.isDefined) {
        parser.usage()
      }
      if (debug.value.isDefined)
        log = new SimpleLogger(
          XmlToTok.getClass.toString,
          SimpleLogger.DEBUG,
          new BufferedWriter(new OutputStreamWriter(System.err)))

      val skipFiles =
        if (skipRegex.value.isDefined) skipRegex.value.get else ""

      if (input.value.isDefined) {
        val fileName = input.value.get
        val inputFile = new File(input.value.get).getAbsoluteFile
        if (!inputFile.exists()) {
          log.err("input file does not exist.")
          System.exit(1)
        }
        if (inputFile.isDirectory && recursive.value.isDefined) {
          log.debug("Main: doing recursive option\n")
          // then recursively descend and transform all files
          // treat the output files as directories and reconstruct the descent tree as a tree rooted there.
          transformDirectoryRecursive(inputFile, "", textOutput.value, output.value, skipFiles, log)
        } else if (inputFile.isDirectory) {
          log.debug("Main: doing directory option\n")
          // then just loop over all the files in inputFile
          // treat the output files as directories and create all the output files there.
          transformDirectory(inputFile, "", textOutput.value, output.value, skipFiles, log)
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