package org.scalabha.preproc

import scala.xml._
import org.clapper.argot.ArgotParser._
import org.scalabha.log.SimpleLogger
import org.clapper.argot.{ArgotUsageException, ArgotParser, ArgotConverters}
import java.io._
import scala.sys.process._
import org.xml.sax.SAXParseException

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
  val log = new SimpleLogger(XmlToTxt.getClass.toString, SimpleLogger.INFO, new BufferedWriter(new OutputStreamWriter(System.err)))
  var FILE_SEPARATOR = System.getProperty("file.separator")

  def getParent(path: String): String = {
    val a :: as = path.split(FILE_SEPARATOR).toList.reverse
    as.reverse.mkString(FILE_SEPARATOR)
  }

  def transformFile(inputFile: File, textOutputFileNameStripped: String, tokenOutputFileNameStripped: String) {
    assert(inputFile.isFile, "input file is not a file.")
    // the output files should be ready to have ".LANG.txt" or ".LANG.tok" appended
    assert(!tokenOutputFileNameStripped.endsWith(".xml") && !textOutputFileNameStripped.endsWith(".xml"))

    //ensure the appropriate parent dirs exist

    new File(getParent(textOutputFileNameStripped)).mkdirs()
    new File(getParent(tokenOutputFileNameStripped)).mkdirs()

    log.info("%s -> %s.*.txt -> %s.*.tok\n".format(inputFile.getPath, textOutputFileNameStripped, tokenOutputFileNameStripped))

    try {
      val xmlTree = XML.load(new InputStreamReader(new FileInputStream(inputFile), "ISO-8859-1")) \ "file"
      val languages = (xmlTree \ "@languages").text.split(",").toList
      val defaultTextOutputWriter = new OutputStreamWriter(new FileOutputStream(
        new File(textOutputFileNameStripped + ".unknownLanguage.txt")), "ISO-8859-1")
      val langToFile: Map[String, OutputStreamWriter] =
        (for (lang <- languages) yield (lang,
          new OutputStreamWriter(new FileOutputStream(
            new File("%s.%s.txt".format(textOutputFileNameStripped, lang))), "ISO-8859-1")
          )).toMap.withDefault(x => defaultTextOutputWriter)

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
      defaultTextOutputWriter.close()

      for (lang <- languages) {
        (new File("%s.%s.txt".format(textOutputFileNameStripped, lang))) #>
          "normalize-text-standalone.pl" #| "tokenize-text.pl" #>
          (new File("%s.%s.tok".format(tokenOutputFileNameStripped, lang))) !
      }
    } catch
    {
      case e: SAXParseException =>
        log.err("Malformed XML in input file: %s, column: %s, line: %s, message: %s\n".format(inputFile.getAbsolutePath,
          e.getColumnNumber, e.getLineNumber, e.getMessage))
        return
      case e: Exception =>
        log.err("Caught an error: %s".format(e.getMessage))
        return
    }
  }

  def withoutEndSlash(str: String): String =
    ((FILE_SEPARATOR + "*$").r).replaceFirstIn(str, "")

  def trimSlashes(str: String): String =
    (("^%s*|%s*$".format(FILE_SEPARATOR, FILE_SEPARATOR).r).replaceAllIn(str, ""))

  def getStrippedOutputFileName(outputPath: String, newSubdirectories: String, inputBaseName: String): String =
    List(withoutEndSlash(outputPath), trimSlashes(newSubdirectories), trimSlashes(inputBaseName)).filter((s) => s != "").mkString(FILE_SEPARATOR)

  def transformDirectory(inputDirectory: File, newSubdirectories: String) {
    for (inputFile <- inputDirectory.listFiles if (inputFile.isFile && inputFile.getName.endsWith("xml"))) {
      val textOutputFileNameStripped = getStrippedOutputFileName(
        (if (textOutput.value.isDefined) textOutput.value.get else inputFile.getParent),
        newSubdirectories, inputFile.getName.replaceFirst(".xml$", ""))
      val tokenOutputFileNameStripped = getStrippedOutputFileName(
        (if (output.value.isDefined) output.value.get else inputFile.getParent),
        newSubdirectories, inputFile.getName.replaceFirst(".xml$", ""))
      transformFile(inputFile, textOutputFileNameStripped, tokenOutputFileNameStripped)
    }
  }

  def transformDirectoryRecursive(inputDirectory: File, newSubdirectories: String) {
    // first, transform all the xml files at the current level
    transformDirectory(inputDirectory, newSubdirectories)
    // then do the same for all the child directories
    for (inputSubDirectory <- inputDirectory.listFiles() if (inputSubDirectory.isDirectory)) {
      transformDirectoryRecursive(inputSubDirectory, newSubdirectories + FILE_SEPARATOR + inputSubDirectory.getName)
    }
  }

  def main(args: Array[String]) {
    try {
      parser.parse(args)

      if (help.value.isDefined) {
        parser.usage()
      }

      if (input.value.isDefined) {
        val fileName = input.value.get
        val inputFile = new File(input.value.get)
        if (!inputFile.exists()) {
          log.err("input file does not exist.")
          System.exit(1)
        }
        if (inputFile.isDirectory && recursive.value.isDefined) {
          // then recursively descend and transform all files
          // treat the output files as directories and reconstruct the descent tree as a tree rooted there.
          transformDirectoryRecursive(inputFile, "")
        } else if (inputFile.isDirectory) {
          // then just loop over all the files in inputFile
          // treat the output files as directories and create all the output files there.
          transformDirectory(inputFile, "")
        } else {
          // then just transform inputFile
          // treat the output files as files and write them out.
          val textOutputFileNameStripped = getStrippedOutputFileName(
            (if (textOutput.value.isDefined) textOutput.value.get else inputFile.getParent), "",
            inputFile.getName.replaceFirst(".xml$", ""))
          val tokenOutputFileNameStripped = getStrippedOutputFileName(
            (if (output.value.isDefined) output.value.get else inputFile.getParent), "",
            inputFile.getName.replaceFirst(".xml$", ""))
          transformFile(inputFile, textOutputFileNameStripped, tokenOutputFileNameStripped)
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