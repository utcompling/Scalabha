package org.fiasana

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

  val debug = parser.flag[Boolean](List("d", "debug"), "Assert this flag if you want to see ridicuous quantities of output.")


  var log: SimpleLogger = new SimpleLogger(
    this.getClass.getName,
    SimpleLogger.WARN,
    new BufferedWriter(new OutputStreamWriter(System.err)))

  /**
   * @param xmlTree This is a parsed XML tree to be transformed to text
   * @param fileName This is the name of the file the XML came from. It's used for logging errors
   * @return A map from language name to list of text strings. Each string in the
   * list represents all the text for that language in an align node.
   * The text strings are in the same order they appeared in in the XML.
   */
  def apply(xmlTree: Elem, fileName: String): Map[String, List[String]] = {
    val languages = (xmlTree \ "file" \ "@languages").text.split(",").toList.map(s=>s.trim)
    var resultMap = languages.map(s=>(s,List[String]())).toMap
    log.debug("Parsing XML\n")
    xmlTree \\ "align" foreach {
      align =>
        val textNodes = (align \ "text")
        val langToText= textNodes.map( textNode => (
          (textNode \ "@langid").text,
          (textNode \ "s").map(
            sentenceNode =>
              "%s <EOS>".format(sentenceNode.text.replaceAll("\\n"," "))).mkString(" ")
          ))
        val langToTextMap = langToText.toMap.withDefaultValue("<EOS>")
        resultMap = resultMap.map{ // TODO is there a fancier functional way to do this?
          case(lang,list) => (lang,langToTextMap(lang)::list)
        }

        val missingLangs = resultMap.keySet.diff(langToTextMap.keySet)
        if (missingLangs.size > 0) {
          log.err(("In file %s, missing language%s \"%s\" " +
            "in the following align node. All align nodes must" +
            " contain a single text node for each language:\n%s\n\n\n")
            .format(fileName, if (missingLangs.size > 1) "s" else "",
            missingLangs.toList.sorted.mkString(","), align.toString()))
        }
        if (langToText.length != langToTextMap.size) {
          log.err(("In file %s, there is more than one text node " +
            "for a language. All align nodes must contain a single " +
            "text node for each language:\n%s\n\n\n")
            .format(fileName, align.toString()))
        }
        val unknownLanguages = langToTextMap.keySet.diff(resultMap.keySet)
        if (unknownLanguages.size > 0) {
          log.err("In file %s, found unknown language%s \"%s\" in align node:\n%s\n\n\n".format(
            fileName,
            if (unknownLanguages.size > 1) "s" else "",
            unknownLanguages.toList.sorted.mkString(","),
            align
          ))
        }
    }
    resultMap.map{
      case(lang,list) => (lang, list.reverse)
    }
  }

  /**
   * @param inputFile This is the XML file to transform to text
   * @param textFile This is the prefix file to use for generating output files.
   * The way it works is that textFile's path gets appended with ".lang.txt", where
   * ".lang" is substituted for each of the languages specified in the XML file.
   *
   * @return Nothing. The output is written to the files generated from textFile.
   */
  def apply(inputFile: File, textFile: File) {
    log.debug("Started file transform\n")
    assert(inputFile.isFile, "input file is not a file.")
    // the output files should be ready to have ".LANG.txt" or ".LANG.tok" appended

    //ensure the appropriate parent dirs exist

    log.debug("Making parent directories and text file\n")
    new File(textFile.getParent).mkdirs()
    log.debug("%s -> %s.{langs...}.txt\n".format(inputFile.getPath, textFile.getPath))
    try {
      log.debug("Extracting text from XML\n")
      val textLines = apply(XML.load(new InputStreamReader(new FileInputStream(inputFile), "UTF-8")),
        inputFile.getName)
      log.debug("Opening output streams\n")
      textLines.foreach{
        case(lang,lines) => {
          val writer = new OutputStreamWriter(new FileOutputStream(
            new File("%s.%s.txt".format(textFile.getPath, lang))), "UTF-8")
          lines.foreach(s=>writer.write(s+"\n"))
          writer.close()
        }
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

  /**
   * Recursively descend a directory structure, transforming XML to text files.
   * @param inputDir This is the root to start descending from
   * @param textDir  This is the root to start creating text files at.
   * The directory structure in inputDir will be recreated in textDir, so
   * <em>in/A.xml</em> is transformed to <em>in/A.lang1.txt</em> and
   * <em>in/another/path/B.xml</em> is
   * transformed to <em>in/another/path/B.lang1.txt</em>.
   */
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
