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

object XmlToInfo {

  import ArgotConverters._

  var log: SimpleLogger = new SimpleLogger(
    this.getClass.toString,
    SimpleLogger.TRACE,
    new BufferedWriter(new OutputStreamWriter(System.err)))

  def transformFile(inputFile: File, infoFileNameStripped: String, log: SimpleLogger) {
    log.debug("Started file transform\n")
    assert(inputFile.isFile, "input file is not a file.")
    // the output files should be ready to have ".LANG.txt" or ".LANG.tok" appended
    assert(!infoFileNameStripped.endsWith(".xml") && !infoFileNameStripped.endsWith(".xml"))

    //ensure the appropriate parent dirs exist

    log.debug("Making parent directories\n")
    new File(FileUtils.getPathParent(infoFileNameStripped)).mkdirs()

    log.trace("%s -> %s.trace\n".format(inputFile.getPath, infoFileNameStripped))

    try {
      log.debug("Loading XML\n")
      val root = XML.load(new InputStreamReader(new FileInputStream(inputFile), "UTF-8"))
      //      val datasetAttrs = Map[String,String]()
      val datasetAttrs = root.attributes.asAttrMap
      log.debug(datasetAttrs.toString)
      val xmlTree = root \ "file"
      val fileName = inputFile.getName
      val fileAttrs = (for (it <- xmlTree) yield {
        it.attributes.asAttrMap.iterator
      }).iterator.flatten.toMap
      log.debug(fileAttrs.toString)
      val metadataAttrs = (for (it <- xmlTree \ "metadata") yield {
        it.attributes.asAttrMap.iterator
      }).iterator.flatten.toMap
      log.debug(metadataAttrs.toString)
      val languages = (xmlTree \ "@languages").text.split(",").toList
      log.debug("Opening output streams\n")
      val infoFile = new File(infoFileNameStripped + ".trace")
      val infoFileWriter = new OutputStreamWriter(new FileOutputStream(
        infoFile), "UTF-8")

      log.debug("Parsing XML\n")
      xmlTree \\ "unit" foreach {
        (unit) =>
          val unitAttrs = unit.attributes.asAttrMap
          unit \ "align" foreach {
            (align) =>
              val noteAttrs = (for ((note, i) <- (align \ "note").zipWithIndex) yield {
                (note.attributes.asAttrMap.toList.map {
                  case (k, v) => ("%d-%s".format(i, k), v)
                } ::: List(("%d-text".format(i), note.text.replaceAll("\"|“|”", "'")))).iterator
              }).iterator.flatten.toMap

              infoFileWriter.write(("::source \"%s\" %s\n".format(fileName,
                List(
                  (for ((k, v) <- datasetAttrs if (v.length > 0)) yield "::data-%s \"%s\"".format(k, v)).toList.mkString(" "),
                  (for ((k, v) <- fileAttrs if (v.length > 0)) yield "::file-%s \"%s\"".format(k, v)).toList.mkString(" "),
                  (for ((k, v) <- metadataAttrs if (v.length > 0)) yield "::meta-%s \"%s\"".format(k, v)).toList.mkString(" "),
                  (for ((k, v) <- unitAttrs if (v.length > 0)) yield "::unit-%s \"%s\"".format(k, v)).toList.mkString(" "),
                  (for ((k, v) <- noteAttrs if (v.length > 0)) yield "::note-%s \"%s\"".format(k, v)).toList.mkString(" ")
                ).mkString(" ")
              )))
          }
      }
      log.debug("Closing streams\n")
      infoFileWriter.close()

      if (infoFile.length() == 0) {
        infoFile.delete()
      }
    } catch {
      case e: SAXParseException =>
        log.err("Malformed XML in input file: %s, column: %s, line: %s, message: %s\n".format(inputFile.getAbsolutePath,
          e.getColumnNumber, e.getLineNumber, e.getMessage))
        return
      case e: Exception =>
        log.err("Caught an error: %s\n".format(e.getMessage))
        return
    }
    log.debug("Exiting file transform\n")
  }

  def transformDirectory(inputDirectory: File, newSubdirectories: String,
                         infoFileNameOption: Option[String], log: SimpleLogger) {
    for (inputFile <- inputDirectory.listFiles if (inputFile.isFile && inputFile.getName.endsWith("xml"))) {
      val infoFileNameStripped = FileUtils.getStrippedOutputFileName(
        (if (infoFileNameOption.isDefined) infoFileNameOption.get else inputFile.getParent),
        newSubdirectories, inputFile.getName.replaceFirst(".xml$", ""))
      transformFile(inputFile, infoFileNameStripped, log)
    }
  }

  def transformDirectoryRecursive(inputDirectory: File, newSubdirectories: String,
                                  infoFileNameOption: Option[String], log: SimpleLogger) {
    // first, transform all the xml files at the current level
    transformDirectory(inputDirectory, newSubdirectories, infoFileNameOption, log)
    // then do the same for all the child directories
    for (inputSubDirectory <- inputDirectory.listFiles() if (inputSubDirectory.isDirectory)) {
      transformDirectoryRecursive(inputSubDirectory, newSubdirectories + FileUtils.FILE_SEPARATOR + inputSubDirectory.getName,
        infoFileNameOption, log)
    }
  }

  def main(args: Array[String]) {
    val parser = new ArgotParser(this.getClass.getName, preUsage = Some("Version 0.0"))
    val help = parser.flag[Boolean](List("h", "help"), "print help")
    val input = parser.option[String](List("i", "input"), "FILE_OR_DIR", "Input inputFile or directory to tokenize")
    val infoFileNameOption = parser.option[String](List("o", "output"), "FILE_OR_DIR", "Output location for trace files. If none is" +
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
          this.getClass.toString,
          SimpleLogger.DEBUG,
          new BufferedWriter(new OutputStreamWriter(System.err)))

      val skipFiles =
        if (skipRegex.value.isDefined) skipRegex.value.get.r else "".r

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
          transformDirectoryRecursive(inputFile, "", infoFileNameOption.value, log)
        } else if (inputFile.isDirectory) {
          log.debug("Main: doing directory option\n")
          // then just loop over all the files in inputFile
          // treat the output files as directories and create all the output files there.
          transformDirectory(inputFile, "", infoFileNameOption.value, log)
        } else {
          log.debug("Main: doing single file option\n")

          // then just transform inputFile
          // treat the output files as files and write them out.
          val infoFileNameStripped = FileUtils.getStrippedOutputFileName(
            (if (infoFileNameOption.value.isDefined) infoFileNameOption.value.get else inputFile.getParent), "",
            inputFile.getName.replaceFirst(".xml$", ""))
          transformFile(inputFile, infoFileNameStripped, log)
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
