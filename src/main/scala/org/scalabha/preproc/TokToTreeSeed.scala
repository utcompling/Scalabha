package org.scalabha.preproc

import scala.xml._
import org.clapper.argot.ArgotParser._
import org.scalabha.log.SimpleLogger
import org.clapper.argot.{ArgotUsageException, ArgotParser, ArgotConverters}
import java.io._
import scala.sys.process._
import org.xml.sax.SAXParseException
import org.scalabha.util.FileUtils

object TokToTreeSeed {
  val CLASS_NAME = TokToTreeSeed.getClass.toString
  import ArgotConverters._

  var log: SimpleLogger = new SimpleLogger(
    CLASS_NAME,
    SimpleLogger.INFO,
    new BufferedWriter(new OutputStreamWriter(System.err)))

  def transformFile(inputFile: File, treeSeedOutputFileNameStripped: String) {
    log.debug("Started file transform\n")
    assert(inputFile.isFile, "input file is not a file.")
    // the output files should be ready to have ".LANG.txt" or ".LANG.tok" appended
    assert(!treeSeedOutputFileNameStripped.endsWith(".tok"))

    //ensure the appropriate parent dirs exist

    log.debug("Making parent directories\n")
    new File(FileUtils.getPathParent(treeSeedOutputFileNameStripped)).mkdirs()

    log.info("%s -> %s.treeseed\n".format(inputFile.getPath, treeSeedOutputFileNameStripped))

    val lines = scala.io.Source.fromFile(inputFile, "ISO-8859-1").getLines()
    val outputWriter = new OutputStreamWriter(new FileOutputStream(
      new File(treeSeedOutputFileNameStripped + ".treeseed")), "ISO-8859-1")
    outputWriter.write((for (line: String <- lines) yield {
      val sentences = line.split("<EOS>")
      "(TOP %s)".format(
        (for (sentence <- sentences) yield {
        val tokens = sentence.split("\\s+")
        "(S %s)".format(
          (for (token <- tokens) yield {
            "(x %s)".format(token) // TODO this is where one would include a tag lookup if so inclined
          }).mkString(" "))
      }).mkString(" "))
    }).mkString("\n") + "\n")

    log.debug("Exiting file transform\n")
  }

  def transformDirectory(inputDirectory: File, newSubdirectories: String,
                         treeSeedOutputOption: Option[String]) {
    for (inputFile <- inputDirectory.listFiles if (inputFile.isFile && inputFile.getName.endsWith("tok"))) {
      val treeSeedOutputFileNameStripped = FileUtils.getStrippedOutputFileName(
        (if (treeSeedOutputOption.isDefined) treeSeedOutputOption.get else inputFile.getParent),
        newSubdirectories, inputFile.getName.replaceFirst(".tok$", ""))
      transformFile(inputFile, treeSeedOutputFileNameStripped)
    }
  }

  def transformDirectoryRecursive(inputDirectory: File, newSubdirectories: String,
                                  treeSeedOutputOption: Option[String]) {
    // first, transform all the xml files at the current level
    transformDirectory(inputDirectory, newSubdirectories, treeSeedOutputOption)
    // then do the same for all the child directories
    for (inputSubDirectory <- inputDirectory.listFiles() if (inputSubDirectory.isDirectory)) {
      transformDirectoryRecursive(
        inputSubDirectory,
        newSubdirectories + FileUtils.FILE_SEPARATOR + inputSubDirectory.getName,
        treeSeedOutputOption)
    }
  }

  def main(args: Array[String]) {
    val parser = new ArgotParser(CLASS_NAME, preUsage = Some("Version 0.0"))
    val help = parser.flag[Boolean](List("h", "help"), "print help")
    val input = parser.option[String](List("i", "input"), "FILE_OR_DIR",
      "Input token file or directory of token files to transform into tree seeds (rudimentary syntax trees)")
    val output = parser.option[String](List("o", "output"), "FILE_OR_DIR",
      "Output location for tree seed (*.treeseed) files. If none is specified, the input" +
        " file's directory will be used.")
    val recursive = parser.flag[Boolean](List("R", "recursive"),
      "If the input parameter is a directory, recursively transform" +
      " all tok files in or below that directory.")
    val debug = parser.flag[Boolean](List("d", "debug"), "Assert this flag if you want to see ridicuous quantities of output.")

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

      if (input.value.isDefined) {
        val inputFile = new File(input.value.get).getAbsoluteFile
        if (!inputFile.exists()) {
          log.err("input file does not exist.")
          System.exit(1)
        }
        if (inputFile.isDirectory && recursive.value.isDefined) {
          log.debug("Main: doing recursive option\n")
          // then recursively descend and transform all files
          // treat the output files as directories and reconstruct the descent tree as a tree rooted there.
          transformDirectoryRecursive(inputFile, "",output.value)
        } else if (inputFile.isDirectory) {
          log.debug("Main: doing directory option\n")
          // then just loop over all the files in inputFile
          // treat the output files as directories and create all the output files there.
          transformDirectory(inputFile, "", output.value)
        } else {
          log.debug("Main: doing single file option\n")

          // then just transform inputFile
          // treat the output files as files and write them out.
          val treeSeedOutputFileNameStripped = FileUtils.getStrippedOutputFileName(
            (if (output.value.isDefined) output.value.get else inputFile.getParent), "",
            inputFile.getName.replaceFirst(".tok$", ""))
          transformFile(inputFile, treeSeedOutputFileNameStripped)
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