package opennlp.scalabha.tree

import org.clapper.argot._
import opennlp.scalabha.model._
import opennlp.scalabha.log.SimpleLogger
import io.BufferedSource
import java.io._

object Merge {

  import ArgotConverters._

  val parser = new ArgotParser(this.getClass().getName, preUsage = Some("Version 0.0"))
  val help = parser.flag[Boolean](List("h", "help"), "print help")
  val input = parser.option[String](List("i", "input"), "FILEorDIR", "tree source file or directory to compile")
  val output = parser.option[String](List("o", "output"), "FILE", "output file to write compiled trees to.")
  val skipErrs = parser.flag[Boolean](List("f", "skipErrs"), "Do not exit on errors. " +
    "The default is to exit as soon as errors are caught in any input file.")
  val pprintErrs = parser.flag[Boolean](List("pprintErrs"), "Format treenodes nicely in error reporting.")

  var log = new SimpleLogger(this.getClass().getName, SimpleLogger.WARN, new BufferedWriter(new OutputStreamWriter(System.err)))


  def applyFile(file: File): List[TreeNode] = {
    if (file.getName.endsWith("tree"))
      MultiLineTreeParser(file.getName, scala.io.Source.fromFile(file, "UTF-8"))
    else {
      log.warn("Assuming that %s is not a tree file because it does not end with the extension '.tree'. Skipping...\n".format(file.getName))
      List[TreeNode]()
    }
  }

  def okToProceed() = (MultiLineTreeParser.log.getStats()._2 == 0 || skipErrs.value.isDefined)

  def applyDir(dir: File): List[TreeNode] = {
    (for (child <- dir.listFiles().sorted) yield {
      if (child.isDirectory) {
        applyDir(child)
      } else if (child.isFile && okToProceed) {
        applyFile(child)
      } else {
        // there are other types of files, and we'll just ignore them
        List[TreeNode]()
      }
    }).toList.flatten
  }

  def apply(fileString: String): List[TreeNode] = {
    // NOTE: fileString could be a filename, or a dirname
    // TODO: This method is written so that it can be extended to accept a list of files
    val inode = new File(fileString)
    var resultTrees = List[TreeNode]()
    if (inode.isDirectory) {
      resultTrees = applyDir(inode)
    } else if (inode.isFile) {
      resultTrees = applyFile(inode)
    } else {
      // there are other types of files
      log.warn("Got a file that is neither a directory nor a regular file: %s\n".format(fileString))
      List[TreeNode]()
    }
    resultTrees
  }

  def main(args: Array[String]) {
    try {
      parser.parse(args)

      if (help.value.isDefined) {
        parser.usage()
      }

      MultiLineTreeParser.log.logLevel = SimpleLogger.WARN
      MultiLineTreeParser.pprintErrs = pprintErrs.value.isDefined

      val parsedTrees = input.value match {
        case Some(filename) => apply(filename)
        case None => parser.usage("you must specify an input tree file or directory of input tree files")
      }


      val (compileWarnings, compileErrors) = log.getStats()
      val (parseWarnings, parseErrors) = MultiLineTreeParser.log.getStats()
      val (warnings, errors) = (compileWarnings + parseWarnings, compileErrors + parseErrors)

      log.summary("Warnings,Errors: %s\n".format((warnings, errors)))
      if (errors == 0 || skipErrs.value.isDefined) {
        val outputBuffer = output.value match {
          case Some(filename) =>
            if (filename.endsWith(".tree")) {
              (new File(filename)).getParentFile.mkdirs()
              new BufferedWriter(new OutputStreamWriter(new FileOutputStream(new File(filename))))
            } else {
              parser.usage("Output file must end with a '.tree' suffix")
            }
          case None => new BufferedWriter(new OutputStreamWriter(System.out))
        }
        outputBuffer.write(parsedTrees.map(tree => tree.getCanonicalString()).mkString("\n") + "\n")

        outputBuffer.close()
      } else {
        log.summary("Suspending output since there were errors.\n")
        System.exit(errors)
      }
    } catch {
      case e: ArgotUsageException =>
        println(e.message)
    }
  }
}
