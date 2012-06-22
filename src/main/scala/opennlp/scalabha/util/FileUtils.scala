package opennlp.scalabha.util

import java.io.BufferedWriter
import java.io.FileWriter
import java.io.BufferedReader
import java.io.FileReader
import java.io.File
import scala.io.BufferedSource

object FileUtils {
  var FILE_SEPARATOR = System.getProperty("file.separator")

  def getPathParent(path: String): String = {
    val a :: as = path.split(FILE_SEPARATOR).toList.reverse
    as.reverse.mkString(FILE_SEPARATOR)
  }

  def getPathNameStripEndSlash(str: String): String = {
    ((FILE_SEPARATOR + "*$").r).replaceFirstIn(str, "")
  }

  def trimSlashes(str: String): String =
    (("^%s*|%s*$".format(FILE_SEPARATOR, FILE_SEPARATOR).r).replaceAllIn(str, ""))

  def getStrippedOutputFileName(outputPath: String, newSubdirectories: String, inputBaseName: String): String =
    List(getPathNameStripEndSlash(outputPath), trimSlashes(newSubdirectories), trimSlashes(inputBaseName)).filter((s) => s != "").mkString(FILE_SEPARATOR)

  /**
   * Automatic Resource Management.  Ensure that the resource is closed after
   * executing the block.
   *
   * Example:
   *   using(new BufferedReader(new FileReader("file"))) { r =>
   *     var count = 0
   *     while (r.readLine != null) count += 1
   *     println(count)
   *   }
   */
  def using[T <: { def close() }, R](resource: T)(block: T => R): R = {
    try {
      block(resource)
    }
    finally {
      if (resource != null) resource.close()
    }
  }

  /**
   * Open a file for reading, execute a block of code, and ensure that the
   * file is closed when finished.
   */
  def readUsing[R](filename: String)(block: BufferedSource => R): R = {
    using(io.Source.fromFile(filename))(block)
  }

  /**
   * Open a file for reading, execute a block of code, and ensure that the
   * file is closed when finished.
   */
  def readUsing[R](file: File)(block: BufferedSource => R): R = {
    using(io.Source.fromFile(file))(block)
  }

  /**
   * Open a file for writing, execute a block of code, and ensure that the
   * file is closed when finished.
   */
  def writeUsing[R](filename: String)(block: BufferedWriter => R): R = {
    using(new BufferedWriter(new FileWriter(filename)))(block)
  }

  /**
   * Open a file for writing, execute a block of code, and ensure that the
   * file is closed when finished.
   */
  def writeUsing[R](file: File)(block: BufferedWriter => R): R = {
    using(new BufferedWriter(new FileWriter(file)))(block)
  }

}
