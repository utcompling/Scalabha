package opennlp.scalabha.util

import java.io.BufferedWriter
import java.io.FileWriter
import java.io.BufferedReader
import java.io.FileReader
import java.io.File
import scala.io.BufferedSource
import scala.io.Source
import java.io.Writer

object FileUtils {
  var FILE_SEPARATOR = System.getProperty("file.separator")

  //
  // Path string manipulation
  //

  def pathjoin(parts: String*): String =
    (parts.dropRight(1).filter(_.nonEmpty).map(getPathNameStripEndSlash) :+ parts.last).mkString(File.separator)

  def getPathParent(path: String): String =
    path.split(FILE_SEPARATOR).dropRight(1).mkString(FILE_SEPARATOR)

  def getPathNameStripEndSlash(str: String): String =
    ((FILE_SEPARATOR + "*$").r).replaceFirstIn(str, "")

  def trimSlashes(str: String): String =
    (("^%s*|%s*$".format(FILE_SEPARATOR, FILE_SEPARATOR).r).replaceAllIn(str, ""))

  def getStrippedOutputFileName(outputPath: String, newSubdirectories: String, inputBaseName: String): String =
    List(getPathNameStripEndSlash(outputPath), trimSlashes(newSubdirectories), trimSlashes(inputBaseName)).filter((s) => s != "").mkString(FILE_SEPARATOR)

  //
  // Basic file utilities
  //

  def getName(filepath: String) =
    filepath.split(FILE_SEPARATOR).last

  def exists(filename: String) =
    new File(filename).exists()

  def mkdir(filename: String) {
    val f = new File(filename)
    if (!f.exists) f.mkdir()
  }

  def mkdirpath(dirpath: String) {
    val parts = dirpath.split(FILE_SEPARATOR)
    for (i <- 1 to parts.size)
      mkdir(parts.take(i).mkString("", "/", "/"))
  }

  def remove(filename: String) =
    new File(filename).delete()

  def mktemp(prefix: String = "temp-", suffix: String = ""): String = {
    val f = File.createTempFile(prefix, suffix)
    f.delete()
    f.getAbsolutePath
  }

  def findAllFiles(topDir: String, path: Vector[String] = Vector()): Set[Vector[String]] = {
    new File(pathjoin((topDir +: path): _*)).listFiles.flatMap { f =>
      if (f.isDirectory)
        findAllFiles(topDir, path :+ f.getName)
      else
        Set(path :+ f.getName)
    }.toSet
  }

  def findBinary(name: String, binDir: Option[String] = None, envar: Option[String] = None, verbose: Boolean = false): String = {
    val checked = collection.mutable.Buffer[String]()

    if (binDir.isDefined) {
      val path = binDir.get + "/" + name
      if (FileUtils.exists(path))
        return path
      else
        checked += path
    }

    if (envar.isDefined) {
      val envpath = System.getenv(envar.get)
      if (envpath != null) {
        val path = envpath + "/" + name
        if (FileUtils.exists(path))
          return path
        else
          checked += path
      }
    }

    try {
      val found = scala.sys.process.Process(List("which", name)) !!;
      return found.trim
    }
    catch {
      case _ => {
        checked += "which " + name
      }
    }

    throw new RuntimeException("No binary found.  Checked the following:\n" + checked.map((" ") * 16 + _).mkString("\n"))
  }

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
    using(Source.fromFile(filename))(block)
  }

  /**
   * Open a file for reading, execute a block of code, and ensure that the
   * file is closed when finished.
   */
  def readUsing[R](file: File)(block: BufferedSource => R): R = {
    using(Source.fromFile(file))(block)
  }

  /**
   * Get an Iterator over the lines in the file.  The file will automatically
   * close itself when the end of the file is reached.  This gets around the
   * problem of having to all of your processing inside the `using` block.
   */
  def readLines(filename: String): Iterator[String] = {
    readLines(new File(filename))
  }

  /**
   * Get an Iterator over the lines in the file.  The file will automatically
   * close itself when the end of the file is reached.  This gets around the
   * problem of having to all of your processing inside the `using` block.
   */
  def readLines(filename: String, encoding: String): Iterator[String] = {
    readLines(new File(filename), Some(encoding))
  }

  /**
   * Get an Iterator over the lines in the file.  The file will automatically
   * close itself when the end of the file is reached.  This gets around the
   * problem of having to all of your processing inside the `using` block.
   */
  def readLines(file: File, encoding: Option[String] = None): Iterator[String] = {
    val resource = encoding match {
      case Some(enc) => Source.fromFile(file, enc)
      case None => Source.fromFile(file)
    }
    val blockItr = resource.getLines
    var finished = false
    new Iterator[String] {
      override def next() = {
        hasNext()
        if (finished) throw new NoSuchElementException("next on empty iterator")
        val n = blockItr.next
        hasNext()
        n
      }
      override def hasNext() = {
        if (finished)
          false
        else {
          val hn = blockItr.hasNext
          if (!hn) {
            finished = true
            if (resource != null) resource.close()
          }
          hn
        }
      }
    }
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
  def writeUsing[R](dirname: String, filename: String)(block: BufferedWriter => R): R = {
    mkdirpath(dirname)
    using(new BufferedWriter(new FileWriter(dirname + FILE_SEPARATOR + filename)))(block)
  }

  /**
   * Open a file for writing, execute a block of code, and ensure that the
   * file is closed when finished.
   */
  def writeUsing[R](file: File)(block: BufferedWriter => R): R = {
    using(new BufferedWriter(new FileWriter(file)))(block)
  }

  def dumpToFile(data: String): String = {
    val filename = mktemp()
    writeUsing(filename)(_.write(data))
    filename
  }

  /**
   * Add a method `writeLine` to Writer classes
   */
  class WriterWithWriteLine(self: Writer) {
    def writeLine(line: String) { self.write(line + "\n") }
  }
  implicit def writerWithWriteLine(self: Writer): WriterWithWriteLine = new WriterWithWriteLine(self)

}
