package opennlp.scalabha.util

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

}
