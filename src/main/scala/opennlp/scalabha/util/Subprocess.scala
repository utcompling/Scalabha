package opennlp.scalabha.util

import scala.sys.process._

/**
 * A class for conveniently running command-line operations.
 * 
 * e.g.
 *   val cmd = Subprocess.findBinary("tr")
 *   cmd.args("l", "L").call("hello")   // "heLLo"
 */
case class Subprocess(binary: String, args: Seq[String] = Nil) {

  /**
   * Create a callable subprocess object
   *
   * @param args		A list of command-line arguments.
   * @return: stdout
   */
  def args(args: String*) = new Subprocess(binary, args)

  /**
   * Call the binary
   *
   * @return: stdout
   */
  def call(): String = {
    val (exitcode, stdout, stderr) = callAllReturns()
    if (exitcode != 0)
      sys.error("ERROR CALLING: %s %s\nReturncode: %d\n%s".format(binary, args.mkString(" "), exitcode, stderr))
    stdout
  }

  /**
   * Call the binary with the given input
   *
   * @param inputStr	A string whose contents are used as stdin
   * @return: stdout
   */
  def call(inputStr: String): String = {
    val (exitcode, stdout, stderr) = callAllReturns(inputStr)
    if (exitcode != 0)
      sys.error("ERROR CALLING: %s %s\nReturncode: %d\n%s".format(binary, args.mkString(" "), exitcode, stderr))
    stdout
  }

  /**
   * Call the binary
   *
   * @return: (returncode, stdout, stderr)
   */
  def callAllReturns(): (Int, String, String) = {
    val out = new StringBuilder
    val err = new StringBuilder
    val exitcode = Process(binary +: args) ! ProcessLogger(out.append(_).append("\n"), err.append(_).append("\n"))
    (exitcode, out.result, err.result)
  }

  /**
   * Call the binary with the given input
   *
   * @param inputStr	A string whose contents are used as stdin
   * @return: (returncode, stdout, stderr)
   */
  def callAllReturns(input: String): (Int, String, String) = {
    val out = new StringBuilder
    val err = new StringBuilder
    val exitcode = Process(List("echo", input)) #| Process(binary +: args) ! ProcessLogger(out.append(_).append("\n"), err.append(_).append("\n"))
    (exitcode, out.result, err.result)
  }

}

object Subprocess {
  def findBinary(binaryName: String, binDir: Option[String] = None, envar: Option[String] = None) =
    new Subprocess(FileUtils.findBinary(binaryName, binDir, envar))
}
