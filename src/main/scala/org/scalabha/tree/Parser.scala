package org.scalabha.tree
import org.clapper.argot._

object Parser {
  import ArgotConverters._
  val parser = new ArgotParser("org.scalabha.lang.Tokenizer", preUsage = Some("Version 0.0"))
  val help = parser.flag[Boolean](List("h", "help"), "print help")
  val input = parser.option[String](List("i", "input"), "FILE", "input file to tokenize")

  def apply(line:String, prefix: String){
    val treeNodeRE = """\s*\(\s*(\S+)\s+(.*)\)\s*""".r
    line match {
      case treeNodeRE(name, contents) =>
        println("%sname:<<%s>> contents:<<%s>>".format(prefix,name, contents.trim))
        apply(contents, "%s|\t".format(prefix))
      case _ =>
        println("%serror:%s".format(prefix,line))
    }
  }

  def apply(line:String){
    apply(line, "")
  }

  def main(args:Array[String]){
      try {
      parser.parse(args)

      if (help.value.isDefined) {
        parser.usage()
      }

      val input_file =
        (if (input.value.isDefined) {
          scala.io.Source.fromFile(input.value.get, "UTF-8")
        } else {
          scala.io.Source.stdin
        }).getLines()

      for (line:String <- input_file){
        Parser(line)
      }
    }
    catch {
      case e: ArgotUsageException =>
        println(e.message)
    }
  }
}