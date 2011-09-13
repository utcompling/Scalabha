package org.scalabha.tree

import org.clapper.argot._
import collection.mutable.MutableList
import org.scalabha.model._

object Parser {

  import ArgotConverters._

  val parser = new ArgotParser("org.scalabha.lang.Tokenizer", preUsage = Some("Version 0.0"))
  val help = parser.flag[Boolean](List("h", "help"), "print help")
  val input = parser.option[String](List("i", "input"), "FILE", "input file to tokenize")

  def apply(line: String, prefix: String): (TreeNode, String) = {
    println("%sparsing:<<%s>>".format(prefix, line))
    val regex = """\s*(\(?)\s*([^\s)]+)(.*)""".r

    val regex(open, sym, rest) = line
    if (open != "") {
      // then sym is the name\
      val name = sym
      var children = List[TreeNode]()
      // parse the next piece
//      val (a, b) = apply(rest, "|\t%s".format(prefix), Empty())
      var next: TreeNode = null
      var rest2 = rest
//      children = children ::: List(next)
      while (!rest2.matches("\\s*\\).*")) {
        val (a, b) = apply(rest2, "|\t%s".format(prefix))
        next = a
        rest2 = b
        children = children ::: List(next)
      }
      val cutoff = rest2.indexOf(')')
      println("%sresult: %s,\"%s\"".format(prefix, Node(name, children), rest2.substring(cutoff+1)))
      return (Node(name, children), rest2.substring(cutoff+1))
    } else {
      // then sym is a value
      println("%sresult: %s,\"%s\"".format(prefix, Value(sym), rest))
      return (Value(sym), rest)
    }

    (Empty(), "")
  }

  def apply(line: String) {
    apply(line, "")
  }

  def main(args: Array[String]) {
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

      for (line: String <- input_file) {
        Parser(line)
      }
    }
    catch {
      case e: ArgotUsageException =>
        println(e.message)
    }
  }
}