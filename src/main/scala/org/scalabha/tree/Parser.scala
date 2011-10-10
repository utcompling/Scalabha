package org.scalabha.tree

import org.clapper.argot._
import collection.mutable.MutableList
import org.scalabha.model._
import org.scalabha.log.SimpleLogger
import java.io.{OutputStreamWriter, BufferedWriter}

object Parser {

  import ArgotConverters._

  val parser = new ArgotParser("org.scalabha.preproc.Tokenizer", preUsage = Some("Version 0.0"))
  val help = parser.flag[Boolean](List("h", "help"), "print help")
  val input = parser.option[String](List("i", "input"), "FILE", "input inputFile to tokenize")
  val log = new SimpleLogger("org.scalabha.tree.Parser", SimpleLogger.WARN, new BufferedWriter(new OutputStreamWriter(System.err)))
  val noLog = new SimpleLogger("org.scalabha.tree.Parser", SimpleLogger.NONE, new BufferedWriter(new OutputStreamWriter(System.err)))

  def apply(line: String, prefix: String, log: SimpleLogger): Option[(TreeNode, String)] = {
    log.info("%sparsing:<<%s>>\n".format(prefix, line))
    val regex = """\s*(\(?)\s*([^\s)(]+)\s*(.*)""".r
//    if (line.matches("""\s*(\(?)\s*([^\s)(]+)\s*(.*)"""))
    line match {
      case regex(open, sym, rest) =>
        if (open != "") {
          // then we are parsing a full node.
          val name = sym
          var children = List[TreeNode]()
          var next: TreeNode = null
          var rest2 = rest
          while (!rest2.matches("\\s*\\).*")) {
            if (rest2 == "") {
              log.err("Missing closing paren in:<<%s>>\n".format(line))
              return None
            }
            apply(rest2, "|\t%s".format(prefix), log) match {
              case Some((a, b)) =>
                next = a
                rest2 = b
                children = children ::: List(next)
              case None => return None
            }
          }
          val cutoff = rest2.indexOf(')')
          log.info("%sresult: %s,\"%s\"\n".format(prefix, Node(name, children), rest2.substring(cutoff + 1)))
          return Some((Node(name, children), rest2.substring(cutoff + 1)))
        } else {
          // then we are only looking at a value
          log.info("%sresult: %s,\"%s\"\n".format(prefix, Value(sym), rest))
          return Some((Value(sym), rest))
        }
      case _ =>
        log.err("Got an empty input line\n")
        return None
    }
  }

  def apply(line: String, log: SimpleLogger): Option[TreeNode] = {
    apply(line, "", log) match {
      case Some((tree, "")) =>
        tree match {
          case Node(_, _) => ()
          case _ =>
            log.warn("Top-level element is not a tree:<<%s>>\n".format(line))
        }
        Some(tree)
      case Some((tree, leftover)) =>
        if (leftover.matches("\\s+")) {
          log.warn("Please delete the extra whitespace found at the end of this line: <<%s>>\n".format(line))
          Some(tree)
        } else {
          log.err("Malformed tree:<<%s>>\tleftover text:<<%s>>\n".format(line, leftover))
          None
        }
      case None =>
        None
    }
  }

  def apply(line: String): Option[TreeNode] = {
    apply(line, noLog)
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
        println(Parser(line, log))
      }

      log.summary("Warnings,Errors: %s\n".format(log.getStats()))
    }
    catch {
      case e: ArgotUsageException =>
        println(e.message)
    }
  }
}