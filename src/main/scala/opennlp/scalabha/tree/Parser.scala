package opennlp.scalabha.tree

import org.clapper.argot._
import collection.mutable.MutableList
import opennlp.scalabha.model._
import opennlp.scalabha.log.SimpleLogger
import java.io.{OutputStreamWriter, BufferedWriter}

object Parser {

  import ArgotConverters._

  val parser = new ArgotParser(this.getClass().getName, preUsage = Some("Version 0.0"))
  val help = parser.flag[Boolean](List("h", "help"), "print help")
  val input = parser.option[String](List("i", "input"), "FILE", "input inputFile to tokenize")
  val log = new SimpleLogger(this.getClass().getName, SimpleLogger.WARN, new BufferedWriter(new OutputStreamWriter(System.err)))
  val noLog = new SimpleLogger(this.getClass().getName, SimpleLogger.NONE, new BufferedWriter(new OutputStreamWriter(System.err)))

  def apply(index: Int, line: String, prefix: String, log: SimpleLogger): Option[(TreeNode, String)] = {
    log.trace("%sparsing:<<%s>>\n".format(prefix, line))
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
              log.err("Line %d: Missing closing paren in:<<%s>>\n".format(index, line))
              return None
            }
            apply(index, rest2, "|\t%s".format(prefix), log) match {
              case Some((a, b)) =>
                next = a
                rest2 = b
                children = children ::: List(next)
              case None => return None
            }
          }
          val cutoff = rest2.indexOf(')')
          if (children.length == 0 || (children.length > 0 && children.map( _.isInstanceOf[Value] ).reduce(_ || _) && children.length != 1)){
            log.err("Line %d: A leaf node may only contain a tag and a token. I.e., (TAG token). Tree node %s fails this test.\n".format(index, Node(name, children).getCanonicalString))
          }
          log.trace("%sresult: %s,\"%s\"\n".format(prefix, Node(name, children), rest2.substring(cutoff + 1)))
          return Some((Node(name, children), rest2.substring(cutoff + 1)))
        } else {
          // then we are only looking at a value
          log.trace("%sresult: %s,\"%s\"\n".format(prefix, Value(sym), rest))
          return Some((Value(sym), rest))
        }
      case "\\s*" =>
        log.err("Line %d: Got an empty input line\n".format(index))
        return None
      case x =>
        log.err("Line %d: Could not parse input line <<%s>>\n".format(index, x))
        return None
    }
  }

  def apply(index: Int, line: String, log: SimpleLogger): Option[TreeNode] = {
    apply(index, line, "", log) match {
      case Some((tree, "")) =>
        tree match {
          case Node(_, _) => ()
          case _ =>
            log.warn("Line %d: Top-level element is not a tree:<<%s>>\n".format(index, line))
        }
        Some(tree)
      case Some((tree, leftover)) =>
        if (leftover.matches("\\s+")) {
          log.warn("Line %d: Please delete the extra whitespace found at the end of this line.\n".format(index))
          Some(tree)
        } else {
          log.err("Line %d: Malformed tree:<<%s>>\tleftover text:<<%s>>\n".format(index, line, leftover))
          None
        }
      case None =>
        None
    }
  }

  def apply(index: Int, line: String): Option[TreeNode] = {
    apply(index, line, noLog)
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
        }).getLines().zipWithIndex

      for ((line, index) <- input_file) {
        println(Parser(index, line, log))
      }

      log.summary("Warnings,Errors: %s\n".format(log.getStats()))
    }
    catch {
      case e: ArgotUsageException =>
        println(e.message)
    }
  }
}
