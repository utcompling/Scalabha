package opennlp.scalabha.tree

import org.clapper.argot._
import collection.mutable.MutableList
import opennlp.scalabha.model._
import opennlp.scalabha.log.SimpleLogger
import java.io.{OutputStreamWriter, BufferedWriter}
import io.BufferedSource

object MultiLineTreeParser {

  import ArgotConverters._

  val parser = new ArgotParser(this.getClass().getName, preUsage = Some("Version 0.0"))
  val help = parser.flag[Boolean](List("h", "help"), "print help")
  val input = parser.option[String](List("i", "input"), "FILE", "input inputFile to tokenize")
  var log = new SimpleLogger(this.getClass().getName, SimpleLogger.WARN, new BufferedWriter(new OutputStreamWriter(System.err)))

  val openSymRestRegex = """\s*(\(?)\s*([^\s)(]+)\s*(.*)""".r

  def apply(groupName: String, index: Int, line: String, prefix: String): Option[(TreeNode, String)] = {
    log.trace("%sparsing:<<%s>>\n".format(prefix, line))

    line match {
      case openSymRestRegex(open, sym, rest) =>
        if (open != "") {
          // then we are parsing a full node.
          val name = sym
          var children = List[TreeNode]()
          var next: TreeNode = null
          var rest2 = rest
          while (!rest2.matches("\\s*\\).*")) {
            if (rest2 == "") {
              log.err("(file:%s,tree#:%d): Missing closing paren in:<<%s>>\n".format(groupName, index, line))
              return None
            }
            apply(groupName, index, rest2, "|\t%s".format(prefix)) match {
              case Some((a, b)) =>
                next = a
                rest2 = b
                children = children ::: List(next)
              case None => return None
            }
          }
          val cutoff = rest2.indexOf(')')
          // a Node can't be empty
          // a Node that contains a Value child must contain only one child
          if (children.length == 0
            || (children.filter(_.isInstanceOf[Value]).length > 0 && children.length != 1)) {
            log.err("(file:%s,tree#:%d): A leaf node may only contain a tag and a token. I.e., (TAG token). Tree node %s fails this test.\n".format(groupName, index, Node(name, children).getCanonicalString))
          }
          log.trace("%sresult: %s,\"%s\"\n".format(prefix, Node(name, children), rest2.substring(cutoff + 1)))
          return Some((Node(name, children), rest2.substring(cutoff + 1)))
        } else {
          // then we are only looking at a value
          log.trace("%sresult: %s,\"%s\"\n".format(prefix, Value(sym), rest))
          return Some((Value(sym), rest))
        }
      case "\\s*" =>
        log.err("(file:%s,tree#:%d): Got an empty input line\n".format(groupName, index))
        return None
      case x =>
        log.err("(file:%s,tree#:%d): Could not parse input line <<%s>>\n".format(groupName, index, x))
        return None
    }
  }

  def apply(forestName: String, forestString: String): List[TreeNode] = {
    /*TODO; Interesting thought: if we hang onto the raw lines from the original file
    and we also compute the character position of errors,
    then we can report the position in the source file of errors*/

    var restToParse = forestString
    var resultantTrees: List[TreeNode] = Nil
    var numTreesParsed = 0
    while (restToParse != "") {
      val index = numTreesParsed + 1
      log.debug("Parsing (file:%s,tree#:%d)\n".format(forestName, index))
      apply(forestName, index, restToParse, "") match {
        case Some((tree, leftover)) =>
          // Is the tree valid?
          tree match {
            case Node(_, _) =>
              resultantTrees = tree :: resultantTrees
            case Value(name) =>
              // TODO: Do we keep executing on critical errors or let them fly? Maybe we just don't write the ouput file.
              log.err("(file:%s,tree#:%d): Top-level element is not a tree:<<%s>>\n".format(forestName, index, name))
          }
          restToParse = leftover.trim()
          numTreesParsed += 1
          if (numTreesParsed > 1)
            log.warn("got more than one tree in file:%s\n".format(forestName))
        case None => () // There was no valid parse for the line. We already printed any errors, so we're done.
      }
    }
    resultantTrees.reverse // because we've been prepending trees
  }

  def apply(bufferName: String, buffer: BufferedSource): List[TreeNode] = {
    log.trace("Concatenating lines...\n")
    val concatenatedLines = buffer.getLines().mkString(" ").replaceAll("\\s+", " ")
    apply(bufferName, concatenatedLines)
  }

  def apply(filename: String): List[TreeNode] = {
    log.debug("Parsing trees from file: %s\n".format(filename))
    apply(filename, scala.io.Source.fromFile(filename, "UTF-8"))
  }


  def main(args: Array[String]) {
    try {
      parser.parse(args)

      if (help.value.isDefined) {
        parser.usage()
      }

      val inputTrees = input.value match {
        case Some(filename) => apply(filename)
        case None => parser.usage("you must specify an input tree file")
      }

      val (warnings, errors) = log.getStats()
      if (errors == 0)
        println(inputTrees.map(tree => tree.getCanonicalString()).mkString("\n"))
      else
        log.summary("Suspending output since there were errors.\n")

      log.summary("Warnings,Errors: %s\n".format(log.getStats()))
    }
    catch {
      case e: ArgotUsageException =>
        println(e.message)
    }
  }
}
