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

  /**
   * Parse a string representation of a syntax tree into a TreeNode tree.
   * @param groupName This is used for logging errors. It's the name of the group that
   * the current tree belongs to. The common use case is that there is a directory's worth
   * of individual tree files that correspond to lines in a single token file. In that case,
   * the groupName is the directory of trees.
   * @param index Also used for logging. This is the index of the current tree in the group.
   * @param line This must be the string representation of a tree on a single line.
   * @param prefix Again, this is just for logging purposes. Since this function is recursive,
   * prefix helps to print the current processing stage indented appropriately.
   *
   * @return An Option: either None, if there was no valid parse, or Some(tree,leftover), where tree
   * is the result of the parse, and leftover is any text to the right of tree in the line parameter.
   */
  def parseLine(log: SimpleLogger, groupName: String, index: Int, prefix: String)(line: String): Option[(TreeNode, String)] = {
    log.trace("%sparsing:<<%s>>\n".format(prefix, line))

    line match {
      case openSymRestRegex(open, sym, rest) =>
        if (open != "") {
          // then we are parsing a full node.
          val name = sym
          var children = List[TreeNode]()
          var next: TreeNode = null
          var childRest = rest
          while (!childRest.matches("\\s*\\).*")) {
            if (childRest == "") {
              log.err("(file:%s,tree#:%d): Missing closing paren in:<<%s>>\n".format(groupName, index, line))
              return None
            }
            parseLine(log, groupName, index, "|\t%s".format(prefix))(childRest) match {
              case Some((a, b)) =>
                next = a
                childRest = b
                children = children ::: List(next)
              case None => return None
            }
          }
          val cutoff = childRest.indexOf(')')
          // a Node can't be empty
          // a Node that contains a Value child must contain only one child
          if (children.length == 0
            || (children.filter(_.isInstanceOf[Value]).length > 0 && children.length != 1)) {
            log.err("(file:%s,tree#:%d): A leaf node may only contain a tag and a token. I.e., (TAG token). Tree node %s fails this test.\n".format(groupName, index, Node(name, children).getCanonicalString))
          }
          log.trace("%sresult: %s,\"%s\"\n".format(prefix, Node(name, children), childRest.substring(cutoff + 1)))
          return Some((Node(name, children), childRest.substring(cutoff + 1)))
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
  
  def apply(groupName:String, index:Int, line:String): Option[TreeNode] = {
    parseLine(log,groupName,index,"")(line) match {
      case Some((tree,"")) => Some(tree)
      case _ => None
    }
  }

  def apply(forestName: String, forestString: String): List[TreeNode] = {
    /*TODO; Interesting thought: if we hang onto the raw lines from the original file
    and we also compute the character position of errors,
    then we can report the position in the source file of errors*/

    var restToParse = forestString
    var lastValueToParse = ""
    var resultantTrees: List[TreeNode] = Nil
    var numTreesParsed = 0
    while (restToParse != "" && restToParse != lastValueToParse) {
      // If we get into a state wherein the parser cannot make progress, we will need to break out of the loop.
      // This is ok to do because we've already logged any errors we have encountered.
      lastValueToParse = restToParse


      val index = numTreesParsed + 1
      log.debug("Parsing (file:%s,tree#:%d)\n".format(forestName, index))
      parseLine(log,forestName, index, "")(restToParse) match {
        case Some((tree, leftover)) =>
          // Is the tree valid?
          tree match {
            case Node(_, _) =>
              resultantTrees = tree :: resultantTrees
            case Value(name) =>
              // TODO: Do we keep executing on critical errors or let them fly? Maybe we just don't write the output file.
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
      System.exit(errors)
    }
    catch {
      case e: ArgotUsageException =>
        println(e.message)
    }
  }
}
