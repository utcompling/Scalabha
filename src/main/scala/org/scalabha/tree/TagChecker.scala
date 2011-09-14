package org.scalabha.tree

import org.scalabha.log.SimpleLogger
import java.io.{OutputStreamWriter, BufferedWriter}
import org.clapper.argot.{ArgotUsageException, ArgotParser, ArgotConverters}
import org.scalabha.model.TreeNode
import collection.Set

object TagChecker {

  import ArgotConverters._

  val parser = new ArgotParser("org.scalabha.lang.Tokenizer", preUsage = Some("Version 0.0"))
  val help = parser.flag[Boolean](List("h", "help"), "print help")
  val input = parser.option[String](List("i", "input"), "FILE", "input file in which to check tags")
  val log = new SimpleLogger("org.scalabha.tree.TagChecker", SimpleLogger.WARN, new BufferedWriter(new OutputStreamWriter(System.err)))
  val noLog = new SimpleLogger("org.scalabha.tree.TagChecker", SimpleLogger.NONE, new BufferedWriter(new OutputStreamWriter(System.err)))


  abstract class TagCheckResult

  case class Success() extends TagCheckResult{
    override def toString(): String = {
      "OK"
    }
    val bool = true
  }

  case class Fail[T](left: T, right: T) extends TagCheckResult {
    override def toString(): String = {
      "FAIL: <<%s>>!=<<%s>>".format(left, right)
    }
    val bool = false
  }

  /**
   * Just get the tagset for the given tree
   */
  def apply(tree: TreeNode): Set[String] = {
    tree.getMap().keySet
  }

  /**
   * Return true iff both trees have the same tagset
   */
  def apply(left: TreeNode, right: TreeNode): TagCheckResult = {
    val leftSet = apply(left)
    val rightSet = apply(right)
    if (leftSet == rightSet)
      Success()
    else
      Fail[Set[String]](leftSet, rightSet)
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
        val tree = Parser(line, Parser.log)
        if (tree.isDefined)
          println(line+"\t"+apply(tree.get))
        else
          println(line+"\tINVAL")
      }

    } catch {
      case e: ArgotUsageException =>
        println(e.message)
    }
  }

}