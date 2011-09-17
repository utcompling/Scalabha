package org.scalabha.tree

import org.scalabha.log.SimpleLogger
import java.io.{OutputStreamWriter, BufferedWriter}
import org.clapper.argot.{ArgotUsageException, ArgotParser, ArgotConverters}
import org.scalabha.model.TreeNode
import collection.Set
import collection.mutable.HashMap
import grizzled.string
import io.BufferedSource

object TagChecker {

  import ArgotConverters._

  val parser = new ArgotParser("org.scalabha.preproc.Tokenizer", preUsage = Some("Version 0.0"))
  val help = parser.flag[Boolean](List("h", "help"), "print help")
  val input = parser.option[String](List("i", "input"), "FILE", "input inputFile in which to check tags")
  val other = parser.option[String](List("other"), "FILE", "(optional) other inputFile to compare against the input inputFile")
  val log = new SimpleLogger("org.scalabha.tree.TagChecker", SimpleLogger.WARN, new BufferedWriter(new OutputStreamWriter(System.err)))
  val noLog = new SimpleLogger("org.scalabha.tree.TagChecker", SimpleLogger.NONE, new BufferedWriter(new OutputStreamWriter(System.err)))


  abstract class TagCheckResult

  case class Success() extends TagCheckResult {
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

  def spprintRepr(map: Map[String,Int], join: String): String = {
    val regex = "[^(]+\\((.*)\\)".r
    val regex(string) = map.toList.sorted.toString
    string.replace(", ", join)
  }

  /**
   * Return true iff both trees have the same tag counts
   */
  def apply(left: TreeNode, right: TreeNode): TagCheckResult = {
    val leftCounts = left.getTagCounts()
    val rightCounts = right.getTagCounts()
    if (leftCounts == rightCounts)
      Success()
    else
      Fail[HashMap[String, Int]](leftCounts, rightCounts)
  }

  def combineMaps[K, V](map1: Map[K, V], map2: Map[K, V], op: (V, V) => V): Map[K, V] =
    ((for ((k, v) <- map2) yield (if (map1.contains(k)) (k, op(v, map1(k))) else (k, v)))
      ++
      (for ((k, v) <- map1 if !map2.contains(k)) yield (k, v))
      ).toMap

  def apply(left: Iterator[String], right: Iterator[String]): Map[String, Int] = {
    var resultCounts = Map[String, Int]()
    for ((leftLine, rightLine) <- (left zip right)) {
      Parser(leftLine, Parser.log) match {
        case Some(leftTree: TreeNode) =>
          Parser(rightLine, Parser.log) match {
            case Some(rightTree: TreeNode) =>
              if (leftTree.compareStructure(rightTree))
                resultCounts = combineMaps[String, Int](resultCounts.toMap, leftTree.getTagCounts().toMap, (a: Int, b: Int) => (a + b))
              else
                log.err("the structure of <<%s>> does not the match strucure of <<%s>>\n".format(leftLine, rightLine))
            case None => ()
          }
        case None => ()
      }
    }
    for (line <- left) {
      log.err("Leftover line <<%s>> in input inputFile\n".format(line))
    }
    for (line <- right) {
      log.err("Leftover line <<%s>> in other inputFile\n".format(  line))
    }
    resultCounts
  }

  def apply(list: Iterator[String]): HashMap[String, Int] = {
    val tagCounts = HashMap[String, Int]()

    for (line: String <- list) {
      val tree = Parser(line, Parser.log)
      if (tree.isDefined) {
        for ((key, value) <- tree.get.getTagCounts()) {
          if (tagCounts.contains(key))
            tagCounts(key) += value
          else
            tagCounts(key) = value
        }
      }
    }
    tagCounts
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

      other.value match {
        case None =>
          log.summary("Tag stats:\n\t%s\n".format(
            spprintRepr(
              apply(input_file).toMap, "\n\t")
          ))
        case Some(other_file_name) =>
          val other_file = scala.io.Source.fromFile(other.value.get, "UTF-8").getLines()
          log.summary("Tag stats:\n\t%s\n".format(
            spprintRepr(
              apply(input_file, other_file), "\n\t")
          ))
      }


    } catch {
      case e: ArgotUsageException =>
        println(e.message)
    }
  }

}