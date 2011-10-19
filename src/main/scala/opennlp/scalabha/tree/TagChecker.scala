package opennlp.scalabha.tree

import opennlp.scalabha.log.SimpleLogger
import java.io.{OutputStreamWriter, BufferedWriter}
import org.clapper.argot.{ArgotUsageException, ArgotParser, ArgotConverters}
import opennlp.scalabha.model.TreeNode
import collection.Set
import collection.mutable.HashMap
import grizzled.string
import io.BufferedSource

object TagChecker {

  import ArgotConverters._

  val parser = new ArgotParser("opennlp.scalabha.preproc.Tokenizer", preUsage = Some("Version 0.0"))
  val help = parser.flag[Boolean](List("h", "help"), "print help")
  val input = parser.option[String](List("i", "input"), "FILE", "input inputFile in which to check tags")
  val other = parser.option[String](List("other"), "FILE", "(optional) other inputFile to compare against the input inputFile")
  val log = new SimpleLogger("opennlp.scalabha.tree.TagChecker", SimpleLogger.WARN, new BufferedWriter(new OutputStreamWriter(System.err)))
  val noLog = new SimpleLogger("opennlp.scalabha.tree.TagChecker", SimpleLogger.NONE, new BufferedWriter(new OutputStreamWriter(System.err)))


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

  def spprintRepr(map: Map[String, Int], join: String): String = {
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
      log.err("Leftover line <<%s>> in other inputFile\n".format(line))
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

  def checkTokensInLine(aList: List[String], bList: List[String]): String = {
    if (aList.length != bList.length) {
      //log.err("Lists should be the same length: %s %s\n".format(aList, bList))
      "Fail: %s is not the same length as %s".format(aList, bList)
    } else if (aList.length == 0) {
      ""
    } else {
      val a :: as = aList
      val b :: bs = bList
      if (a != b) {
        if ((a == "-LRB-" && b == "(")||(b == "-LRB-" && a == "(")) {
          "" + checkTokensInLine(as, bs)
        } else if ((a == "-RRB-" && b == ")") || (b == "-RRB-" && a == ")")) {
          "" + checkTokensInLine(as, bs)
        } else {
          //log.err("%s does not match %s\n".format(a, b))
          "Fail: \"%s\" does not match \"%s\"".format(a, b)
        }
      } else {
        "" + checkTokensInLine(as, bs)
      }
    }
  }

  def checkTokens(infile: List[String], tokfile: List[String]): List[String] = {
    for (((inTreeLine, tokLine), index) <- (infile zip tokfile).toList.zipWithIndex) yield {
      val inTree = Parser(inTreeLine, Parser.log)
      inTree match {
        case Some(root) =>
          val inTreeTokens: List[String] = root.getTokens
          val tokTokens = tokLine.replace("<EOS>", "").split("\\s+").toList
          checkTokensInLine(inTreeTokens, tokTokens) match {
            case "" => "%d: pass".format(index)
            case x => "%d: %s".format(index, x)
          }
        case _ => "%d: Fail - couldn't parse tree. See parser log messages.".format(index)
      }
    }


  }

  def main(args: Array[String]) {
    val parser = new ArgotParser("opennlp.scalabha.preproc.Tokenizer", preUsage = Some("Version 0.0"))
    val help = parser.flag[Boolean](List("h", "help"), "print help")
    val input = parser.option[String](List("i", "input"), "FILE", "input inputFile in which to check tags")
    val tokens = parser.option[String](List("tok"), "FILE", "tags to check")
    val other = parser.option[String](List("other"), "FILE", "(optional) other inputFile to compare against the input inputFile")


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

      if (tokens.value.isDefined) {
        log.info("comparing tokens from %s to those in the trees in %s\n")
        val inputList = input_file.toList
        println(
          checkTokens(inputList, scala.io.Source.fromFile(tokens.value.get, "UTF-8").getLines().toList).mkString("\n")
        )
        log.summary("Tag stats:\n\t%s\n".format(spprintRepr(apply(inputList.iterator).toMap, "\n\t"))
        )
      } else {

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
      }
      log.summary("Warnings,Errors: %s\n".format(log.getStats()))
      Parser.log.summary("Warnings,Errors: %s\n".format(Parser.log.getStats()))


    } catch {
      case e: ArgotUsageException =>
        println(e.message)
    }
  }

}
