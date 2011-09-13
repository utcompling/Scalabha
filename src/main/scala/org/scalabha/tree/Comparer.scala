package org.scalabha.tree

import org.clapper.argot._
import org.scalabha.model._
import org.scalabha.log.SimpleLogger
import java.io.{OutputStreamWriter, BufferedWriter}

object Comparer {

  import ArgotConverters._

  val parser = new ArgotParser("org.scalabha.lang.Tokenizer", preUsage = Some("Version 0.0"))
  val help = parser.flag[Boolean](List("h", "help"), "print help")
  val left = parser.option[String](List("l", "left"), "FILE", "left tree file for comparison")
  val right = parser.option[String](List("r", "right"), "FILE", "right tree file for comparison")
  val leftLog = new SimpleLogger("compare_LeftFile", SimpleLogger.WARN, new BufferedWriter(new OutputStreamWriter(System.err)))
  val rightLog = new SimpleLogger("compare_RightFile", SimpleLogger.WARN, new BufferedWriter(new OutputStreamWriter(System.err)))
  val log = new SimpleLogger("Comparer", SimpleLogger.WARN, new BufferedWriter(new OutputStreamWriter(System.err)))

  def apply(left: TreeNode, right: TreeNode): Boolean = {
    left.compareStructure(right)
  }

  def apply(left: String, right: String): Boolean = {
    val (leftOption, rightOption) = (Parser(left, leftLog), Parser(right, rightLog))
    if (leftOption.isDefined && rightOption.isDefined) {
      val result = apply(leftOption.get, rightOption.get)
      if (!result) {
        log.err("Mismatched lines: left:<<%s>> right:<<%s>>\n".format(left, right))
      }
      result
    } else if (leftOption.isDefined) {
      log.err("Could not parse RIGHT line:<<%s>>".format(right))
      false
    } else if (rightOption.isDefined) {
      log.err("Could not parse LEFT line:<<%s>>".format(right))
      false
    } else {
      log.err("Unable to parse lines: left:<<%s>> right:<<%s>>\n".format(left, right))
      false
    }
  }

  def main(args: Array[String]) {
    try {
      parser.parse(args)

      if (help.value.isDefined) {
        parser.usage()
      }

      val left_file =
        (if (left.value.isDefined) {
          scala.io.Source.fromFile(left.value.get, "UTF-8").getLines()
        } else {
          parser.usage()
        })

      val right_file =
        (if (right.value.isDefined) {
          scala.io.Source.fromFile(right.value.get, "UTF-8").getLines()
        } else {
          parser.usage()
        })

      for ((leftLine, rightLine) <- (left_file zip right_file)) {
        println(Comparer(leftLine, rightLine))
      }

      println("left parse (Warnings,Errors): %s".format(leftLog.getStats()))
      println("right parse (Warnings,Errors): %s".format(rightLog.getStats()))
      println("Total comparison (Warnings,Errors): %s".format(log.getStats()))
    }
    catch {
      case e: ArgotUsageException =>
        println(e.message)
    }
  }
}