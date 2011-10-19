package opennlp.scalabha.tree

import org.clapper.argot._
import opennlp.scalabha.model._
import opennlp.scalabha.log.SimpleLogger
import java.io.{OutputStreamWriter, BufferedWriter}

object Comparer {

  import ArgotConverters._

  val parser = new ArgotParser("opennlp.scalabha.preproc.Tokenizer", preUsage = Some("Version 0.0"))
  val help = parser.flag[Boolean](List("h", "help"), "print help")
  val left = parser.option[String](List("l", "left"), "FILE", "left tree inputFile for comparison")
  val right = parser.option[String](List("r", "right"), "FILE", "right tree inputFile for comparison")
  val leftLog = new SimpleLogger("opennlp.scalabha.tree.Comparer(LEFT FILE)", SimpleLogger.WARN, new BufferedWriter(new OutputStreamWriter(System.err)))
  val rightLog = new SimpleLogger("opennlp.scalabha.tree.Comparer(RIGHT FILE)", SimpleLogger.WARN, new BufferedWriter(new OutputStreamWriter(System.err)))
  val log = new SimpleLogger("opennlp.scalabha.tree.Comparer", SimpleLogger.WARN, new BufferedWriter(new OutputStreamWriter(System.err)))

  abstract class ComparisonResult

  case class Success() extends ComparisonResult{
    override def toString(): String = {
      "OK"
    }
    val bool = true
  }

  case class Fail[T](left: T, right: T) extends ComparisonResult {
    override def toString(): String = {
      "FAIL: <<%s>>!=<<%s>>".format(left, right)
    }
    val bool = false
  }

  def apply(left: TreeNode, right: TreeNode): ComparisonResult = {
    if (left.compareStructure(right))
      Success()
    else
      Fail[TreeNode](left, right)
  }

  def apply(left: String, right: String): ComparisonResult = {
    val (leftOption, rightOption) = (Parser(left, leftLog), Parser(right, rightLog))
    if (
      if (leftOption.isDefined && rightOption.isDefined) {
        apply(leftOption.get, rightOption.get) match {
          case Success() =>
            true
          case _ =>
            false
        }
      } else if (leftOption.isDefined) {
        log.err("Could not parse RIGHT line:<<%s>>".format(right))
        false
      } else if (rightOption.isDefined) {
        log.err("Could not parse LEFT line:<<%s>>".format(right))
        false
      } else {
        log.err("Unable to parse lines: left:<<%s>> right:<<%s>>\n".format(left, right))
        false
      })
      Success()
    else
      Fail[String](left, right)
  }

  def burnRemainder(toRead: Iterator[String], log: SimpleLogger) {
    for (line <- toRead) {
      log.err("found an extra line:<<%s>>\n".format(line))
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
      burnRemainder(left_file, leftLog)
      burnRemainder(right_file, rightLog)

      log.summary("left parse (Warnings,Errors): %s\n".format(leftLog.getStats()))
      log.summary("right parse (Warnings,Errors): %s\n".format(rightLog.getStats()))
      log.summary("Total comparison (Warnings,Errors): %s\n".format(log.getStats()))
    }
    catch {
      case e: ArgotUsageException =>
        println(e.message)
    }
  }

}
