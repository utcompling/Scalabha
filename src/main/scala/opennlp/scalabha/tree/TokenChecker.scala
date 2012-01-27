package opennlp.scalabha.tree

import opennlp.scalabha.log.SimpleLogger
import java.io.{OutputStreamWriter, BufferedWriter}
import org.clapper.argot.{ArgotUsageException, ArgotParser, ArgotConverters}
import opennlp.scalabha.model.TreeNode
import collection.mutable.HashMap

object TokenChecker {

  import ArgotConverters._
  val parser = new ArgotParser(this.getClass.getName, preUsage = Some("Version 0.0"))
  val help = parser.flag[Boolean](List("h", "help"), "print help")
  val input = parser.option[String](List("i", "input"), "FILE", "input inputFile in which to check tokens")
  val tokens = parser.option[String](List("tok"), "FILE", "tokens to check")
  val silent = parser.flag[Boolean](List("s"), "Set this flag to silence warnings and errors in the tree parser.")
  val log = new SimpleLogger(this.getClass.getName, SimpleLogger.WARN, new BufferedWriter(new OutputStreamWriter(System.err)))



  def spprintRepr(map: Map[String, Int], join: String): String = {
    val regex = "[^(]+\\((.*)\\)".r
    val regex(string) = map.toList.sorted.toString
    string.replace(", ", join)
  }

  def checkTokensInLine(treeTokens: List[String], tokFileTokens: List[String]): Boolean = {
    if (treeTokens.length != tokFileTokens.length) {
      //log.err("Lists should be the same length: %s %s\n".format(treeTokens, tokFileTokens))
      log.err("Fail: \n\ttree: %s is not the same length as \n\ttok:  %s\n".format(treeTokens, tokFileTokens))
      false
    } else if (treeTokens.length == 0) {
      true
    } else {
      val a :: as = treeTokens
      val b :: bs = tokFileTokens
      if (a != b) {
        if ((a == "-LRB-" && b == "(")||(b == "-LRB-" && a == "(")) {
          checkTokensInLine(as, bs)
        } else if ((a == "-RRB-" && b == ")") || (b == "-RRB-" && a == ")")) {
          checkTokensInLine(as, bs)
        } else {
          //log.err("%s does not match %s\n".format(a, b))
          log.err(("Fail: \"%s\" does not match \"%s\" in:" +
            "\n\ttree:%s\n\t tok:%s\n").format(a, b, treeTokens, tokFileTokens))
          false
        }
      } else {
        checkTokensInLine(as, bs)
      }
    }
  }

  def checkTokens(infile: Iterator[String], tokfile: Iterator[String]): List[String] = {
    for (((inTreeLine, tokLine), index) <- (infile zip tokfile).toList.zipWithIndex) yield {
      val inTree = MultiLineTreeParser("trees",index,inTreeLine)
      inTree match {
        case Some(root) =>
          val inTreeTokens: List[String] = root.getTokens
          val tokTokens = tokLine.replace("<EOS>", "").split("\\s+").toList
          checkTokensInLine(inTreeTokens, tokTokens) match {
            case true => "%d: pass".format(index)
            case false => "%d: fail".format(index)
          }
        case _ => "%d: Fail - Couldn't parse tree. See parser log messages.".format(index)
      }
    }


  }

  def main(args: Array[String]) {


    try {
      parser.parse(args)

      if (help.value.isDefined) {
        parser.usage()
      }
      MultiLineTreeParser.log.logLevel = silent.value match {
        case Some(_) => SimpleLogger.NONE
        case _ => MultiLineTreeParser.log.logLevel
      }

      val input_file = input.value match {
        case Some(filename:String) => scala.io.Source.fromFile(filename, "UTF-8").getLines()
        case _ => parser.usage()
      }
      val token_file = tokens.value match {
        case Some(filename:String) => scala.io.Source.fromFile(filename, "UTF-8").getLines()
        case _ => parser.usage()
      }

      log.trace("comparing tokens from %s to those in the trees in %s\n".format(tokens.value.get, input.value.get))

      println(checkTokens(input_file, token_file).mkString("\n"))

      val (theseWarnings, theseErrors) = log.getStats()
      val (parseWarnings, parseErrors) = MultiLineTreeParser.log.getStats()
      val (warnings, errors) = (theseWarnings + parseWarnings, theseErrors + parseErrors)

      log.summary("Warnings,Errors: %s\n".format((warnings, errors)))

    } catch {
      case e: ArgotUsageException =>
        println(e.message)
    }
  }

}
