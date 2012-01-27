package opennlp.scalabha.test

import opennlp.scalabha.model._
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import opennlp.scalabha.tree.MultiLineTreeParser
import opennlp.scalabha.log.SimpleLogger
import java.io.{BufferedWriter, StringWriter}

class ParserTest extends FlatSpec with ShouldMatchers {

  "parseLine" should "succeed" in {
    val logString = new StringWriter()
    val log = new SimpleLogger("", SimpleLogger.WARN, new BufferedWriter(logString))
    assert(MultiLineTreeParser.parseLine(log, "", 0, "")("(a b)") 
      === Some((Node("a", List(Value("b"))), "")))
    assert(log.getStats() === (0, 0))
  }

  "parseLine" should "succeed and complain" in {
    val logString = new StringWriter()
    val log = new SimpleLogger("", SimpleLogger.WARN, new BufferedWriter(logString))
    assert(MultiLineTreeParser.parseLine(log, "", 0, "")("(a b c)")
      === Some((Node("a", List(Value("b"), Value("c"))), "")))
    assert(log.getStats() === (0, 2))
    assert(logString.toString
      === ": [ERR] (file:,tree#:0): A leaf node may only contain a tag and a token." +
      " I.e., (TAG token). Following tree node fails this test: (a b c)\n" +
      ": [ERR] (file:,tree#:0): A node must have exactly one head." +
      " Following tree node fails this test: (a b c)\n")
  }

  "parseLine1" should "succeed and complain" in {
    val logString = new StringWriter()
    val log = new SimpleLogger("", SimpleLogger.WARN, new BufferedWriter(logString))
    assert(MultiLineTreeParser.parseLine(log, "", 0, "")("(a)")
      === Some((Node("a", List()), "")))
    assert(log.getStats() === (0, 2))
    println(logString.toString)
    assert(logString.toString
      === ": [ERR] (file:,tree#:0): A leaf node may only contain a tag and a token. " +
      "I.e., (TAG token). Following tree node fails this test: (a )\n" +
      ": [ERR] (file:,tree#:0): A node must have exactly one head. " +
      "Following tree node fails this test: (a )\n")
  }

  "parseLine" should "succeed and return extra string" in {
    val logString = new StringWriter()
    val log = new SimpleLogger("", SimpleLogger.WARN, new BufferedWriter(logString))
    assert(MultiLineTreeParser.parseLine(log, "", 0, "")("(a b))")
      === Some((Node("a", List(Value("b"))), ")")))
    assert(log.getStats() === (0, 0))
    assert(logString.toString
      === "")
  }

  "parseLine" should "handle line breaks" in {
    val logString = new StringWriter()
    val log = new SimpleLogger("", SimpleLogger.WARN, new BufferedWriter(logString))
    assert(MultiLineTreeParser.parseLine(log, "", 0, "")("(a \nb)")
      === Some((Node("a", List(Value("b"))), "")))
    assert(log.getStats() === (0, 0))
    assert(logString.toString
      === "")
  }
}
