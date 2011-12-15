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
    assert(log.getStats() === (0, 1))
    assert(logString.toString
      === ": [ERR] (file:,tree#:0): A leaf node may only contain a tag and a token. " +
      "I.e., (TAG token). Tree node (a b c) fails this test.\n")
  }

  "parseLine1" should "succeed and complain" in {
    val logString = new StringWriter()
    val log = new SimpleLogger("", SimpleLogger.WARN, new BufferedWriter(logString))
    assert(MultiLineTreeParser.parseLine(log, "", 0, "")("(a)")
      === Some((Node("a", List()), "")))
    assert(log.getStats() === (0, 1))
    assert(logString.toString
      === ": [ERR] (file:,tree#:0): A leaf node may only contain a tag and a token. " +
      "I.e., (TAG token). Tree node (a ) fails this test.\n")
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
