package opennlp.scalabha.tag.hmm.support

import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.util.LogNum
import opennlp.scalabha.tag.support._
import org.apache.commons.logging.LogFactory
import org.junit.BeforeClass
import org.apache.log4j.Logger
import org.apache.log4j.Level
import org.junit.Test
import org.junit.Assert._

class UnsupervisedEmissionDistTests {

  @Test
  def test_EstimatedRawCountUnsupervisedEmissionDistFactory() {

    val rawData = List(
      "the dog walks quickly",
      "the cat walks quietly",
      "the dog saw the cat",
      "the cat saw the dog",
      "the dog saw the saw",
      "the bird sings",
      "the mouse walks",
      "the aardvark walks",
      "the aardvark meanders").map(_.split(" ").toList)

    val tagDict = Map(
      "bird" -> Set('N),
      "cat" -> Set('N),
      "dog" -> Set('N),
      "mouse" -> Set('N),
      "quickly" -> Set('R),
      "quietly" -> Set('R),
      "saw" -> Set('N, 'V),
      "sings" -> Set('V),
      "the" -> Set('D),
      "walks" -> Set('V))

    val d = new EstimatedRawCountUnsupervisedEmissionDistFactory[Symbol, String](tagDict, rawData, startEndSymbol = "<END>", startEndTag = 'END).make()

    println(d('N)("aardvark"))
    println(d('N)("meanders"))
    println(d('V)("aardvark"))
    println(d('V)("meanders"))
    println(d('N)("dog"))
    println(d('V)("walks"))
    
    println
    println(d('N)("aardvark") / d('V)("aardvark"))
    println(d('N)("meanders") / d('V)("meanders"))
    
    assertEqualsProb(LogNum(1. / 5.), d('N)("bird"))
  }

  def assertEqualsProb(a: LogNum, b: LogNum) {
    assertEquals(a.toDouble, b.toDouble, 0.001)
  }

  def assertEqualsDouble(a: Double, b: Double) {
    assertEquals(a.toDouble, b.toDouble, 0.0000001)
  }

}

object UnsupervisedEmissionDistTests {

  @BeforeClass def turnOffLogging() {
    Logger.getRootLogger.setLevel(Level.OFF)
  }

}
