package opennlp.scalabha.test

import org.junit.Assert._
import org.junit._
import opennlp.scalabha.util.LogNum

object TestUtils {

  def assertEqualsProb(expected: LogNum, model: LogNum) {
    assertEquals(expected.toDouble, model.toDouble, 0.0000001)
  }

  def assertException(block: => Unit)(catcher: PartialFunction[Throwable, Unit]) {
    try { block; fail("no exception thrown") } catch catcher
  }

}
