package opennlp.scalabha.test

import org.junit.Assert._
import org.junit._
import opennlp.scalabha.util.LogNum

object TestUtils {

  def assertEqualsProb(expected: LogNum, result: LogNum) {
    assertEquals(expected.toDouble, result.toDouble, 0.0000001)
  }

  def assertException(block: => Unit)(catcher: PartialFunction[Throwable, Unit]) {
    try { block; fail("no exception thrown") } catch catcher
  }

  def assertEqualsIterator[A](expected: Iterator[A], result: Iterator[A]) {
    var i = 0
    while (expected.hasNext && result.hasNext) {
      assertEquals("mismatch on element " + i, expected.next, result.next)
      i += 1
    }
    if (expected.hasNext)
      fail("expected still contains: [%s]".format(expected.toSeq.mkString(", ")))
    if (result.hasNext)
      fail("result still contains: [%s]".format(expected.toSeq.mkString(", ")))
  }

}
