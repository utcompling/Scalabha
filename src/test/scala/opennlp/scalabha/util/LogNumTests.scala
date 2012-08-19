package opennlp.scalabha.util

import org.junit.Assert._
import org.junit.Test
import opennlp.scalabha.util.LogNum._
import scala.math._

class LogNumTests {

  @Test
  def test_operators() {
    val a: LogNum = 3.toLogNum
    val b: LogNum = 5.toLogNum
    val c: LogNum = 6.toLogNum
    val d: LogNum = 2.toLogNum
    val e: LogNum = 8.toLogNum

    assertEquals(log(3.0), (a.logValue: Double), 0.0000001)
    assertEquals(3.0, (a.toDouble: Double), 0.0000001)
    assertEquals(3, (a.toInt: Int), 0.0000001)

    assertTrue(a == 3)
    assertTrue(a == 3L)
    assertTrue(a == 3f)
    assertTrue(a == 3.0)

    assertFalse(a == 4)
    assertFalse(a == 4L)
    assertFalse(a == 4f)
    assertFalse(a == 4.0)
    assertFalse(a == "3")

    assertEquals(3.0 ##, a ##)

    assertEqualsLog(b, d + a)
    assertEqualsLog(d, b - a)
    assertEqualsLog(c, d * a)
    assertEqualsLog(d, c / a)
    assertEqualsLog(e, d ** a)

    assertEqualsLog(b, a max b)
    assertEqualsLog(b, b max a)
    assertEqualsLog(a, a min b)
    assertEqualsLog(a, b min a)

    assertTrue(a < b)
    assertFalse(a > b)
    assertTrue(b > a)
    assertFalse(b < a)

    assertEqualsLog(LogNum(10), List(a, b, d).sum)
    assertEqualsNumericLog(b, List(a, b).max)
    assertEqualsNumericLog(b, List(b, a).max)
    assertEqualsNumericLog(a, List(a, b).min)
    assertEqualsNumericLog(a, List(b, a).min)

    assertEqualsLog(b, 2 + a)
    assertEqualsLog(b, 2L + a)
    assertEqualsLog(b, 2f + a)
    assertEqualsLog(b, 2.0 + a)
    assertEqualsLog(d, 5 - a)
    assertEqualsLog(d, 5L - a)
    assertEqualsLog(d, 5f - a)
    assertEqualsLog(d, 5.0 - a)
    assertEqualsLog(c, 2 * a)
    assertEqualsLog(c, 2L * a)
    assertEqualsLog(c, 2f * a)
    assertEqualsLog(c, 2.0 * a)
    assertEqualsLog(d, 6 / a)
    assertEqualsLog(d, 6L / a)
    assertEqualsLog(d, 6f / a)
    assertEqualsLog(d, 6.0 / a)
    assertEqualsLog(e, 2 ** a)
    assertEqualsLog(e, 2L ** a)
    assertEqualsLog(e, 2f ** a)
    assertEqualsLog(e, 2.0 ** a)

    assertEqualsLog(b, a + 2)
    assertEqualsLog(b, a + 2L)
    assertEqualsLog(b, a + 2f)
    assertEqualsLog(b, a + 2.0)
    assertEqualsLog(a, b - 2)
    assertEqualsLog(a, b - 2L)
    assertEqualsLog(a, b - 2f)
    assertEqualsLog(a, b - 2.0)
    assertEqualsLog(c, a * 2)
    assertEqualsLog(c, a * 2L)
    assertEqualsLog(c, a * 2f)
    assertEqualsLog(c, a * 2.0)
    assertEqualsLog(a, c / 2)
    assertEqualsLog(a, c / 2L)
    assertEqualsLog(a, c / 2f)
    assertEqualsLog(a, c / 2.0)
    assertEqualsLog(e, d ** 3)
    assertEqualsLog(e, d ** 3L)
    assertEqualsLog(e, d ** 3f)
    assertEqualsLog(e, d ** 3.0)
  }

  @Test
  def test_numeric() {
    val a: LogNum = 3.toLogNum
    val b: LogNum = 5.toLogNum
    val c: LogNum = 6.toLogNum
    val d: LogNum = 2.toLogNum
    val e: LogNum = 8.toLogNum

    def stuff[N](a: N, b: N, c: N, d: N, e: N)(implicit num: Fractional[N]) = {
      assertEqualsNumericLog(b, num.plus(d, a))
      assertEqualsNumericLog(d, num.minus(b, a))
      assertEqualsNumericLog(c, num.times(d, a))
      assertEqualsNumericLog(d, num.div(c, a))

      assertEqualsNumericLog(b, List(a, b).max)
      assertEqualsNumericLog(b, List(b, a).max)
      assertEqualsNumericLog(a, List(a, b).min)
      assertEqualsNumericLog(a, List(b, a).min)
    }

    stuff(a, b, c, d, e)
  }

  def assertEqualsLog(a: LogNum, b: LogNum) {
    assertEquals(a.toDouble, b.toDouble, 0.0000001)
  }

  def assertEqualsNumericLog[N](a: N, b: N)(implicit num: Numeric[N]) {
    (a, b) match {
      case (a: LogNum, b: LogNum) => assertEqualsLog(a, b)
      case _ => fail("a and b are not LogNum instances: a:[%s], b:[%s]".format(a, b))
    }
  }

}
