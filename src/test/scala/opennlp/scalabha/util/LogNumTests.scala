package opennlp.scalabha.tag.support

import org.junit.Assert._
import org.junit.Test
import opennlp.scalabha.util.LogNum
import opennlp.scalabha.util.LogNum._

class LogNumTests {

  @Test
  def test_operators() {
    val a = 3.toLogNum
    val b = 5.toLogNum
    val c = 6.toLogNum
    val d = 2.toLogNum

    assertEqualsLog(b, d + a)
    assertEqualsLog(d, b - a)
    assertEqualsLog(c, d * a)
    assertEqualsLog(d, c / a)

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
  }

  def assertEqualsLog(a: LogNum, b: LogNum) {
    assertEquals(a.toDouble, b.toDouble, 0.0000001)
  }

}
