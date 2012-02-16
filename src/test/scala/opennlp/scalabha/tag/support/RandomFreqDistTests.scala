package opennlp.scalabha.tag.support

import org.junit.Assert._
import org.junit.Test

import opennlp.scalabha.util.Probability

class RandomFreqDistTests {

  @Test
  def test_RandomFreqDist() {
    val x = new RandomFreqDist[Symbol]()
    val a = x('a)
    assertTrue(0.0 < a.toDouble && a.toDouble < 1.0)
    val b = x('b)
    assertTrue(0.0 < b.toDouble && b.toDouble < 1.0)
    val c = x('c)
    assertTrue(0.0 < c.toDouble && c.toDouble < 1.0)
    val a2 = x('a)
    assertTrue(0.0 < a2.toDouble && a2.toDouble < 1.0)
    assertEquals(a, a2)
    val b2 = x('b)
    assertTrue(0.0 < b2.toDouble && b2.toDouble < 1.0)
    assertEquals(b, b2)
  }

  @Test
  def test_RandomCondFreqDist() {
  }

  def assertEqualsProb(a: Probability, b: Probability) {
    assertEquals(a.toDouble, b.toDouble, 0.0000001)
  }

}
