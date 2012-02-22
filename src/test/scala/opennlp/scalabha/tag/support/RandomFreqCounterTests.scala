package opennlp.scalabha.tag.support

import org.junit.Assert._
import org.junit.Test

import opennlp.scalabha.util.Probability

class RandomFreqDistTests {

  @Test
  def test_RandomFreqDist() {
    val x = new RandomCondFreqCounter[Char, Symbol](10, new SimpleCondFreqCounter())
    x ++= List('a -> 'A', 'b -> 'A', 'a -> 'B', 'b -> 'B', 'c -> 'B')

    val d = x.toFreqDist

    assertEqualsProb(Probability(0.571), d('A')('a))
    assertEqualsProb(Probability(0.429), d('A')('b))
    assertEqualsProb(Probability(0.000), d('A')('c))
    assertEqualsProb(Probability(0.000), d('A')('z))
    assertEqualsProb(Probability(0.450), d('B')('a))
    assertEqualsProb(Probability(0.500), d('B')('b))
    assertEqualsProb(Probability(0.050), d('B')('c))
    assertEqualsProb(Probability(0.000), d('B')('z))
    assertEqualsProb(Probability(0.000), d('Z')('z))

  }

  def assertEqualsProb(a: Probability, b: Probability) {
    assertEquals(a.toDouble, b.toDouble, 0.001)
  }

}
