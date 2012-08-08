package opennlp.scalabha.tag.support

import org.junit.Assert._
import org.junit.Test

import opennlp.scalabha.test.TestUtils._
import opennlp.scalabha.util.LogNum

class FreqDistTests {

  @Test
  def test_FreqDist_empty() {
    val x = FreqDist.empty[Symbol]
    assertEqualsProb(LogNum(0), x('z))
  }

  @Test
  def test_FreqDist_static() {
    val x = FreqDist.static[Symbol](LogNum(0.5))
    assertEqualsProb(LogNum(0.5), x('z))
  }

  @Test
  def test_FreqDist_apply_counts_noDefaults() {
    val x = FreqDist(DefaultedFreqCounts(Map('a -> 1.5, 'b -> 3.5), 0.0, 0.0))
    assertEqualsProb(LogNum(0.3), x('a))
    assertEqualsProb(LogNum(0.7), x('b))
    assertEqualsProb(LogNum(0.0), x('z))
  }

  @Test
  def test_FreqDist_apply_counts_defaults() {
    val x = FreqDist(DefaultedFreqCounts(Map('a -> 3.0, 'b -> 4.0), 1.0, 2.0))
    assertEqualsProb(LogNum(0.375), x('a))
    assertEqualsProb(LogNum(0.500), x('b))
    assertEqualsProb(LogNum(0.250), x('z))
  }

  @Test
  def test_CondFreqDist_empty() {
    val x = CondFreqDist.empty[Symbol, Symbol]
    assertEqualsProb(LogNum.zero, x('y)('z))
  }

  @Test
  def test_CondFreqDist_static() {
    val x = CondFreqDist.static[Symbol, Symbol](LogNum(0.5))
    assertEqualsProb(LogNum(0.5), x('y)('z))
  }

  @Test
  def test_CondFreqDist_apply_counts_noBDefaults() {
    val x = CondFreqDist(DefaultedCondFreqCounts(
      Map(
        'A' -> DefaultedFreqCounts(Map('a -> 5.0, 'b -> 1.0), 0.0, 0.0),
        'B' -> DefaultedFreqCounts(Map('a -> 3.0, 'b -> 6.0), 0.0, 0.0))))

    assertEqualsProb(LogNum(5 / 6.), x('A')('a))
    assertEqualsProb(LogNum(1 / 6.), x('A')('b))
    assertEqualsProb(LogNum(0 / 6.), x('A')('z))
    assertEqualsProb(LogNum(3 / 9.), x('B')('a))
    assertEqualsProb(LogNum(6 / 9.), x('B')('b))
    assertEqualsProb(LogNum(0 / 9.), x('B')('z))
    assertEqualsProb(LogNum(8 / 15.), x('Z')('a))
    assertEqualsProb(LogNum(7 / 15.), x('Z')('b))
    assertEqualsProb(LogNum(0 / 15.), x('Z')('z))
  }

  @Test
  def test_CondFreqDist_apply_counts_bDefaults() {
    val x = CondFreqDist(DefaultedCondFreqCounts(
      Map(
        'A' -> DefaultedFreqCounts(Map('a -> 3.0, 'b -> 4.0), 1.0, 2.0),
        'B' -> DefaultedFreqCounts(Map('a -> 4.4, 'b -> 4.6), 1.5, 3.0))))

    assertEqualsProb(LogNum(3.0 / 8.0), x('A')('a))
    assertEqualsProb(LogNum(4.0 / 8.0), x('A')('b))
    assertEqualsProb(LogNum(2.0 / 8.0), x('A')('z))
    assertEqualsProb(LogNum(4.4 / 10.5), x('B')('a))
    assertEqualsProb(LogNum(4.6 / 10.5), x('B')('b))
    assertEqualsProb(LogNum(3.0 / 10.5), x('B')('z))
    assertEqualsProb(LogNum(7.4 / 18.5), x('Z')('a))
    assertEqualsProb(LogNum(8.6 / 18.5), x('Z')('b))
    assertEqualsProb(LogNum(5.0 / 18.5), x('Z')('z))
  }

}
