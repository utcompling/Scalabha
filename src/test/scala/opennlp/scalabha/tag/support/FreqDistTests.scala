package opennlp.scalabha.tag.support

import org.junit.Assert.assertEquals
import org.junit.Test

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
        'A' -> DefaultedFreqCounts(Map('a -> 7.0, 'b -> 1.0), 0.0, 0.0),
        'B' -> DefaultedFreqCounts(Map('a -> 3.0, 'b -> 9.0), 0.0, 0.0))))

    assertEqualsProb(LogNum(0.875), x('A')('a))
    assertEqualsProb(LogNum(0.125), x('A')('b))
    assertEqualsProb(LogNum(0.000), x('A')('z))
    assertEqualsProb(LogNum(0.250), x('B')('a))
    assertEqualsProb(LogNum(0.750), x('B')('b))
    assertEqualsProb(LogNum(0.000), x('B')('z))
    assertEqualsProb(LogNum.zero, x('Z')('a))
    assertEqualsProb(LogNum.zero, x('Z')('b))
    assertEqualsProb(LogNum.zero, x('Z')('z))
  }

  @Test
  def test_CondFreqDist_apply_counts_bDefaults() {
    val x = CondFreqDist(DefaultedCondFreqCounts(
      Map(
        'A' -> DefaultedFreqCounts(Map('a -> 3.0, 'b -> 4.0), 1.0, 1.6),
        'B' -> DefaultedFreqCounts(Map('a -> 4.4, 'b -> 9.6), 2.0, 6.0))))

    assertEqualsProb(LogNum(0.375), x('A')('a))
    assertEqualsProb(LogNum(0.500), x('A')('b))
    assertEqualsProb(LogNum(0.200), x('A')('z))
    assertEqualsProb(LogNum(0.275), x('B')('a))
    assertEqualsProb(LogNum(0.600), x('B')('b))
    assertEqualsProb(LogNum(0.375), x('B')('z))
    assertEqualsProb(LogNum.zero, x('Z')('a))
    assertEqualsProb(LogNum.zero, x('Z')('b))
    assertEqualsProb(LogNum.zero, x('Z')('z))
  }

  @Test
  def test_CondFreqDist_apply_counts_noBDefaults_unseenContextProb() {
    val x = CondFreqDist(DefaultedCondFreqCounts(
      Map(
        'A' -> DefaultedFreqCounts(Map('a -> 7.0, 'b -> 1.0), 0.0, 0.0),
        'B' -> DefaultedFreqCounts(Map('a -> 3.0, 'b -> 9.0), 0.0, 0.0)),
      LogNum(0.2)))

    assertEqualsProb(LogNum(0.875), x('A')('a))
    assertEqualsProb(LogNum(0.125), x('A')('b))
    assertEqualsProb(LogNum(0.000), x('A')('z))
    assertEqualsProb(LogNum(0.250), x('B')('a))
    assertEqualsProb(LogNum(0.750), x('B')('b))
    assertEqualsProb(LogNum(0.000), x('B')('z))
    assertEqualsProb(LogNum(0.2), x('Z')('a))
    assertEqualsProb(LogNum(0.2), x('Z')('b))
    assertEqualsProb(LogNum(0.2), x('Z')('z))
  }

  @Test
  def test_CondFreqDist_apply_counts_bDefaults_unseenContextProb() {
    val x = CondFreqDist(DefaultedCondFreqCounts(
      Map(
        'A' -> DefaultedFreqCounts(Map('a -> 3.0, 'b -> 4.0), 1.0, 1.6),
        'B' -> DefaultedFreqCounts(Map('a -> 4.4, 'b -> 9.6), 2.0, 6.0)),
      LogNum(0.2)))

    assertEqualsProb(LogNum(0.375), x('A')('a))
    assertEqualsProb(LogNum(0.500), x('A')('b))
    assertEqualsProb(LogNum(0.200), x('A')('z))
    assertEqualsProb(LogNum(0.275), x('B')('a))
    assertEqualsProb(LogNum(0.600), x('B')('b))
    assertEqualsProb(LogNum(0.375), x('B')('z))
    assertEqualsProb(LogNum(0.2), x('Z')('a))
    assertEqualsProb(LogNum(0.2), x('Z')('b))
    assertEqualsProb(LogNum(0.2), x('Z')('z))
  }

  def assertEqualsProb(a: LogNum, b: LogNum) {
    assertEquals(a.toDouble, b.toDouble, 0.0000001)
  }

}
