package opennlp.scalabha.tag.support

import org.junit.Assert._
import org.junit.Test

import opennlp.scalabha.util.Probability

class FreqCounterTests {

  @Test
  def test_SimpleFreqCounter() {
    val x = new SimpleFreqCounter[Symbol]()
    x ++= List('a, 'b, 'a)
    x ++= FreqCounts(Map('a -> 3.0, 'b -> 2.0))
    val d = x.toFreqDist
    assertEqualsProb(Probability(0.625), d('a))
    assertEqualsProb(Probability(0.375), d('b))
    assertEqualsProb(Probability(0.000), d('z))
  }

  @Test
  def test_ConstrainingFreqCounter_strict() {
    val constr = Set('a, 'b)
    val totalAddition = 2
    val defaultCount = 3
    val simple = new SimpleFreqCounter[Symbol]
    val x = new ConstrainingFreqCounter[Symbol](constr,
      new FreqCounter[Symbol] {
        val delegate = new SimpleFreqCounter[Symbol]()
        override def increment(b: Symbol, n: Double) { delegate.increment(b, n) }
        override def resultCounts() = DefaultedFreqCounts(delegate.resultCounts.counts, totalAddition, defaultCount)
      })
    x ++= List('c, 'a, 'd, 'd, 'b, 'c, 'a)
    x ++= FreqCounts(Map('a -> 3.0, 'b -> 2.0, 'c -> 4.0, 'd -> 5.0))
    val d = x.toFreqDist
    assertEqualsProb(Probability(0.625), d('a))
    assertEqualsProb(Probability(0.375), d('b))
    assertEqualsProb(Probability(0.000), d('c))
    assertEqualsProb(Probability(0.000), d('d))
    assertEqualsProb(Probability(0.000), d('z))
  }

  @Test
  def test_FreqCounterFactory_get() {
    val constr = Set('a, 'b)

    val f = new FreqCounterFactory[Symbol] {
      def get() =
        new ConstrainingFreqCounter[Symbol](
          constr,
          new SimpleFreqCounter[Symbol])
    }
    val x = f.get
    x ++= List('c, 'a, 'd, 'd, 'b, 'c, 'a)
    x ++= FreqCounts(Map('a -> 3.0, 'b -> 2.0, 'c -> 4.0, 'd -> 5.0))
    val d = x.toFreqDist
    assertEqualsProb(Probability(0.625), d('a))
    assertEqualsProb(Probability(0.375), d('b))
    assertEqualsProb(Probability(0.000), d('c))
    assertEqualsProb(Probability(0.000), d('d))
    assertEqualsProb(Probability(0.000), d('z))
  }

  @Test
  def test_FreqCounterFactory_getInitial_traversable() {
    val constr = Set('a, 'b)

    val f = new FreqCounterFactory[Symbol] {
      def get() =
        new ConstrainingFreqCounter[Symbol](
          constr,
          new SimpleFreqCounter[Symbol])
    }
    val x = f.get(List('c, 'a, 'd, 'd))
    x ++= List('b, 'c, 'a)
    x ++= FreqCounts(Map('a -> 3.0, 'b -> 2.0, 'c -> 4.0, 'd -> 5.0))
    val d = x.toFreqDist
    assertEqualsProb(Probability(0.625), d('a))
    assertEqualsProb(Probability(0.375), d('b))
    assertEqualsProb(Probability(0.000), d('c))
    assertEqualsProb(Probability(0.000), d('d))
    assertEqualsProb(Probability(0.000), d('z))
  }

  @Test
  def test_FreqCounterFactory_getInitial_counts() {
    val constr = Set('a, 'b)

    val f = new FreqCounterFactory[Symbol] {
      def get() =
        new ConstrainingFreqCounter[Symbol](
          constr,
          new SimpleFreqCounter[Symbol])
    }
    val x = f.get(FreqCounts(Map('b -> 2.0, 'c -> 4.0)))
    x ++= List('c, 'a, 'd, 'd, 'b, 'c, 'a)
    x ++= FreqCounts(Map('a -> 3.0, 'd -> 5.0))
    val d = x.toFreqDist
    assertEqualsProb(Probability(0.625), d('a))
    assertEqualsProb(Probability(0.375), d('b))
    assertEqualsProb(Probability(0.000), d('c))
    assertEqualsProb(Probability(0.000), d('d))
    assertEqualsProb(Probability(0.000), d('z))
  }

  def assertEqualsProb(a: Probability, b: Probability) {
    assertEquals(a.toDouble, b.toDouble, 0.0000001)
  }

}
