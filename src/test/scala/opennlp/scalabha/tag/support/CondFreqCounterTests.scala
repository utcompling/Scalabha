package opennlp.scalabha.tag.support

import org.junit.Assert._
import org.junit._
import opennlp.scalabha.util.Probability
import opennlp.scalabha.util.Probability._
import opennlp.scalabha.util.Pattern.{ -> }
import opennlp.scalabha.util.CollectionUtils._
import org.apache.log4j.Logger
import org.apache.log4j.Level

class CondCountsTransformerTests {

  @Test
  def test_PassthroughCondCountsTransformer_DefaultCounts_double() {
    val transformer = new PassthroughCondCountsTransformer[Char, Symbol]()

    val counts = Map(
      'A' -> Map('a -> 1., 'b -> 4.),
      'B' -> Map('a -> 1., 'b -> 3.),
      'C' -> Map('a -> 2.))

    val r = transformer(counts)
    // TODO: assert counts 

    val d = CondFreqDist(r)
    assertEqualsProb(Probability(1. / 5.), d('A')('a))
    assertEqualsProb(Probability(4. / 5.), d('A')('b))
    assertEqualsProb(Probability.zero, d('A')('z))
    assertEqualsProb(Probability(5. / 8.), d('B')('a))
    assertEqualsProb(Probability(3. / 8.), d('B')('b))
    assertEqualsProb(Probability.zero, d('B')('z))
    assertEqualsProb(Probability.one, d('C')('a))
    assertEqualsProb(Probability.zero, d('C')('b))
    assertEqualsProb(Probability.zero, d('C')('z))
  }

  @Test
  def test_ConstrainingCondCountsTransformer_DefaultCounts_double() {
    val transformer = new ConstrainingCondCountsTransformer[Char, Symbol](
      validEntries = Map('A' -> Set('a, 'b, 'd), 'B' -> Set('a, 'b), 'D' -> Set('d)),
      delegate = MockCondCountsTransformer(
        new DefaultedCondFreqCounts(Map(
          'A' -> DefaultedFreqCounts(Map('a -> 27.), 21., 22.),
          'C' -> DefaultedFreqCounts(Map('b -> 29.), 25., 26.))),
        new DefaultedCondFreqCounts(Map(
          'A' -> DefaultedFreqCounts(Map('a -> 1., 'b -> 4., 'c -> 7.), 2., 3.),
          'B' -> DefaultedFreqCounts(Map('a -> 6., 'c -> 6.), 1.5, 2.5),
          'C' -> DefaultedFreqCounts(Map('a -> 2.), 2.5, 3.5)))))

    val counts = new DefaultedCondFreqCounts(Map(
      'A' -> DefaultedFreqCounts(Map('a -> 27.), 21., 22.),
      'C' -> DefaultedFreqCounts(Map('b -> 29.), 25., 26)))

    val r = transformer(counts)
    // TODO: assert counts 

    val d = CondFreqDist(r)
    assertEqualsProb(Probability(0.125), d('A')('a))
    assertEqualsProb(Probability(0.500), d('A')('b))
    assertEqualsProb(Probability.zero, d('A')('c))
    assertEqualsProb(Probability(0.375), d('A')('d))
    assertEqualsProb(Probability.zero, d('A')('z))
    assertEqualsProb(Probability(0.625), d('B')('a))
    assertEqualsProb(Probability(0.375), d('B')('b))
    assertEqualsProb(Probability.zero, d('B')('c))
    assertEqualsProb(Probability.zero, d('B')('d))
    assertEqualsProb(Probability.zero, d('B')('z))
    assertEqualsProb(Probability.zero, d('C')('a))
    assertEqualsProb(Probability.zero, d('C')('b))
    assertEqualsProb(Probability.zero, d('C')('c))
    assertEqualsProb(Probability.zero, d('C')('d))
    assertEqualsProb(Probability.zero, d('C')('z))
    assertEqualsProb(Probability.zero, d('D')('a))
    assertEqualsProb(Probability.zero, d('D')('b))
    assertEqualsProb(Probability.zero, d('D')('c))
    assertEqualsProb(Probability.zero, d('D')('d))
    assertEqualsProb(Probability.zero, d('D')('z))
  }

  @Test
  def test_AddLambdaSmoothingCondCountsTransformer_DefaultCounts_double() {
    val transformer = new AddLambdaSmoothingCondCountsTransformer[Char, Symbol](
      lambda = 0.1,
      delegate = MockCondCountsTransformer(
        new DefaultedCondFreqCounts(Map(
          'A' -> DefaultedFreqCounts(Map('a -> 27.), 21., 22.),
          'C' -> DefaultedFreqCounts(Map('b -> 29.), 25., 26.))),
        new DefaultedCondFreqCounts(Map(
          'A' -> DefaultedFreqCounts(Map('a -> 1., 'b -> 4., 'c -> 1.), .1, .2),
          'B' -> DefaultedFreqCounts(Map('a -> 4., 'b -> 3.), .3, .4),
          'C' -> DefaultedFreqCounts(Map('a -> 2., 'c -> 1.), .5, .6)))))

    /* at this point, the frequency distribution (without smoothing) looks like
     * 
     *    |  A  |  B  |  C  |
     *  ==+=====+=====+=====+
     *  a |  1  |  4  |  2  |
     *  b |  4  |  3  |  0  |
     *  c |  1  |  0  |  1  |
     *  ==+=====+=====+=====+
     *    |  6  |  7  |  3  |
     * 
     * The way this is interpreted with smoothing is:
     * 
     *    |  A  |  B  |  C  |
     *  ==+=====+=====+=====+
     *  a | 1.1 | 4.1 | 2.1 |
     *  b | 4.1 | 3.1 | 0.7 |
     *  c | 1.1 | 0.5 | 1.1 |
     *  Z | 0.3 | 0.5 | 0.7 |
     *  ==+=====+=====+=====+
     *    | 6.6 | 8.2 | 4.6 | 
     * 
     * where L is the smoothing parameter (lambda). (B,c) and (C,b) aren't 
     * in the original counts, but are added at this point. Thus, for the
     * first test case below:
     * 
     * p(a|A) = count(a|A) / count(A) = 1.1/6.4
     */

    val counts = new DefaultedCondFreqCounts(Map(
      'A' -> DefaultedFreqCounts(Map('a -> 27.), 21., 22.),
      'C' -> DefaultedFreqCounts(Map('b -> 29.), 25., 26)))

    val r = transformer(counts)
    // TODO: assert counts 

    val d = CondFreqDist(r)

    assertEqualsProb(Probability(1.1 / 6.6), d('A')('a))
    assertEqualsProb(Probability(4.1 / 6.6), d('A')('b))
    assertEqualsProb(Probability(1.1 / 6.6), d('A')('c))
    assertEqualsProb(Probability(0.3 / 6.6), d('A')('z))
    assertEqualsProb(Probability(4.1 / 8.2), d('B')('a))
    assertEqualsProb(Probability(3.1 / 8.2), d('B')('b))
    assertEqualsProb(Probability(0.5 / 8.2), d('B')('c))
    assertEqualsProb(Probability(0.5 / 8.2), d('B')('z))
    assertEqualsProb(Probability(2.1 / 4.6), d('C')('a))
    assertEqualsProb(Probability(0.7 / 4.6), d('C')('b))
    assertEqualsProb(Probability(1.1 / 4.6), d('C')('c))
    assertEqualsProb(Probability(0.7 / 4.6), d('C')('z))
  }

  @Test
  def test_AddLambdaSmoothingCondCountsTransformer_before_ConstrainingCondCountsTransformer_DefaultCounts_double() {
    val transformer =
      new AddLambdaSmoothingCondCountsTransformer[Char, Symbol](
        lambda = 0.1,
        new ConstrainingCondCountsTransformer[Char, Symbol](
          validEntries = Map('A' -> Set('a, 'b, 'd), 'B' -> Set('a, 'b), 'D' -> Set('d)),
          delegate = MockCondCountsTransformer(
            new DefaultedCondFreqCounts(Map(
              'A' -> DefaultedFreqCounts(Map('a -> 27.), 21., 22.),
              'C' -> DefaultedFreqCounts(Map('b -> 29.), 25., 26.))),
            new DefaultedCondFreqCounts(Map(
              'A' -> DefaultedFreqCounts(Map('a -> 1., 'b -> 4., 'c -> 1.), .1, .2),
              'B' -> DefaultedFreqCounts(Map('a -> 4., 'b -> 3.), .3, .4),
              'C' -> DefaultedFreqCounts(Map('a -> 2., 'c -> 1.), .5, .6))))))

    // TODO: Complete
  }

  @Test
  def test_RandomCondCountsTransformer_DefaultCounts_double() {
    val transformer = new RandomCondCountsTransformer[Char, Symbol](
      maxCount = 10,
      delegate = MockCondCountsTransformer(
        new DefaultedCondFreqCounts(Map(
          'A' -> DefaultedFreqCounts(Map('a -> 27.), 21., 22.),
          'C' -> DefaultedFreqCounts(Map('b -> 29.), 25., 26.))),
        new DefaultedCondFreqCounts(Map(
          'A' -> DefaultedFreqCounts(Map('a -> 1., 'b -> 2.), .1, .2),
          'B' -> DefaultedFreqCounts(Map('a -> 3.), .3, .4)))))

    /* As it happens, the first random values for this test are:
     * 8 9 7 5
     *
     * without the random weighting, the frequency distribution is:
     *
     *   |  A  |  B  |
     * ==+=====+=====+
     * a |  1  |  2  |
     * b |  3  |  0  |
     * Z |  .2 |  .4 |
     * ==+=====+=====+
     *   | 4.2 | 2.4 |
     *
     * Given the order the frequencies are (re-)calculated, this is randomly
     * adjusted to:
     *
     *   |   A   |   B   |
     * ==+=======+=======+
     * a | 1 + 5 | 2 + 9 |
     * b | 3 + 7 | 0 + 8 |
     * 
     * or
     * 
     *   |  A   |  B   |
     * ==+======+======+
     * a |  6   |  11  |
     * b |  10  |  8   |
     * Z |  .2  |  .4  |
     * ==+======+======+
     *   | 16.3 | 19.7 |
     */

    val counts = new DefaultedCondFreqCounts(Map(
      'A' -> DefaultedFreqCounts(Map('a -> 27.), 21., 22.),
      'C' -> DefaultedFreqCounts(Map('b -> 29.), 25., 26)))

    val r = transformer(counts)
    // TODO: assert counts 

    val d = CondFreqDist(r)

    assertEqualsProb(Probability(6. / 16.3), d('A')('a))
    assertEqualsProb(Probability(10. / 16.3), d('A')('b))
    assertEqualsProb(Probability(.2 / 16.3), d('A')('z))
    assertEqualsProb(Probability(11. / 19.7), d('B')('a))
    assertEqualsProb(Probability(8. / 19.7), d('B')('b))
    assertEqualsProb(Probability(.4 / 19.7), d('B')('z))
  }

  case class MockCondCountsTransformer[A, B](expected: DefaultedCondFreqCounts[A, B, Double], returned: DefaultedCondFreqCounts[A, B, Double]) extends CondCountsTransformer[A, B] {
    override def apply(counts: DefaultedCondFreqCounts[A, B, Double]) = {
      for ((eA -> e, cA -> c) <- (expected.counts zipEqual counts.counts)) {
        assertEquals(eA, cA)
        val DefaultedFreqCounts(eC, eT, eD) = e
        val DefaultedFreqCounts(cC, cT, cD) = c
        assertEquals(eC, cC)
        assertEquals(eT, cT)
        assertEquals(eD, cD)
      }
      returned
    }
  }

  def printDist(d: Char => Symbol => Probability) {
    val sb = new StringBuilder
    for (c <- "ABC"; s <- List('a, 'b, 'c, 'z)) {
      val v = d(c)(s).toDouble
      val vs = "%.03f".format(v)
      sb.append("assertEqualsProb(Probability(%s), d('%s')(%s))".format(vs, c, s))
      if (vs == "0.000")
        sb.append("; assertTrue(d('%s')(%s).toDouble > 0)".format(c, s))
      sb.append("\n")
    }
    sb.append("\n")
    print(sb.toString)
  }

  def assertEqualsProb(a: Probability, b: Probability) {
    assertEquals(a.toDouble, b.toDouble, 0.001)
  }

}

object CondCountsTransformerTests {

  @BeforeClass def turnOffLogging() {
    Logger.getRootLogger.setLevel(Level.OFF)
  }

}
