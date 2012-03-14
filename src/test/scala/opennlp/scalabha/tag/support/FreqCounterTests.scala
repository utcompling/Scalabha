package opennlp.scalabha.tag.support

import org.junit.Assert._
import org.junit.Test

import opennlp.scalabha.util.Probability

class CountsTransformerTests {

  @Test
  def test_PassthroughCountsTransformer_Map_int() {
    val transformer = new PassthroughCountsTransformer[Symbol]

    val counts = Map('a -> 5, 'b -> 3)

    val r @ DefaultedFreqCounts(rC, rT, rD) = transformer(counts)
    assertEqualsDouble(5., rC('a))
    assertEqualsDouble(3., rC('b))
    assertEqualsDouble(0., rT)
    assertEqualsDouble(0., rD)

    val d = FreqDist(r)
    assertEqualsProb(Probability(5. / 8.), d('a))
    assertEqualsProb(Probability(3. / 8.), d('b))
    assertEqualsProb(Probability.zero, d('def))
  }

  @Test
  def test_PassthroughCountsTransformer_DefaultCounts_double() {
    val transformer = new PassthroughCountsTransformer[Symbol]

    val counts = DefaultedFreqCounts(Map('a -> 5., 'b -> 3.), 2., 1.)

    val r @ DefaultedFreqCounts(rC, rT, rD) = transformer(counts)
    assertEqualsDouble(5., rC('a))
    assertEqualsDouble(3., rC('b))
    assertEqualsDouble(2., rT)
    assertEqualsDouble(1., rD)

    val d = FreqDist(r)
    assertEqualsProb(Probability(5. / 10.), d('a))
    assertEqualsProb(Probability(3. / 10.), d('b))
    assertEqualsProb(Probability(1. / 10.), d('def))
  }

  @Test
  def test_ConstrainingCountsTransformer_Map_int() {
    val transformer = new ConstrainingCountsTransformer[Symbol](validEntries = Set('a, 'b, 'd),
      delegate = MockCountsTransformer(
        DefaultedFreqCounts(Map('a -> 7., 'b -> 8., 'c -> 9.), 0., 0.),
        DefaultedFreqCounts(Map('a -> 5., 'b -> 3., 'c -> 6.), 2., 1.)))

    /*
     * Original Counts:
     * 
     * |  a  |  b  |  c  |  d  | def |
     * +=====+=====+=====+=====+=====+=================
     * | 5.0 | 3.0 | 6.0 |  -  | 1.0 | 14.0 + 2 = 16.0
     *
     * After constraining:
     * 
     * |  a  |  b  |  c  |  d  | def |
     * +=====+=====+=====+=====+=====+=================
     * | 5.0 | 3.0 | 0.0 | 1.0 | 0.0 | 9.0 + 0 = 9.0
     * 
     */

    val counts = Map('a -> 7, 'b -> 8, 'c -> 9)

    val r @ DefaultedFreqCounts(rC, rT, rD) = transformer(counts)
    assertEqualsDouble(5., rC('a))
    assertEqualsDouble(3., rC('b))
    assertEqualsDouble(0., rC('c))
    assertEqualsDouble(1., rC('d))
    assertEqualsDouble(0., rT)
    assertEqualsDouble(0., rD)

    val d = FreqDist(r)
    assertEqualsProb(Probability(5. / 9.), d('a))
    assertEqualsProb(Probability(3. / 9.), d('b))
    assertEqualsProb(Probability.zero, d('c))
    assertEqualsProb(Probability(1. / 9.), d('d))
    assertEqualsProb(Probability.zero, d('def))
  }

  @Test
  def test_ConstrainingCountsTransformer_DefaultCounts_double() {
    val transformer = new ConstrainingCountsTransformer[Symbol](validEntries = Set('a, 'b, 'd),
      delegate = MockCountsTransformer(
        DefaultedFreqCounts(Map('a -> 7., 'b -> 8., 'c -> 9.), 10., 11.),
        DefaultedFreqCounts(Map('a -> 5., 'b -> 3., 'c -> 6.), 2., 1.)))

    /*
     * Original Counts:
     * 
     * |  a  |  b  |  c  |  d  | def |
     * +=====+=====+=====+=====+=====+=================
     * | 5.0 | 3.0 | 6.0 |  -  | 1.0 | 14.0 + 2 = 16.0
     *
     * After constraining:
     * 
     * |  a  |  b  |  c  |  d  | def |
     * +=====+=====+=====+=====+=====+=================
     * | 5.0 | 3.0 | 0.0 | 1.0 | 0.0 | 9.0 + 0 = 9.0
     * 
     */

    val counts = DefaultedFreqCounts(Map('a -> 7., 'b -> 8., 'c -> 9.), 10., 11.)

    val r @ DefaultedFreqCounts(rC, rT, rD) = transformer(counts)
    assertEqualsDouble(5., rC('a))
    assertEqualsDouble(3., rC('b))
    assertEqualsDouble(0., rC('c))
    assertEqualsDouble(1., rC('d))
    assertEqualsDouble(0., rT)
    assertEqualsDouble(0., rD)

    val d = FreqDist(r)
    assertEqualsProb(Probability(5. / 9.), d('a))
    assertEqualsProb(Probability(3. / 9.), d('b))
    assertEqualsProb(Probability.zero, d('c))
    assertEqualsProb(Probability(1. / 9.), d('d))
    assertEqualsProb(Probability.zero, d('def))
  }

  @Test
  def test_AddLambdaSmoothingCountsTransformer_DefaultCounts_double() {
    val transformer =
      new AddLambdaSmoothingCountsTransformer(lambda = 0.1,
        delegate = MockCountsTransformer(
          DefaultedFreqCounts(Map('a -> 7., 'b -> 8., 'c -> 9.), 10., 11.),
          DefaultedFreqCounts(Map('a -> 5., 'b -> 3., 'c -> 6.), 2., 1.)))

    /*
     * Original Counts:
     * 
     * |  a  |  b  |  c  |  d  | def |
     * +=====+=====+=====+=====+=====+=================
     * | 5.0 | 3.0 | 6.0 |  -  | 1.0 | 14.0 + 2 = 16.0
     *
     * After smoothing:
     * 
     * |  a  |  b  |  c  |  d  | def |
     * +=====+=====+=====+=====+=====+=================
     * | 5.1 | 3.1 | 6.1 |  -  | 1.1 | 14.3 + 2.1 = 16.4
     */

    val counts = DefaultedFreqCounts(Map('a -> 7., 'b -> 8., 'c -> 9.), 10., 11.)

    val r @ DefaultedFreqCounts(rC, rT, rD) = transformer(counts)
    assertEqualsDouble(5.1, rC('a))
    assertEqualsDouble(3.1, rC('b))
    assertEqualsDouble(6.1, rC('c))
    assertEqualsDouble(2.1, rT)
    assertEqualsDouble(1.1, rD)

    val d = FreqDist(r)
    assertEqualsProb(Probability(5.1 / 16.4), d('a))
    assertEqualsProb(Probability(3.1 / 16.4), d('b))
    assertEqualsProb(Probability(6.1 / 16.4), d('c))
    assertEqualsProb(Probability(1.1 / 16.4), d('d))
    assertEqualsProb(Probability(1.1 / 16.4), d('def))
  }

  @Test
  def test_ConstrainingCountsTransformer_before_AddLambdaSmoothingCountsTransformer_DefaultCounts_double() {
    val transformer =
      new AddLambdaSmoothingCountsTransformer(lambda = 0.1,
        new ConstrainingCountsTransformer[Symbol](validEntries = Set('a, 'b, 'd),
          delegate = MockCountsTransformer(
            DefaultedFreqCounts(Map('a -> 7., 'b -> 8., 'c -> 9.), 10., 11.),
            DefaultedFreqCounts(Map('a -> 5., 'b -> 3., 'c -> 6.), 2., 1.))))

    /*
     * Original Counts:
     * 
     * |  a  |  b  |  c  |  d  | def |
     * +=====+=====+=====+=====+=====+=================
     * | 5.0 | 3.0 | 6.0 |  -  | 1.0 | 14.0 + 2 = 16.0
     *
     * After constraining:
     * 
     * |  a  |  b  |  c  |  d  | def |
     * +=====+=====+=====+=====+=====+=================
     * | 5.0 | 3.0 | 0.0 | 1.0 | 0.0 | 9.0 + 0 = 9.0
     * 
     * After smoothing:
     * 
     * |  a  |  b  |  c  |  d  | def |
     * +=====+=====+=====+=====+=====+=================
     * | 5.1 | 3.1 | 0.1 | 1.1 | 0.1 | 9.4 + 0.1 = 9.5
     * 
     */

    val counts = DefaultedFreqCounts(Map('a -> 7., 'b -> 8., 'c -> 9.), 10., 11.)

    val r @ DefaultedFreqCounts(rC, rT, rD) = transformer(counts)
    assertEqualsDouble(5.1, rC('a))
    assertEqualsDouble(3.1, rC('b))
    assertEqualsDouble(0.1, rC('c))
    assertEqualsDouble(1.1, rC('d))
    assertEqualsDouble(0.1, rT)
    assertEqualsDouble(0.1, rD)

    val d = FreqDist(r)
    assertEqualsProb(Probability(5.1 / 9.5), d('a))
    assertEqualsProb(Probability(3.1 / 9.5), d('b))
    assertEqualsProb(Probability(0.1 / 9.5), d('c))
    assertEqualsProb(Probability(1.1 / 9.5), d('d))
    assertEqualsProb(Probability(0.1 / 9.5), d('def))
  }

  @Test
  def test_AddLambdaSmoothingCountsTransformer_before_ConstrainingCountsTransformer_DefaultCounts_double() {
    val transformer =
      new ConstrainingCountsTransformer[Symbol](validEntries = Set('a, 'b, 'd),
        new AddLambdaSmoothingCountsTransformer(lambda = 0.1,
          delegate = MockCountsTransformer(
            DefaultedFreqCounts(Map('a -> 7., 'b -> 8., 'c -> 9.), 10., 11.),
            DefaultedFreqCounts(Map('a -> 5., 'b -> 3., 'c -> 6.), 2., 1.))))

    /*
     * Original Counts:
     * 
     * |  a  |  b  |  c  |  d  | def |
     * +=====+=====+=====+=====+=====+=================
     * | 5.0 | 3.0 | 6.0 |  -  | 1.0 | 14.0 + 2 = 16.0
     *
     * After smoothing:
     * 
     * |  a  |  b  |  c  |  d  | def |
     * +=====+=====+=====+=====+=====+=================
     * | 5.1 | 3.1 | 6.1 |  -  | 1.1 | 14.3 + 2.1 = 16.4
     * 
     * After constraining:
     * 
     * |  a  |  b  |  c  |  d  | def |
     * +=====+=====+=====+=====+=====+=================
     * | 5.1 | 3.1 | 0.0 | 1.1 | 0.0 | 9.3 + 0.0 = 9.3
     * 
     */

    val counts = DefaultedFreqCounts(Map('a -> 7., 'b -> 8., 'c -> 9.), 10., 11.)

    val r @ DefaultedFreqCounts(rC, rT, rD) = transformer(counts)
    assertEqualsDouble(5.1, rC('a))
    assertEqualsDouble(3.1, rC('b))
    assertEqualsDouble(0.0, rC('c))
    assertEqualsDouble(1.1, rC('d))
    assertEqualsDouble(0.0, rT)
    assertEqualsDouble(0.0, rD)

    val d = FreqDist(r)
    assertEqualsProb(Probability(5.1 / 9.3), d('a))
    assertEqualsProb(Probability(3.1 / 9.3), d('b))
    assertEqualsProb(Probability(0.0 / 9.3), d('c))
    assertEqualsProb(Probability(1.1 / 9.3), d('d))
    assertEqualsProb(Probability(0.0 / 9.3), d('def))
  }

  case class MockCountsTransformer[B](expected: DefaultedFreqCounts[B, Double], returned: DefaultedFreqCounts[B, Double]) extends CountsTransformer[B] {
    override def apply(counts: DefaultedFreqCounts[B, Double]) = {
      val DefaultedFreqCounts(eC, eT, eD) = expected
      val DefaultedFreqCounts(cC, cT, cD) = counts
      assertEquals(eC, cC)
      assertEqualsDouble(eT, cT)
      assertEqualsDouble(eD, cD)
      returned
    }
  }

  def assertEqualsProb(a: Probability, b: Probability) {
    assertEqualsDouble(a.toDouble, b.toDouble)
  }

  def assertEqualsDouble(a: Double, b: Double) {
    assertEquals(a.toDouble, b.toDouble, 0.0000001)
  }

}
