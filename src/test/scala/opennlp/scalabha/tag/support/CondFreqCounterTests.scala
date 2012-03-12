package opennlp.scalabha.tag.support

import org.junit.Assert._
import org.junit._
import opennlp.scalabha.util.Probability
import opennlp.scalabha.util.Probability._
import opennlp.scalabha.util.CollectionUtils._
import org.apache.log4j.Logger
import org.apache.log4j.Level

class CondFreqCounterTests {

  @Test
  def test_SimpleCondFreqCounter() {
    val x = new SimpleCondFreqCounter[Char, Symbol]()
    x ++= CondFreqCounts(Map('A' -> Map('a -> 1.0, 'b -> 2.0), 'B' -> Map('a -> 4.0)))
    x ++= CondFreqCounts(Map('A' -> Map('b -> 2.0), 'B' -> Map('a -> 1.0, 'b -> 3.0), 'C' -> Map('a -> 2.0)))
    val d = x.toFreqDist

    assertEqualsProb(Probability(0.200), d('A')('a))
    assertEqualsProb(Probability(0.800), d('A')('b))
    assertEqualsProb(Probability(0.000), d('A')('z))
    assertEqualsProb(Probability(0.625), d('B')('a))
    assertEqualsProb(Probability(0.375), d('B')('b))
    assertEqualsProb(Probability(0.000), d('B')('z))
    assertEqualsProb(Probability(1.000), d('C')('a))
    assertEqualsProb(Probability(0.000), d('C')('b))
    assertEqualsProb(Probability(0.000), d('C')('z))
  }

  @Test
  def test_ConstrainingCondFreqCounter() {
    val constr = Map('A' -> Set('a, 'b, 'd), 'B' -> Set('a, 'b), 'D' -> Set('d))
    val totalAddition = 2.0
    val defaultCount = 3.0
    val x = new ConstrainingCondFreqCounter[Char, Symbol](constr,
      new CondFreqCounter[Char, Symbol] {
        val delegate = new SimpleCondFreqCounter[Char, Symbol]
        override def increment(a: Char, b: Symbol, n: Double) { delegate.increment(a, b, n) }
        override def resultCounts() =
          DefaultedCondFreqCounts(delegate.resultCounts.counts.map {
            case ('A', DefaultedFreqCounts(x, _, _)) => ('A', DefaultedFreqCounts(x, 2.0, 3.0))
            case ('B', DefaultedFreqCounts(x, _, _)) => ('B', DefaultedFreqCounts(x, 1.5, 2.5))
            case ('C', DefaultedFreqCounts(x, _, _)) => ('C', DefaultedFreqCounts(x, 2.5, 3.5))
          })
      })
    x ++= CondFreqCounts(Map('A' -> Map('a -> 1.0, 'b -> 2.0, 'c -> 6.0), 'B' -> Map('a -> 4.0)))
    x ++= CondFreqCounts(Map('A' -> Map('b -> 2.0, 'c -> 1.0), 'B' -> Map('a -> 1.0, 'b -> 3.0), 'C' -> Map('a -> 2.0)))
    val d = x.toFreqDist

    //    val resultCounts = x.resultCounts.simpleCounts.toMap
    //    println(resultCounts)

    assertEqualsProb(Probability(0.125), d('A')('a))
    assertEqualsProb(Probability(0.500), d('A')('b))
    assertEqualsProb(Probability(0.000), d('A')('c))
    assertEqualsProb(Probability(0.375), d('A')('d))
    assertEqualsProb(Probability(0.000), d('A')('z))
    assertEqualsProb(Probability(0.625), d('B')('a))
    assertEqualsProb(Probability(0.375), d('B')('b))
    assertEqualsProb(Probability(0.000), d('B')('c))
    assertEqualsProb(Probability(0.000), d('B')('d))
    assertEqualsProb(Probability(0.000), d('B')('z))
    assertEqualsProb(Probability(0.000), d('C')('a))
    assertEqualsProb(Probability(0.000), d('C')('b))
    assertEqualsProb(Probability(0.000), d('C')('c))
    assertEqualsProb(Probability(0.000), d('C')('d))
    assertEqualsProb(Probability(0.000), d('C')('z))
    assertEqualsProb(Probability(0.000), d('D')('a))
    assertEqualsProb(Probability(0.000), d('D')('b))
    assertEqualsProb(Probability(0.000), d('D')('c))
    assertEqualsProb(Probability(0.000), d('D')('d))
    assertEqualsProb(Probability(0.000), d('D')('z))
  }

  @Test
  def test_AddLambdaSmoothingCondFreqCounter() {
    val lambda = 0.1
    val x = new AddLambdaSmoothingCondFreqCounter[Char, Symbol](lambda,
      new SimpleCondFreqCounter[Char, Symbol])
    x ++= CondFreqCounts(Map('A' -> Map('a -> 1.0, 'b -> 2.0, 'c -> 1.0), 'B' -> Map('a -> 3.0)))
    x ++= CondFreqCounts(List('B' -> 'b, 'A' -> 'b, 'B' -> 'a, 'C' -> 'a, 'B' -> 'b, 'C' -> 'c, 'A' -> 'b, 'B' -> 'b, 'C' -> 'a).groupByKey.mapValuesStrict(_.counts.mapValuesStrict(_.toDouble)))

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
     *  b | 4.1 | 3.1 | 0.1 |
     *  c | 1.1 | 0.1 | 1.1 |
     *  L | 0.1 | 0.1 | 0.1 |
     *  ==+=====+=====+=====+
     *    | 6.4 | 7.4 | 3.4 | 
     * 
     * where L is the smoothing parameter (lambda). (B,c) and (C,b) aren't 
     * in the original counts, but are added at this point. Thus, for the
     * first test case below:
     * 
     * p(a|A) = count(a|A) / count(A) = 1.1/6.4
     */

    val d = x.toFreqDist

    assertEqualsProb(Probability(1.1 / 6.4), d('A')('a))
    assertEqualsProb(Probability(4.1 / 6.4), d('A')('b))
    assertEqualsProb(Probability(1.1 / 6.4), d('A')('c))
    assertEqualsProb(Probability(0.1 / 6.4), d('A')('z))
    assertEqualsProb(Probability(4.1 / 7.4), d('B')('a))
    assertEqualsProb(Probability(3.1 / 7.4), d('B')('b))
    assertEqualsProb(Probability(0.1 / 7.4), d('B')('c))
    assertEqualsProb(Probability(0.1 / 7.4), d('B')('z))
    assertEqualsProb(Probability(2.1 / 3.4), d('C')('a))
    assertEqualsProb(Probability(0.1 / 3.4), d('C')('b))
    assertEqualsProb(Probability(1.1 / 3.4), d('C')('c))
    assertEqualsProb(Probability(0.1 / 3.4), d('C')('z))
  }

  @Test
  def test_addLambdaSmoothing_after_constrain() {
    val lambda = 0.1
    val constr = Map('A' -> Set('a, 'b), 'B' -> Set('a, 'b, 'c))

    val x =
      new AddLambdaSmoothingCondFreqCounter[Char, Symbol](
        lambda,
        new ConstrainingCondFreqCounter[Char, Symbol](
          constr,
          new SimpleCondFreqCounter[Char, Symbol]))

    x ++= CondFreqCounts(Map('A' -> Map('a -> 1.0, 'b -> 2.0, 'c -> 1.0), 'B' -> Map('a -> 3.0, 'c -> 1.0)))
    x ++= CondFreqCounts(List('B' -> 'b, 'A' -> 'b, 'B' -> 'a, 'C' -> 'a, 'B' -> 'b, 'C' -> 'c, 'A' -> 'b, 'B' -> 'b, 'C' -> 'a).groupByKey.mapValuesStrict(_.counts.mapValuesStrict(_.toDouble)))
    val d = x.toFreqDist

    /* before applying the constraints and the add-lambda smoothing, the
     * frequency table is:
     * 
     *   |  A  |  B  |  C  |
     * ==+=====+=====+=====+
     * a |  1  |  4  |  2  |
     * b |  4  |  3  |  0  |
     * c |  1  |  1  |  1  |
     * ==+=====+=====+=====+
     *   |  6  |  8  |  3  |
     *
     * the constraints zero out the following cells
     *
     *   |  A  |  B  |  C  |
     * ==+=====+=====+=====+
     * a |  1  |  4  |  0  |
     * b |  4  |  3  |  0  |
     * c |  0  |  1  |  0  |
     * ==+=====+=====+=====+
     *   |  5  |  8  |  0  |
     *
     * applying the add-lambda smoothing adjusts these counts as follows.
     *
     *   |  A  |  B  |  C  |
     * ==+=====+=====+=====+
     * a | 1.1 | 4.1 | 0.1 |
     * b | 4.1 | 3.1 | 0.1 |
     * c | 0.1 | 1.1 | 0.1 |
     * L | 0.1 | 0.1 | 0.1 |
     * ==+=====+=====+=====+
     *   | 5.4 | 8.4 | 0.4 |
     *
     * so the probability in the first test case below is
     * 
     * p(a|A) = 1.1/5.4
     */

    assertEqualsProb(Probability(1.1 / 5.4), d('A')('a))
    assertEqualsProb(Probability(4.1 / 5.4), d('A')('b))
    assertEqualsProb(Probability(0.1 / 5.4), d('A')('c))
    assertEqualsProb(Probability(0.1 / 5.4), d('A')('z))
    assertEqualsProb(Probability(4.1 / 8.4), d('B')('a))
    assertEqualsProb(Probability(3.1 / 8.4), d('B')('b))
    assertEqualsProb(Probability(1.1 / 8.4), d('B')('c))
    assertEqualsProb(Probability(0.1 / 8.4), d('B')('z))
    assertEqualsProb(Probability(0.1 / 0.4), d('C')('a))
    assertEqualsProb(Probability(0.1 / 0.4), d('C')('b))
    assertEqualsProb(Probability(0.1 / 0.4), d('C')('c))
    assertEqualsProb(Probability(0.1 / 0.4), d('C')('z))
  }

  @Test
  def test_constrain_after_addLambdaSmoothing() {
    val lambda = 0.1
    val constr = Map('A' -> Set('a, 'b), 'C' -> Set('a, 'b, 'c))

    val x =
      new ConstrainingCondFreqCounter[Char, Symbol](
        constr,
        new AddLambdaSmoothingCondFreqCounter[Char, Symbol](
          lambda,
          new SimpleCondFreqCounter[Char, Symbol]))

    x ++= CondFreqCounts(Map('A' -> Map('a -> 1.0, 'b -> 2.0, 'c -> 1.0), 'B' -> Map('a -> 3.0, 'c -> 1.0)))
    x ++= CondFreqCounts(List('B' -> 'b, 'A' -> 'b, 'B' -> 'a, 'C' -> 'a, 'B' -> 'b, 'C' -> 'c, 'A' -> 'b, 'B' -> 'b, 'C' -> 'a).groupByKey.mapValuesStrict(_.counts.mapValuesStrict(_.toDouble)))
    val d = x.toFreqDist

    /* before applying the constraints and the add-lambda smoothing, the
     * frequency table is:
     * 
     *   |  A  |  B  |  C  |
     * ==+=====+=====+=====+
     * a |  1  |  4  |  2  |
     * b |  4  |  3  |  0  |
     * c |  1  |  1  |  1  |
     * ==+=====+=====+=====+
     *   |  6  |  8  |  3  |
     * 
     * applying the add-lambda smoothing to these counts before applying 
     * the constraints gives us
     * 
     *   |  A  |  B  |  C  |
     * ==+=====+=====+=====+
     * a | 1.1 | 4.1 | 2.1 |
     * b | 4.1 | 3.1 | 0.1 |
     * c | 1.1 | 1.1 | 1.1 |
     * L | 0.1 | 0.1 | 0.1 |
     * ==+=====+=====+=====+
     *   | 6.4 | 8.4 | 3.3 |
     * 
     * applying the constraints 0's out the following cells:
     * 
     *   |  A  |  B  |  C  |
     * ==+=====+=====+=====+
     * a | 1.1 |  0  | 2.1 |
     * b | 4.1 |  0  | 0.1 |
     * c |  0  |  0  | 1.1 |
     * L |  0  |  0  |  0  |
     * ==+=====+=====+=====+
     *   | 5.2 |  0  | 3.3 |
     *   
     * so the probability in the first test case below is:
     * 
     * p(a|A) = 1.1/5.2
     */

    assertEqualsProb(Probability(1.1 / 5.2), d('A')('a))
    assertEqualsProb(Probability(4.1 / 5.2), d('A')('b))
    assertEqualsProb(Probability(0.), d('A')('c))
    assertEqualsProb(Probability(0.), d('A')('z))
    assertEqualsProb(Probability(0.), d('B')('a))
    assertEqualsProb(Probability(0.), d('B')('b))
    assertEqualsProb(Probability(0.), d('B')('c))
    assertEqualsProb(Probability(0.), d('B')('z))
    assertEqualsProb(Probability(2.1 / 3.3), d('C')('a))
    assertEqualsProb(Probability(0.1 / 3.3), d('C')('b))
    assertEqualsProb(Probability(1.1 / 3.3), d('C')('c))
    assertEqualsProb(Probability(0.), d('C')('z))
  }

  @Test
  def test_RandomFreqDist() {
    val x = new RandomCondFreqCounter[Char, Symbol](10, new SimpleCondFreqCounter())

    x ++= CondFreqCounts(List('a -> 'A', 'b -> 'A', 'a -> 'B', 'b -> 'B', 'c -> 'B').map(_.swap).groupByKey.mapValuesStrict(_.counts.mapValuesStrict(_.toDouble)))

    /* As it happens, the first random values for this test are:
     * 0 8 9 7 5
     *
     * without the random weighting, the frequency distribution is:
     *
     *   |  A  |  B  |
     * ==+=====+=====+
     * a |  1  |  1  |
     * b |  1  |  1  |
     * c |  0  |  1  |
     * ==+=====+=====+
     *   |  2  |  3  |
     *
     * Given the order the frequencies are (re-)calculated, this is randomly
     * adjusted to (the random values are +1 to not introduce 0s):
     *
     *   |     A     |     B     |
     * ==+===========+===========+
     * a | 1 * (5+1) | 1 * (9+1) |
     * b | 1 * (7+1) | 1 * (8+1) |
     * c |     0     | 1 * (0+1) |
     * 
     * or
     * 
     *   |  A  |  B  |
     * ==+=====+=====+
     * a |  6  |  10 |
     * b |  8  |  9  |
     * c |  0  |  1  |
     * ==+=====+=====+
     *   |  14 |  20 |
     * 
     */
    val d = x.toFreqDist

    assertEqualsProb(Probability(6. / 14.), d('A')('a))
    assertEqualsProb(Probability(8. / 14.), d('A')('b))
    assertEqualsProb(Probability(0.), d('A')('c))
    assertEqualsProb(Probability(0.), d('A')('z))
    assertEqualsProb(Probability(10. / 20.), d('B')('a))
    assertEqualsProb(Probability(9. / 20.), d('B')('b))
    assertEqualsProb(Probability(1. / 20.), d('B')('c))
    assertEqualsProb(Probability(0.), d('B')('z))
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

object CondFreqCounterTests {

  @BeforeClass def turnOffLogging() {
    Logger.getRootLogger.setLevel(Level.OFF)
  }

}
