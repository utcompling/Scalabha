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
    assertEqualsProb(Probability(0.000), d('Z')('z))
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
          }, totalAddition, defaultCount)
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
    assertEqualsProb(Probability(1.000), d('D')('d))
    assertEqualsProb(Probability(0.000), d('D')('z))
    assertEqualsProb(Probability(0.000), d('Z')('a))
    assertEqualsProb(Probability(0.000), d('Z')('b))
    assertEqualsProb(Probability(0.000), d('Z')('c))
    assertEqualsProb(Probability(0.000), d('Z')('d))
    assertEqualsProb(Probability(0.000), d('Z')('z))
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
     *  ==+=====+=====+=====+===
     *  a |  1  |  4  |  2  |
     *  b |  4  |  3  |  0  |
     *  c |  1  |  0  |  1  |
     *  ==+=====+=====+=====+===
     *    |  6  |  7  |  3  | 16
     * 
     * The way this is interpreted with smoothing is:
     * 
     *    |  A  |  B  |  C  |
     *  ==+=====+=====+=====+=====
     *  a | 1.1 | 4.1 | 2.1 |
     *  b | 4.1 | 3.1 |  0  |
     *  c | 1.1 |  0  | 1.1 |
     *  L | 0.1 | 0.1 | 0.1 |
     *  ==+=====+=====+=====+=====
     *    | 6.4 | 7.3 | 3.3 | 17.0
     * 
     * where L is the smoothing parameter (lambda). (B,c) and (C,b) aren't 
     * counted since the counter never witnesses these events. Thus, for the
     * first test case below:
     * 
     * p(a|A) = count(a|A) / count(A) = 1.1/6.4
     * 
     * finally, for unseen label z:
     * 
     * p(z|Z) = .1 / 17.0
     * 
     * though I'm (eponvert) not entirely sure how this makes sense
     */
    
    val d = x.toFreqDist

    assertEqualsProb(Probability(1.1/6.4), d('A')('a))
    assertEqualsProb(Probability(4.1/6.4), d('A')('b))
    assertEqualsProb(Probability(1.1/6.4), d('A')('c))
    assertEqualsProb(Probability(0.1/6.4), d('A')('z))
    assertEqualsProb(Probability(4.1/7.3), d('B')('a))
    assertEqualsProb(Probability(3.1/7.3), d('B')('b))
    assertEqualsProb(Probability(0.1/7.3), d('B')('c))
    assertEqualsProb(Probability(0.1/7.3), d('B')('z)); assertTrue(d('B')('z).toDouble > 0)
    assertEqualsProb(Probability(2.1/3.3), d('C')('a))
    assertEqualsProb(Probability(0.1/3.3), d('C')('b))
    assertEqualsProb(Probability(1.1/3.3), d('C')('c))
    assertEqualsProb(Probability(0.1/3.3), d('C')('z)); assertTrue(d('C')('z).toDouble > 0)
    assertEqualsProb(Probability(0.1/17.0), d('Z')('z)); assertTrue(d('Z')('z).toDouble > 0)
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
     * ==+=====+=====+=====+====
     * a |  1  |  4  |  2  |
     * b |  4  |  3  |  0  |
     * c |  1  |  1  |  1  |
     * ==+=====+=====+=====+====
     *   |  6  |  8  |  3  | 17
     *
     * the constraints zero out the following cells
     *
     *   |  A  |  B  |  C  |
     * ==+=====+=====+=====+====
     * a |  1  |  4  |  0  |
     * b |  4  |  3  |  0  |
     * c |  0  |  1  |  0  |
     * ==+=====+=====+=====+====
     *   |  5  |  8  |  0  | 13
     *
     * applying the add-lambda smoothing adjusts these counts as follows.
     * the 0 cells in the previous table are not smoothed since they are 
     * not witnessed by the counter
     *
     *   |  A  |  B  |  C  |
     * ==+=====+=====+=====+====
     * a | 1.1 | 4.1 |  0  |
     * b | 4.1 | 3.1 |  0  |
     * c |  0  | 1.1 |  0  |
     * L | 0.1 | 0.1 |  0  |
     * ==+=====+=====+=====+====
     *   | 5.3 | 8.4 |  0  | 13.7
     *
     * so the probability in the first test case below is
     * 
     * p(a|A) = 1.1/5.3
     * 
     * for unseen antecedants (which now includes C, since its outputs have
     * been fogotten and not counted by the final counter), we have
     * 
     * p(z|C) = 0.1/13.7
     * 
     * thought I'm (eponvert) not entirely sure why
     */

    assertEqualsProb(Probability(1.1/5.3), d('A')('a))
    assertEqualsProb(Probability(4.1/5.3), d('A')('b))
    assertEqualsProb(Probability(0.1/5.3), d('A')('c))
    assertEqualsProb(Probability(0.1/5.3), d('A')('z))
    assertEqualsProb(Probability(4.1/8.4), d('B')('a))
    assertEqualsProb(Probability(3.1/8.4), d('B')('b))
    assertEqualsProb(Probability(1.1/8.4), d('B')('c))
    assertEqualsProb(Probability(0.1/8.4), d('B')('z))
    assertEqualsProb(Probability(0.1/13.7), d('C')('a))
    assertEqualsProb(Probability(0.1/13.7), d('C')('b))
    assertEqualsProb(Probability(0.1/13.7), d('C')('z))
    assertEqualsProb(Probability(0.1/13.7), d('Z')('z)); assertTrue(d('Z')('z).toDouble > 0)
  }

  @Test
  def test_constrain_after_addLambdaSmoothing() {
    val lambda = 0.1
    val constr = Map('A' -> Set('a, 'b), 'B' -> Set('a, 'b, 'c))

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
     * ==+=====+=====+=====+====
     * a |  1  |  4  |  2  |
     * b |  4  |  3  |  0  |
     * c |  1  |  1  |  1  |
     * ==+=====+=====+=====+====
     *   |  6  |  8  |  3  | 17
     * 
     * applying the add-lambda smoothing to these counts before applying 
     * the constraints gives us ((b,C) isn't smoothed because that event isn't
     * witnessed by the counter)
     * 
     *   |  A  |  B  |  C  |
     * ==+=====+=====+=====+====
     * a | 1.1 | 4.1 | 2.1 |
     * b | 4.1 | 3.1 |  0  |
     * c | 1.1 | 1.1 | 1.1 |
     * L | 0.1 | 0.1 | 0.1 |
     * ==+=====+=====+=====+====
     *   | 6.4 | 8.4 | 3.3 | 17
     * 
     * applying the constraints 0's out the following cells:
     * 
     *   |  A  |  B  |  C  |
     * ==+=====+=====+=====+====
     * a | 1.1 | 4.1 |  0  |
     * b | 4.1 | 3.1 |  0  |
     * c |  0  | 1.1 |  0  |
     * L |  0  |  0  |  0  |
     * ==+=====+=====+=====+====
     *   | 5.2 | 8.3 |  0  | 13.5
     *   
     * so the probability in the first test case below is:
     * 
     * p(a|A) = 1.1/5.2
     */

    assertEqualsProb(Probability(1.1/5.2), d('A')('a))
    assertEqualsProb(Probability(4.1/5.2), d('A')('b))
    assertEqualsProb(Probability(0.), d('A')('c))
    assertEqualsProb(Probability(0.), d('A')('z))
    assertEqualsProb(Probability(4.1/8.3), d('B')('a))
    assertEqualsProb(Probability(3.1/8.3), d('B')('b))
    assertEqualsProb(Probability(1.1/8.3), d('B')('c))
    assertEqualsProb(Probability(0.), d('B')('z))
    assertEqualsProb(Probability(0.), d('C')('a))
    assertEqualsProb(Probability(0.), d('C')('b))
    assertEqualsProb(Probability(0.), d('C')('c))
    assertEqualsProb(Probability(0.), d('C')('z))
    assertEqualsProb(Probability(0.), d('Z')('z))
  }

  @Test
  def test_constrain_after_addLambdaSmoothing_after_constrain() {
    val lambda = 0.1
    val constr = Map('A' -> Set('a, 'b), 'B' -> Set('a, 'b, 'c))

    val x =
      new ConstrainingCondFreqCounter[Char, Symbol](
        constr,
        new AddLambdaSmoothingCondFreqCounter[Char, Symbol](
          lambda,
          new ConstrainingCondFreqCounter[Char, Symbol](
            constr,
            new SimpleCondFreqCounter[Char, Symbol])))

    x ++= CondFreqCounts(Map('A' -> Map('a -> 1.0, 'b -> 2.0, 'c -> 1.0), 'B' -> Map('a -> 3.0, 'c -> 1.0)))
    x ++= CondFreqCounts(List('B' -> 'b, 'A' -> 'b, 'B' -> 'a, 'C' -> 'a, 'B' -> 'b, 'C' -> 'c, 'A' -> 'b, 'B' -> 'b, 'C' -> 'a).groupByKey.mapValuesStrict(_.counts.mapValuesStrict(_.toDouble)))

    /* before applying the constraints and the add-lambda smoothing, the
     * frequency table is:
     * 
     *   |  A  |  B  |  C  |
     * ==+=====+=====+=====+====
     * a |  1  |  4  |  2  |
     * b |  4  |  3  |  0  |
     * c |  1  |  1  |  1  |
     * ==+=====+=====+=====+====
     *   |  6  |  8  |  3  | 17
     * 
     * zeroing out the entries not in the constraint set:
     * 
     *   |  A  |  B  |  C  |
     * ==+=====+=====+=====+====
     * a |  1  |  4  |  0  |
     * b |  4  |  3  |  0  |
     * c |  0  |  1  |  0  |
     * ==+=====+=====+=====+====
     *   |  5  |  8  |  0  | 13.
     * 
     * then adding the smoothing factor -- the second application of the
     * constraints ensures that the c values and the L (lambda or oov) values
     * are actual 0s, not the smoothing value 0.1
     * 
     *   |  A  |  B  |  C  |
     * ==+=====+=====+=====+====
     * a | 1.1 | 4.1 |  0  |
     * b | 4.1 | 3.1 |  0  |
     * c |  0  | 1.1 |  0  |
     * L |  0  |  0  |  0  |
     * ==+=====+=====+=====+====
     *   | 5.2 | 8.3 |  0  | 13.5 
     * 
     * the second application of the constraints should have no effect, except
     * to zero-out the . so, 
     * for instance, the first test case below should be:
     * 
     * p(a|A) = 1.1 / 5.2
     */
    val d = x.toFreqDist

    assertEqualsProb(Probability(1.1/5.2), d('A')('a))
    assertEqualsProb(Probability(4.1/5.2), d('A')('b))
    assertEqualsProb(Probability(0.), d('A')('c))
    assertEqualsProb(Probability(0.), d('A')('z))
    assertEqualsProb(Probability(4.1/8.3), d('B')('a))
    assertEqualsProb(Probability(3.1/8.3), d('B')('b))
    assertEqualsProb(Probability(1.1/8.3), d('B')('c))
    assertEqualsProb(Probability(0.), d('B')('z))
    assertEqualsProb(Probability(0.), d('C')('a))
    assertEqualsProb(Probability(0.), d('C')('b))
    assertEqualsProb(Probability(0.), d('C')('c))
    assertEqualsProb(Probability(0.), d('C')('z))
    assertEqualsProb(Probability(0.), d('Z')('z))
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
     * ==+=====+=====+===
     * a |  1  |  1  |
     * b |  1  |  1  |
     * c |  0  |  1  |
     * ==+=====+=====+===
     *   |  2  |  3  | 5
     *
     * Given the order the frequencies are (re-)calculated, this is randomly
     * adjusted to (the random values are +1 to not introduce 0s):
     *
     *   |     A     |     B     |
     * ==+===========+===========+====
     * a | 1 * (5+1) | 1 * (9+1) |
     * b | 1 * (7+1) | 1 * (8+1) |
     * c |     0     | 1 * (0+1) |
     * 
     * or
     * 
     *   |  A  |  B  |
     * ==+=====+=====+===
     * a |  6  |  10 |
     * b |  8  |  9  |
     * c |  0  |  1  |
     * ==+=====+=====+===
     *   |  14 |  20 | 34
     * 
     */
    val d = x.toFreqDist

    assertEqualsProb(Probability(6./14.), d('A')('a))
    assertEqualsProb(Probability(8./14.), d('A')('b))
    assertEqualsProb(Probability(0.), d('A')('c))
    assertEqualsProb(Probability(0.), d('A')('z))
    assertEqualsProb(Probability(10./20.), d('B')('a))
    assertEqualsProb(Probability(9./20.), d('B')('b))
    assertEqualsProb(Probability(1./20.), d('B')('c))
    assertEqualsProb(Probability(0.), d('B')('z))
    assertEqualsProb(Probability(0.), d('Z')('z))
  }

  def printDist(d: Char => Symbol => Probability) {
    val sb = new StringBuilder
    for (c <- "ABCZ"; s <- List('a, 'b, 'c, 'z)) {
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
