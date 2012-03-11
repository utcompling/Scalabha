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
    val d = x.toFreqDist

    assertEqualsProb(Probability(0.230), d('A')('a))
    assertEqualsProb(Probability(0.605), d('A')('b))
    assertEqualsProb(Probability(0.164), d('A')('c))
    assertEqualsProb(Probability(0.013), d('A')('z))
    assertEqualsProb(Probability(0.569), d('B')('a))
    assertEqualsProb(Probability(0.428), d('B')('b))
    assertEqualsProb(Probability(0.002), d('B')('c))
    assertEqualsProb(Probability(0.000), d('B')('z)); assertTrue(d('B')('z).toDouble > 0)
    assertEqualsProb(Probability(0.603), d('C')('a))
    assertEqualsProb(Probability(0.109), d('C')('b))
    assertEqualsProb(Probability(0.288), d('C')('c))
    assertEqualsProb(Probability(0.014), d('C')('z)); assertTrue(d('C')('z).toDouble > 0)
    assertEqualsProb(Probability(0.000), d('Z')('z)); assertTrue(d('Z')('z).toDouble > 0)
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

    assertEqualsProb(Probability(0.230), d('A')('a))
    assertEqualsProb(Probability(0.749), d('A')('b))
    assertEqualsProb(Probability(0.011), d('A')('z))
    assertEqualsProb(Probability(0.486), d('B')('a))
    assertEqualsProb(Probability(0.389), d('B')('b))
    assertEqualsProb(Probability(0.007), d('B')('z))
    assertEqualsProb(Probability(0.375), d('C')('a))
    assertEqualsProb(Probability(0.500), d('C')('b))
    assertEqualsProb(Probability(0.063), d('C')('z))
    assertEqualsProb(Probability(0.000), d('Z')('z)); assertTrue(d('Z')('z).toDouble > 0)
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

    assertEqualsProb(Probability(0.273), d('A')('a))
    assertEqualsProb(Probability(0.727), d('A')('b))
    assertEqualsProb(Probability(0.015), d('A')('c))
    assertEqualsProb(Probability(0.015), d('A')('z))
    assertEqualsProb(Probability(0.488), d('B')('a))
    assertEqualsProb(Probability(0.378), d('B')('b))
    assertEqualsProb(Probability(0.134), d('B')('c))
    assertEqualsProb(Probability(0.006), d('B')('z))
    assertEqualsProb(Probability(0.000), d('C')('a))
    assertEqualsProb(Probability(0.000), d('C')('b))
    assertEqualsProb(Probability(0.000), d('C')('c))
    assertEqualsProb(Probability(0.000), d('C')('z))
    assertEqualsProb(Probability(0.000), d('Z')('z))
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
    val d = x.toFreqDist

    assertEqualsProb(Probability(0.235), d('A')('a))
    assertEqualsProb(Probability(0.765), d('A')('b))
    assertEqualsProb(Probability(0.011), d('A')('c))
    assertEqualsProb(Probability(0.011), d('A')('z))
    assertEqualsProb(Probability(0.486), d('B')('a))
    assertEqualsProb(Probability(0.389), d('B')('b))
    assertEqualsProb(Probability(0.125), d('B')('c))
    assertEqualsProb(Probability(0.007), d('B')('z))
    assertEqualsProb(Probability(0.000), d('C')('a))
    assertEqualsProb(Probability(0.000), d('C')('b))
    assertEqualsProb(Probability(0.000), d('C')('c))
    assertEqualsProb(Probability(0.000), d('C')('z))
    assertEqualsProb(Probability(0.000), d('Z')('z))
  }

  @Test
  def test_RandomFreqDist() {
    val x = new RandomCondFreqCounter[Char, Symbol](10, new SimpleCondFreqCounter())
    x ++= CondFreqCounts(List('a -> 'A', 'b -> 'A', 'a -> 'B', 'b -> 'B', 'c -> 'B').map(_.swap).groupByKey.mapValuesStrict(_.counts.mapValuesStrict(_.toDouble)))

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
