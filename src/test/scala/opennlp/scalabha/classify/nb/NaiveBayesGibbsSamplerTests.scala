package opennlp.scalabha.classify.nb

import scala.io.Source

import org.apache.commons.logging.LogFactory
import org.apache.log4j.Level
import org.apache.log4j.Logger
import org.junit.Assert._
import org.junit.BeforeClass
import org.junit.Test

import breeze.stats.distributions.Beta
import opennlp.scalabha.classify.CompositeClassifierEvaluator
import opennlp.scalabha.classify.CsvLabeledInstanceSource
import opennlp.scalabha.classify.DefaultInstance
import opennlp.scalabha.classify.SimpleClassifierEvaluator
import opennlp.scalabha.tag.support.BinomialFreqDist
import opennlp.scalabha.tag.support.FreqDist
import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.util.LogNum
import opennlp.scalabha.util.LogNum._
import opennlp.scalabha.util.Stats.DirichletSampler

class NaiveBayesGibbsSamplerTests {
  val LOG = LogFactory.getLog(NaiveBayesGibbsSamplerTests.getClass)

  @Test
  def test_balanceGenerator() {
    val b = new Beta(5., 2.)
    val m = Iterator.fill(500)(b.sample).flatMap(p => Iterator.fill(20)(BinomialFreqDist("N", "S", p.toLogNum).sample)).counts.normalizeValues
    println(m)
    // Map(S -> 0.28847, N -> 0.71153)
  }

  @Test
  def test_wordDistGenerator() {
    val vocabulary = List("A", "B", "C", "D")

    def go(pseudocounts: Map[String, Double]) = {
      val dirichlet = DirichletSampler(vocabulary, pseudocounts)
      Iterator.fill(1000)(dirichlet.sample).sumByKey.normalizeValues
    }

    val a = go(Map[String, Double]().withDefaultValue(1.0))
    println(a)
    // Map(A -> 0.26159122413172337, B -> 0.24945911486440997, C -> 0.2518183415036672, D -> 0.23713131950019958)

    val b = go(Map("A" -> 4., "B" -> 2., "C" -> 1., "D" -> 1.))
    println(b)
    // Map(A -> 0.499068071427437, B -> 0.2536510845705246, C -> 0.12212724629356361, D -> 0.12515359770847484)
  }

  @Test
  def test_generated_noOverlap {
    println("\nNo Overlap\n=======================\n")

    val g = new NaiveBayesGenerator(
      priorProb = BinomialFreqDist("N", "S", LogNum(0.75)),
      featureProb = Map(
        "N" -> FreqDist(Map("A" -> 40, "B" -> 30, "C" -> 20, "D" -> 10)),
        "S" -> FreqDist(Map("E" -> 10, "F" -> 20, "G" -> 30, "H" -> 40))))
    runTestGenerated(g)

    //   Supervised accuracy = 0.34 (34/100)
    // Unsupervised accuracy = 1.0 (100/100); model output dist = Map(S -> 0.32, N -> 0.68)
  }

  @Test
  def test_generated_tinyOverlap {
    println("\nTiny Overlap\n=======================\n")

    val g = new NaiveBayesGenerator(
      priorProb = BinomialFreqDist("N", "S", LogNum(0.75)),
      featureProb = Map(
        "N" -> FreqDist(Map("A" -> 50, "B" -> 40, "C" -> 5, "D" -> 5)),
        "S" -> FreqDist(Map("C" -> 5, "D" -> 5, "E" -> 40, "F" -> 50))))
    runTestGenerated(g)

    //   Supervised accuracy = 0.73 (73/100)
    // Unsupervised accuracy = accuracy = 1.0 (100/100); model output dist = Map(S -> 0.28, N -> 0.72)
  }

  @Test
  def test_generated_partOverlap {
    println("\nPartial Overlap\n=======================\n")

    val g = new NaiveBayesGenerator(
      priorProb = BinomialFreqDist("N", "S", LogNum(0.75)),
      featureProb = Map(
        "N" -> FreqDist(Map("A" -> 40, "B" -> 30, "C" -> 20, "D" -> 10)),
        "S" -> FreqDist(Map("C" -> 10, "D" -> 20, "E" -> 30, "F" -> 40))))
    runTestGenerated(g)

    //   Supervised accuracy = 0.26 (26/100)
    // Unsupervised accuracy = accuracy = 1.0 (100/100); model output dist = Map(S -> 0.2, N -> 0.8)
  }

  @Test
  def test_generated_fullOverlap {
    println("\nFull Overlap\n=======================\n")

    val g = new NaiveBayesGenerator(
      priorProb = BinomialFreqDist("N", "S", LogNum(0.75)),
      featureProb = Map(
        "N" -> FreqDist(Map("A" -> 38, "B" -> 30, "C" -> 20, "D" -> 10, "E" -> 1, "F" -> 1)),
        "S" -> FreqDist(Map("A" -> 1, "B" -> 1, "C" -> 10, "D" -> 20, "E" -> 30, "F" -> 38))))
    runTestGenerated(g)

    //   Supervised accuracy = 0.39 (39/100)
    // Unsupervised accuracy = 0.92 (92/100); model output dist = Map(S -> 0.18, N -> 0.82)
  }

  private def runTestGenerated(g: NaiveBayesGenerator[String, String]) {
    val labeled = Seq(g.generate("N", 4), g.generate("S", 4))
    val unlabeled = Seq.fill(100)(g.generate(4))
    val test = Seq.fill(100)(g.generate(4))

    val eval = CompositeClassifierEvaluator(labeled, unlabeled, test)

    // Supervised
    val sc = OnlineNaiveBayesClassifierTrainer[String, String](0.)(labeled.mapVals(new DefaultInstance(_)))
    LOG.debug(sc.priorProb)
    LOG.debug(sc.featureProb)
    eval(sc).map(_.accuracyStr) foreach println

    // Unsupervised
    val uc = new NaiveBayesGibbsSampler(
      List("N", "S"),
      100, 100, 5,
      Map("N" -> 5, "S" -> 2),
      Function.const(Function.const(1) _),
      None)(
      unlabeled.map(_._2).iterator,
      labeled.iterator)
    LOG.debug(uc.priorProb)
    LOG.debug(uc.featureProb)
    eval(uc).map(_.accuracyStr) foreach println
  }

  @Test
  def test_tennis() {
    println("\nTennis\n==============\n")

    val trainer = OnlineNaiveBayesClassifierTrainer[String, String](0)
    val train = new CsvLabeledInstanceSource(Source.fromFile("data/classify/tennis/train"))
    val test = new CsvLabeledInstanceSource(Source.fromFile("data/classify/tennis/test"))
    val eval = new SimpleClassifierEvaluator(test.getLabeledInstances.mapVals(_.allFeatures.toSeq).toList)

    // Supervised
    val sc = trainer(train.getLabeledInstances)
    eval(sc)

    // Unsupervised
    val uc =
      new NaiveBayesGibbsSampler(
        List("Yes", "No"),
        100, 100, 5,
        Function.const(1),
        Function.const(Function.const(1) _),
        Some(eval))(
        train.getLabeledInstances.map(_._2.allFeatures.toSeq).toList,
        List())
    eval(uc) foreach println
  }
  def assertEqualsLog(a: LogNum, b: LogNum) {
    assertEquals(a.toDouble, b.toDouble, 0.0000001)
  }

}

object NaiveBayesGibbsSamplerTests {

  @BeforeClass def turnOffLogging() {
    Logger.getRootLogger.setLevel(Level.OFF)
  }

}
