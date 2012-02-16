package opennlp.scalabha.tag.hmm

import org.junit.Assert._
import org.junit._
import opennlp.scalabha.util.CollectionUtils._
import scala.io.Source
import opennlp.scalabha.tag._
import opennlp.scalabha.tag.support._
import opennlp.scalabha.tag.hmm.support._
import opennlp.scalabha.tag.support._
import support.SimpleTagDictFactory
import opennlp.scalabha.tag.TaggerEvaluator
import opennlp.scalabha.tag.ScoreResults
import org.apache.log4j.Logger
import org.apache.log4j.Level
import org.apache.log4j.PropertyConfigurator
import org.apache.log4j.BasicConfigurator

class HmmTaggerTrainerTests {

  @Test
  def unsupervised_training_en_largeTagDict() {
    val trainLab = TaggedFile("data/postag/english/entrain")
    val results = runUnsupervisedTrainingTest(trainLab)
    assertResultsEqual("""
		Total:   77.13 (18471/23949)
		Known:   84.54 (18464/21841)
		Unknown: 0.33 (7/2108)
		Common Mistakes:
		#Err     Gold      Model
		1174     N        E
		1039     D        N
		492      D        F
		342      V        N
		326      V        E
    	""", results)
  }

  @Test
  def unsupervised_training_en_smallTagDict() {
    val trainLab = TaggedFile("data/postag/english/entrain500")
    val results = runUnsupervisedTrainingTest(trainLab)
    assertResultsEqual("""
		Total:   46.29 (11086/23949)
		Known:   96.61 (10777/11155)
		Unknown: 2.42 (309/12794)
		Common Mistakes:
		#Err     Gold      Model
		5536     N        E
		1890     V        E
		1420     J        E
		776      C        E
		682      R        E
    	""", results)
  }

  private def runUnsupervisedTrainingTest(trainLab: Seq[IndexedSeq[(String, String)]]) = {
    val trainRaw = RawFile("data/postag/english/enraw20k")
    val testRaw = AsRawFile("data/postag/english/entest")
    val gold = TaggedFile("data/postag/english/entest")

    val unsupervisedTrainer: UnsupervisedTaggerTrainer[String, String] =
      new HmmTaggerTrainer(
        initialTransitionCounterFactory = new CondFreqCounterFactory[String, String] { def get() = new SimpleSmoothingTransitionFreqCounter(0.5, "<END>", new SimpleCondFreqCounter()) },
        initialEmissionCounterFactory = new CondFreqCounterFactory[String, String] { def get() = new SimpleSmoothingEmissionFreqCounter(0.5, "<END>", "<END>", new SimpleCondFreqCounter[String, String]()) },
        estimatedTransitionCounterFactory = new CondFreqCounterFactory[String, String] { def get() = new SimpleCondFreqCounter() },
        estimatedEmissionCounterFactory = new CondFreqCounterFactory[String, String] { def get() = new SimpleCondFreqCounter[String, String]() },
        "<END>", "<END>",
        maxIterations = 20,
        minAvgLogProbChangeForEM = 0.00001,
        new SimpleTagDictFactory())
    val tagDict = new SimpleTagDictFactory().make(trainLab)
    val unsupervisedTagger = unsupervisedTrainer.trainUnsupervised(tagDict, trainRaw)
    val output = unsupervisedTagger.tag(testRaw)
    val results = new TaggerEvaluator().evaluate(output, gold, unsupervisedTagger.asInstanceOf[HmmTagger[String, String]].tagDict)
    results
  }

  object TaggedFile {
    def apply(filename: String) =
      Source.fromFile(filename).getLines
        .map(_.trim)
        .split("###/###")
        .filter(_.nonEmpty)
        .map(_.map(_.split("/").toSeq match { case Seq(w, t) => (w, t) }).toIndexedSeq)
  }

  object AsRawFile {
    def apply(filename: String) =
      TaggedFile(filename).map(_.map(_._1))
  }

  object RawFile {
    def apply(filename: String) =
      Source.fromFile(filename).getLines
        .map(_.trim)
        .split("###")
        .filter(_.nonEmpty)
        .map(_.toIndexedSeq)
  }

  def assertResultsEqual(expectedString: String, results: ScoreResults[String, String]) {
    assertEquals(
      expectedString.split("\n").map(_.trim).filter(_.nonEmpty).mkString("\n"),
      results.toString.split("\n").map(_.trim).filter(_.nonEmpty).mkString("\n"))
  }
}

object HmmTaggerTrainerTests {

  @BeforeClass def turnOffLogging() {
    Logger.getRootLogger.setLevel(Level.INFO)
  }

}
