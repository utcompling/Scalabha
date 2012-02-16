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
  def semisupervised_training_en_smoothed() {
    val trainLab = TaggedFile("data/postag/english/entrain500")
    val trainRaw = RawFile("data/postag/english/enraw20k")
    val testRaw = AsRawFile("data/postag/english/entest")
    val gold = TaggedFile("data/postag/english/entest")

    val transitionCounterFactory = new CondFreqCounterFactory[String, String] { def get() = new SimpleSmoothingTransitionFreqCounter(0.5, "<END>", new SimpleCondFreqCounter()) }
    val emissionCounterFactory = new CondFreqCounterFactory[String, String] { def get() = new SimpleSmoothingEmissionFreqCounter(0.5, "<END>", "<END>", new SimpleCondFreqCounter[String, String]()) }

    {
      ///////////////////////////////
      // First, see how well supervised learning does on the small labeled set.
      // The result: not very well.
      ///////////////////////////////
      val supervisedTrainer: SupervisedTaggerTrainer[String, String] =
        new SupervisedHmmTaggerTrainer(
          transitionCounterFactory = transitionCounterFactory,
          emissionCounterFactory = emissionCounterFactory,
          "<END>", "<END>",
          new SimpleTagDictFactory())
      val supervisedTagger = supervisedTrainer.trainSupervised(trainLab)
      val output = supervisedTagger.tag(testRaw)
      val results = new TaggerEvaluator().evaluate(output, gold, supervisedTagger.asInstanceOf[HmmTagger[String, String]].tagDict)
      assertResultsEqual("""
		Total:   72.16 (17282/23949)
		Known:   98.06 (10939/11155)
		Unknown: 49.58 (6343/12794)
		Common Mistakes:
		#Err     Gold      Model
		1113     J        N       
		938      V        N       
		496      R        N       
		461      C        N       
		399      N        J   
		""", results)
    }

    {
      ///////////////////////////////
      // Now add raw training data and do semi-supervised training.
      // The result: hopefully better than labeled data alone.
      ///////////////////////////////
      val unsupervisedTrainer: UnsupervisedTaggerTrainer[String, String] =
        new HmmTaggerTrainer(
          initialTransitionCounterFactory = transitionCounterFactory,
          initialEmissionCounterFactory = emissionCounterFactory,
          estimatedTransitionCounterFactory = new CondFreqCounterFactory[String, String] { def get() = new SimpleCondFreqCounter() },
          estimatedEmissionCounterFactory = new CondFreqCounterFactory[String, String] { def get() = new SimpleCondFreqCounter[String, String]() },
          "<END>", "<END>",
          maxIterations = 20,
          minAvgLogProbChangeForEM = 0.00001,
          new SimpleTagDictFactory())
      val unsupervisedTagger = unsupervisedTrainer.trainSemisupervised(trainRaw, trainLab)
      val output = unsupervisedTagger.tag(testRaw)
      val results = new TaggerEvaluator().evaluate(output, gold, unsupervisedTagger.asInstanceOf[HmmTagger[String, String]].tagDict)
      assertResultsEqual("""
    	""", results)
    }
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
