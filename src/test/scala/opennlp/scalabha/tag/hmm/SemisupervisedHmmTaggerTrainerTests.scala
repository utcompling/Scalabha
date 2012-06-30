package opennlp.scalabha.tag.hmm

import org.junit.Assert._
import org.junit._
import opennlp.scalabha.util.CollectionUtils._
import scala.io.Source
import opennlp.scalabha.tag._
import opennlp.scalabha.tag.support._
import opennlp.scalabha.tag.hmm.support._
import opennlp.scalabha.tag.TaggerEvaluator
import opennlp.scalabha.tag.ScoreResults
import org.apache.log4j.Logger
import org.apache.log4j.Level
import org.apache.log4j.PropertyConfigurator
import org.apache.log4j.BasicConfigurator
import org.apache.commons.logging.LogFactory

class SemisupervisedHmmTaggerTrainerTests {
  val LOG = LogFactory.getLog(classOf[SemisupervisedHmmTaggerTrainerTests])

  @Test
  def en_smallTagDict() {
    val (tagDictTrain, labeledTrain) = TaggedFile("data/postag/english/entrain").splitAt(3000)
    val tagDict = new SimpleTagDictFactory().make(tagDictTrain)
    val (semisupervisedResults, autosupervisedResults) = runUnsupervisedTrainingTest(tagDict, tagDictTrain)
    assertResultsEqual("""
		Total:   83.81 (20071/23949)
		Known:   93.34 (20040/21469)
		Unknown: 1.25 (31/2480)
		Common Mistakes:
		#Err     Gold      Model
		1359     N        .
		393      V        .
		341      J        .
		246      V        N
    	229      C        .
    	""", semisupervisedResults)
    assertResultsEqual("""
		Total:   90.42 (21655/23949)
		Known:   96.01 (20613/21469)
		Unknown: 42.02 (1042/2480)
		Common Mistakes:
		#Err     Gold      Model
		185      V        N
		180      N        J
		168      N        D
		156      N        V
		115      J        N
    	""", autosupervisedResults)
  }

  private def runUnsupervisedTrainingTest(tagDict: TagDict[String, String], trainLab: Seq[IndexedSeq[(String, String)]]) = {
    val trainRaw = RawFile("data/postag/english/enraw20k")
    val gold = TaggedFile("data/postag/english/entest")

    LOG.info("tagDictTrain.size = " + tagDict.setIterator.ungroup.size)
    LOG.info("labeledTrain.size = " + trainLab.size)
    LOG.info("rawTrain.size     = " + trainRaw.size)

    val trainer: SemisupervisedTaggerTrainer[String, String] =
      new SemisupervisedHmmTaggerTrainer(
        initialTransitionCountsTransformer =
          new TransitionCountsTransformer(tagDict,
            EisnerSmoothingCondCountsTransformer(lambda = 1.0)),
        initialEmissionCountsTransformer =
          new EmissionCountsTransformer(
            EisnerSmoothingCondCountsTransformer(lambda = 1.0, backoffCountsTransformer = AddLambdaSmoothingCountsTransformer(lambda = 1.0))),
        estimatedTransitionCountsTransformer = TransitionCountsTransformer(tagDict),
        estimatedEmissionCountsTransformer = EmissionCountsTransformer(),
        maxIterations = 20,
        minAvgLogProbChangeForEM = 0.00001)
    val semisupervisedTagger = trainer.trainSemisupervised(tagDict, trainRaw, trainLab)
    val output = semisupervisedTagger.tag(gold.map(_.map(_._1)))
    val semisupervisedResults = new TaggerEvaluator().evaluate(output, gold, tagDict)

    val supervisedTrainer: SupervisedTaggerTrainer[String, String] =
      new SupervisedHmmTaggerTrainer(
        transitionCountsTransformer =
          new TransitionCountsTransformer(tagDict,
            EisnerSmoothingCondCountsTransformer(1.)),
        emissionCountsTransformer =
          new EmissionCountsTransformer(
            EisnerSmoothingCondCountsTransformer(1., AddLambdaSmoothingCountsTransformer(1.))))
    val semisupervisedAutotagged = semisupervisedTagger.tag(trainRaw)
    val autosupervisedTagger = supervisedTrainer.trainSupervised(semisupervisedAutotagged, tagDict)
    val autosupervisedOutput = autosupervisedTagger.tag(gold.map(_.map(_._1)))
    val autosupervisedResults = new TaggerEvaluator().evaluate(autosupervisedOutput, gold, tagDict)

    (semisupervisedResults, autosupervisedResults)
  }

  object TaggedFile {
    def apply(filename: String): List[IndexedSeq[(String, String)]] =
      Source.fromFile(filename).getLines
        .map(_.trim)
        .split("###/###")
        .filter(_.nonEmpty)
        .map(_.map(_.split("/").toSeq.toTuple2).toIndexedSeq)
        .toList
  }

  object AsRawFile {
    def apply(filename: String): List[IndexedSeq[String]] =
      TaggedFile(filename).map(_.map(_._1))
  }

  object RawFile {
    def apply(filename: String): List[IndexedSeq[String]] =
      Source.fromFile(filename).getLines
        .map(_.trim)
        .split("###")
        .filter(_.nonEmpty)
        .map(_.toIndexedSeq)
        .toList
  }

  def assertResultsEqual(expectedString: String, results: ScoreResults[String, String]) {
    assertEquals(
      expectedString.split("\n").map(_.trim).filter(_.nonEmpty).mkString("\n"),
      results.toString.split("\n").map(_.trim).filter(_.nonEmpty).mkString("\n"))
  }
}

object SemisupervisedHmmTaggerTrainerTests {

  @BeforeClass def turnOffLogging() {
    Logger.getRootLogger.setLevel(Level.OFF)
  }

}
