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
                Total:   15.68 (3755/23949)
                Known:   17.42 (3739/21469)
                Unknown: 0.65 (16/2480)
                Common Mistakes:
                #Err     Gold      Model
                6272     N        .
                2758     V        .
                2218     I        .
                1824     D        .
                1499     J        .
    	""", semisupervisedResults)
    assertResultsEqual("""
                Total:   86.41 (20694/23949)
                Known:   90.63 (19458/21469)
                Unknown: 49.84 (1236/2480)
                Common Mistakes:
                #Err     Gold      Model
                345      N        J
                314      V        N
                223      N        V
                183      J        N
                154      N        M
    	""", autosupervisedResults)
  }

  @Test
  def en_smallTagDict_tdConstEmTraining() {
    // TODO: WRITE THIS TEST
  }

  @Test
  def en_smallTagDict_tdConstEmTagging() {
    // TODO: WRITE THIS TEST
  }

  @Test
  def en_smallTagDict_tdConstEmTrainingAndEmTagging() {
    // TODO: WRITE THIS TEST
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
          new TransitionCountsTransformer(
            EisnerSmoothingCondCountsTransformer(lambda = 1.0)),
        initialEmissionCountsTransformer =
          new EmissionCountsTransformer(
            EisnerSmoothingCondCountsTransformer(lambda = 1.0, backoffCountsTransformer = AddLambdaSmoothingCountsTransformer(lambda = 1.0))),
        hmmTaggerFactory = new SimpleHmmTaggerFactory(tagDict.allTags),
        estimatedTransitionCountsTransformer = TransitionCountsTransformer(),
        estimatedEmissionCountsTransformer = EmissionCountsTransformer(),
        maxIterations = 20,
        minAvgLogProbChangeForEM = 0.00001)
    val semisupervisedTagger = trainer.trainSemisupervised(tagDict, trainRaw, trainLab)
    val output = semisupervisedTagger.tag(gold.map(_.map(_._1)))
    val semisupervisedResults = new TaggerEvaluator().evaluate(output, gold, tagDict)

    val supervisedTrainer: SupervisedTaggerTrainer[String, String] =
      new SupervisedHmmTaggerTrainer(
        transitionCountsTransformer =
          new TransitionCountsTransformer(
            EisnerSmoothingCondCountsTransformer(1.)),
        emissionCountsTransformer =
          new EmissionCountsTransformer(
            EisnerSmoothingCondCountsTransformer(1., AddLambdaSmoothingCountsTransformer(1.))),
        hmmTaggerFactory = new SimpleHmmTaggerFactory(tagDict.allTags))
    val semisupervisedAutotagged = semisupervisedTagger.tag(trainRaw)
    val autosupervisedTagger = supervisedTrainer.trainSupervised(semisupervisedAutotagged)
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
