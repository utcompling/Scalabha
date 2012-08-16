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

class SemisupervisedEmHmmTaggerTrainerTests {
  val LOG = LogFactory.getLog(classOf[SemisupervisedEmHmmTaggerTrainerTests])

  @Test
  def en_smallTagDict() {
    val (tagDictTrain, labeledTrain) = TaggedFile("data/postag/english/entrain").splitAt(3000)
    val tagDict = new SimpleTagDictFactory().make(tagDictTrain)
    val semisupervisedResults = runUnsupervisedTrainingTest(tagDict, labeledTrain)

    assertResultsEqual("""
	            Total:   91.26 (21857/23949)
				Known:   94.22 (20228/21469)
				Unknown: 65.69 (1629/2480)
				Common Mistakes:
				#Err     Gold      Model
				416      N        J       
				311      V        N       
				210      J        N       
				171      N        V       
				133      V        J       
    	""", semisupervisedResults)
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
      new SemisupervisedEmHmmTaggerTrainer(
        transitionCountsTransformer =
          new TransitionCountsTransformer(
            EisnerSmoothingCondCountsTransformer(lambda = 1.0)),
        emissionCountsTransformer =
          new EmissionCountsTransformer(
            EisnerSmoothingCondCountsTransformer(lambda = 1.0, backoffCountsTransformer = AddLambdaSmoothingCountsTransformer(lambda = 1.0))),
        hmmTaggerFactory = new SimpleHmmTaggerFactory(tagDict.allTags),
        maxIterations = 20,
        minAvgLogProbChangeForEM = 0.00001)
    val tagger = trainer.trainSemisupervised(tagDict, trainRaw, trainLab)
    val output = tagger.tag(gold.map(_.map(_._1)))
    new TaggerEvaluator().evaluate(output, gold, tagDict)
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

object SemisupervisedEmHmmTaggerTrainerTests {

  @BeforeClass def turnOffLogging() {
    Logger.getRootLogger.setLevel(Level.OFF)
  }

}
