package opennlp.scalabha.tag.hmm

import scala.Array.canBuildFrom
import scala.io.Source

import org.apache.commons.logging.LogFactory
import org.apache.log4j.Level
import org.apache.log4j.Logger
import org.junit.Assert.assertEquals
import org.junit.BeforeClass
import org.junit.Test

import opennlp.scalabha.tag.hmm._
import opennlp.scalabha.tag.hmm.support._
import opennlp.scalabha.tag.support._
import opennlp.scalabha.tag._
import opennlp.scalabha.util.CollectionUtils._

class UnsupervisedEmHmmTaggerTrainerTests {
  val LOG = LogFactory.getLog(classOf[UnsupervisedEmHmmTaggerTrainerTests])

  @Test
  def tiny_beforeEM() {
    val trainRaw = List(
      "the dog walks quickly",
      "the cat walks quietly",
      "the dog saw the cat",
      "the cat saw the dog",
      "the dog saw the saw",
      "the bird sings",
      "the mouse walks",
      "the aardvark walks",
      "the aardvark meanders").map(_.split(" ").toIndexedSeq)

    val tagDict = SimpleTagDict(Map(
      "bird" -> Set("N"),
      "cat" -> Set("N"),
      "dog" -> Set("N"),
      "horse" -> Set("N"),
      "mouse" -> Set("N"),
      "quickly" -> Set("R"),
      "quietly" -> Set("R"),
      "saw" -> Set("N", "V"),
      "sings" -> Set("V"),
      "the" -> Set("D"),
      "walks" -> Set("V")))

    val gold = List(
      Vector(("the", "D"), ("bird", "N"), ("walks", "V")),
      Vector(("the", "D"), ("horse", "N"), ("walks", "V")),
      Vector(("the", "D"), ("aardvark", "N"), ("walks", "V")),
      Vector(("the", "D"), ("dog", "N"), ("meanders", "V")),
      Vector(("a", "D"), ("dog", "N"), ("walks", "V"), ("quickly", "R")),
      Vector(("the", "D"), ("moose", "N"), ("walks", "V"), ("quickly", "R")),
      Vector(("the", "D"), ("dog", "N"), ("runs", "V"), ("quickly", "R")),
      Vector(("the", "D"), ("dog", "N"), ("walks", "V"), ("briskly", "R")))

    // Create the initial distributions
    val allTags = tagDict.allTags.map(Option(_)) + None
    val initialTransitions = CondFreqDist(DefaultedCondFreqCounts.fromMap(allTags.mapToVal(allTags.mapToVal(1.).toMap).toMap))
    val initialEmissions =
      new EstimatedRawCountUnsupervisedEmissionDistFactory(
        new PassthroughCountsTransformer(),
        tagDict,
        trainRaw).make()
    val unsupervisedTagger = new HmmTagger(initialTransitions, initialEmissions, tagDict.allTags)

    val output = unsupervisedTagger.tag(gold.map(_.map(_._1)))
    val results = new TaggerEvaluator().evaluate(output, gold, tagDict)
    assertResultsEqual("""
		Total:   53.57 (15/28)
		Known:   59.09 (13/22)
		Unknown: 33.33 (2/6)
		Common Mistakes:
		#Err     Gold      Model
		6        V        N
		4        R        N
		3        D        N
    	""", results)
  }

  @Test
  def tiny() {
    val trainRaw = List(
      "the aardvark walks",
      "the aardvark meanders",
      "the dog walks quickly",
      "the cat walks quietly",
      "the dog saw the cat",
      "the cat saw the dog",
      "the dog saw the saw",
      "the bird sings",
      "the mouse walks").map(_.split(" ").toIndexedSeq)

    val tagDict = SimpleTagDict(Map(
      "bird" -> Set("N"),
      "cat" -> Set("N"),
      "dog" -> Set("N"),
      "horse" -> Set("N"),
      "mouse" -> Set("N"),
      "quickly" -> Set("R"),
      "quietly" -> Set("R"),
      "saw" -> Set("N", "V"),
      "sings" -> Set("V"),
      "the" -> Set("D"),
      "walks" -> Set("V")))

    val gold = List(
      Vector(("a", "D"), ("dog", "N"), ("walks", "V"), ("quickly", "R")),
      Vector(("the", "D"), ("bird", "N"), ("walks", "V")),
      Vector(("the", "D"), ("horse", "N"), ("walks", "V")),
      Vector(("the", "D"), ("aardvark", "N"), ("walks", "V")),
      Vector(("the", "D"), ("moose", "N"), ("walks", "V"), ("quickly", "R")),
      Vector(("the", "D"), ("dog", "N"), ("runs", "V"), ("quickly", "R")),
      Vector(("the", "D"), ("dog", "N"), ("walks", "V"), ("briskly", "R")))

    val unsupervisedTrainer: UnsupervisedTaggerTrainer[String, String] =
      new UnsupervisedEmHmmTaggerTrainer(
        initialUnsupervisedEmissionDist =
          new EstimatedRawCountUnsupervisedEmissionDistFactory(
            new PassthroughCountsTransformer(),
            tagDict,
            trainRaw).make(),
        transitionCountsTransformer =
          new TransitionCountsTransformer(
            EisnerSmoothingCondCountsTransformer(1.)),
        emissionCountsTransformer =
          new EmissionCountsTransformer(
            EisnerSmoothingCondCountsTransformer(1., AddLambdaSmoothingCountsTransformer(1.))),
        hmmTaggerFactory = new HardTagDictConstraintHmmTaggerFactory(OptionalTagDict(tagDict)),
        maxIterations = 1,
        minAvgLogProbChangeForEM = 0.00001)
    val unsupervisedTagger = unsupervisedTrainer.trainUnsupervised(tagDict, trainRaw)
    val output = unsupervisedTagger.tag(gold.map(_.map(_._1)))
    val results = new TaggerEvaluator().evaluate(output, gold, tagDict)
    assertResultsEqual("""
		Total:   100.00 (25/25)
		Known:   100.00 (20/20)
		Unknown: 100.00 (5/5)
		Common Mistakes:
		#Err     Gold      Model
    	""", results)
  }

  @Test
  def ic_fullTagDict_noSmoothing() {
    val trainLab = TaggedFile("data/postag/ic/ictrain.txt")
    val testLab = TaggedFile("data/postag/ic/ictest.txt")

    val testRaw = AsRawFile("data/postag/ic/ictest.txt")
    val gold = TaggedFile("data/postag/ic/ictest.txt")

    val tagDict = new SimpleTagDictFactory().make(trainLab ++ testLab)
    val unsupervisedTrainer: UnsupervisedTaggerTrainer[String, String] =
      new UnsupervisedEmHmmTaggerTrainer(
        initialUnsupervisedEmissionDist =
          new EstimatedRawCountUnsupervisedEmissionDistFactory(
            new PassthroughCountsTransformer(),
            tagDict,
            trainLab.map(_.map(_._1))).make(),
        transitionCountsTransformer =
          new TransitionCountsTransformer(
            EisnerSmoothingCondCountsTransformer(1.)),
        emissionCountsTransformer =
          new EmissionCountsTransformer(
            EisnerSmoothingCondCountsTransformer(1., AddLambdaSmoothingCountsTransformer(1.))),
        hmmTaggerFactory = new HardTagDictConstraintHmmTaggerFactory(OptionalTagDict(tagDict)),
        maxIterations = 20,
        minAvgLogProbChangeForEM = 0.00001)
    val unsupervisedTagger = unsupervisedTrainer.trainUnsupervised(tagDict, trainLab.map(_.map(_._1)))
    val output = unsupervisedTagger.tag(testRaw)
    val results = new TaggerEvaluator().evaluate(output, gold, tagDict)
    assertResultsEqual("""
		Total:   48.48 (16/33)
		Known:   48.48 (16/33)
		Unknown: NaN (0/0)
		Common Mistakes:
		#Err     Gold      Model
		17       H        C
    	""", results)
  }

  @Test
  def en_largeTagDict() {
    val trainLab = TaggedFile("data/postag/english/entrain")
    val tagDict = new SimpleTagDictFactory().make(trainLab)
    val unsupervisedResults = runUnsupervisedTrainingTest(tagDict)
    assertResultsEqual("""
		Total:   89.16 (21353/23949)
		Known:   92.48 (20198/21841)
		Unknown: 54.79 (1155/2108)
		Common Mistakes:
		#Err     Gold      Model
		522      D        F
		375      N        J
		223      N        V
		208      V        N
		122      J        N
    	""", unsupervisedResults)
  }

  @Test
  def en_smallTagDict() {
    val tagDictTrain = TaggedFile("data/postag/english/entrain").take(3000)
    val tagDict = new SimpleTagDictFactory().make(tagDictTrain)
    val unsupervisedResults = runUnsupervisedTrainingTest(tagDict)
    assertResultsEqual("""
		Total:   89.98 (21549/23949)
		Known:   94.56 (20301/21469)
		Unknown: 50.32 (1248/2480)
		Common Mistakes:
		#Err     Gold      Model
		458      N        J
		230      V        N
		224      N        V
		132      N        D
		126      J        N
    	""", unsupervisedResults)
  }

  private def runUnsupervisedTrainingTest(tagDict: TagDict[String, String]) = {
    val trainRaw = RawFile("data/postag/english/enraw20k")
    val gold = TaggedFile("data/postag/english/entest")

    LOG.debug("tagDictTrain.size = " + tagDict.setIterator.ungroup.size)
    LOG.debug("labeledTrain.size = " + 0)
    LOG.debug("rawTrain.size     = " + trainRaw.size)

    val unsupervisedTrainer =
      new UnsupervisedEmHmmTaggerTrainer[String, String](
        initialUnsupervisedEmissionDist =
          new EstimatedRawCountUnsupervisedEmissionDistFactory(
            new PassthroughCountsTransformer(),
            tagDict,
            trainRaw).make(),
        transitionCountsTransformer =
          new TransitionCountsTransformer(
            EisnerSmoothingCondCountsTransformer(1.)),
        emissionCountsTransformer =
          new EmissionCountsTransformer(
            EisnerSmoothingCondCountsTransformer(1., AddLambdaSmoothingCountsTransformer(1.))),
        hmmTaggerFactory = new HardTagDictConstraintHmmTaggerFactory(OptionalTagDict(tagDict)),
        maxIterations = 20,
        minAvgLogProbChangeForEM = 0.00001)

    val unsupervisedTagger = unsupervisedTrainer.trainUnsupervised(tagDict, trainRaw)
    val unsupervisedOutput = unsupervisedTagger.tag(gold.map(_.map(_._1)))
    new TaggerEvaluator().evaluate(unsupervisedOutput, gold, tagDict)
  }

  object TaggedFile {
    def apply(filename: String) =
      Source.fromFile(filename).getLines
        .map(_.trim)
        .split("###/###")
        .filter(_.nonEmpty)
        .map(_.map(_.split("/").toSeq.toTuple2).toIndexedSeq)
        .toList
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
        .toList
  }

  def assertResultsEqual(expectedString: String, results: ScoreResults[String, String]) {
    assertEquals(
      expectedString.split("\n").map(_.trim).filter(_.nonEmpty).mkString("\n"),
      results.toString.split("\n").map(_.trim).filter(_.nonEmpty).mkString("\n"))
  }
}

object UnsupervisedEmHmmTaggerTrainerTests {

  @BeforeClass def turnOffLogging() {
    Logger.getRootLogger.setLevel(Level.OFF)
  }

}
