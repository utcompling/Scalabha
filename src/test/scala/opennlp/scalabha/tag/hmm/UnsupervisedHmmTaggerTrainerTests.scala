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

class UnsupervisedHmmTaggerTrainerTests {
  val LOG = LogFactory.getLog(classOf[UnsupervisedHmmTaggerTrainerTests])

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
    val initialTransitions = CondFreqDist(DefaultedCondFreqCounts(allTags.mapTo(_ => allTags.mapTo(_ => 1.0).toMap).toMap))
    val initialEmissions =
      new EstimatedRawCountUnsupervisedEmissionDistFactory(
        new PassthroughCountsTransformer(),
        tagDict,
        trainRaw).make()
    val unsupervisedTagger = new HmmTagger(initialTransitions, initialEmissions, OptionalTagDict(tagDict))

    val output = unsupervisedTagger.tag(gold.map(_.map(_._1)))
    val results = new TaggerEvaluator().evaluate(output, gold, tagDict)
    assertResultsEqual("""
		Total:   84.21 (16/19)
		Known:   100.00 (15/15)
		Unknown: 25.00 (1/4)
		Common Mistakes:
		#Err     Gold      Model
		1        V        N
		1        D        N
		1        R        N
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
      new UnsupervisedHmmTaggerTrainer(
        initialUnsupervisedEmissionDist =
          new EstimatedRawCountUnsupervisedEmissionDistFactory(
            new PassthroughCountsTransformer(),
            tagDict,
            trainRaw).make(),
        estimatedTransitionCountsTransformer = TransitionCountsTransformer(tagDict),
        estimatedEmissionCountsTransformer = EmissionCountsTransformer(),
        maxIterations = 1,
        minAvgLogProbChangeForEM = 0.00001)
    val unsupervisedTagger = unsupervisedTrainer.trainUnsupervised(tagDict, trainRaw)
    val output = unsupervisedTagger.tag(gold.map(_.map(_._1)))
    val results = new TaggerEvaluator().evaluate(output, gold, tagDict)
    assertResultsEqual("""
		Total:   84.21 (16/19)
		Known:   100.00 (15/15)
		Unknown: 25.00 (1/4)
		Common Mistakes:
		#Err     Gold      Model
		1        V        N
		1        D        N
		1        R        N
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
      new UnsupervisedHmmTaggerTrainer(
        initialUnsupervisedEmissionDist =
          new EstimatedRawCountUnsupervisedEmissionDistFactory(
            new PassthroughCountsTransformer(),
            tagDict,
            trainLab.map(_.map(_._1))).make(),
        estimatedTransitionCountsTransformer = TransitionCountsTransformer(tagDict),
        estimatedEmissionCountsTransformer = EmissionCountsTransformer(),
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
  def en_fullTagDict_noSmoothing() {
    val trainLab = TaggedFile("data/postag/english/entrain")
    val testLab = TaggedFile("data/postag/english/entest")

    val testRaw = AsRawFile("data/postag/english/entest")
    val gold = TaggedFile("data/postag/english/entest")

    val tagDict = new SimpleTagDictFactory().make(trainLab ++ testLab)
    val unsupervisedTrainer: UnsupervisedTaggerTrainer[String, String] =
      new UnsupervisedHmmTaggerTrainer(
        initialUnsupervisedEmissionDist =
          new EstimatedRawCountUnsupervisedEmissionDistFactory(
            new PassthroughCountsTransformer(),
            tagDict,
            trainLab.map(_.map(_._1))).make(),
        estimatedTransitionCountsTransformer = TransitionCountsTransformer(tagDict),
        estimatedEmissionCountsTransformer = EmissionCountsTransformer(),
        maxIterations = 20,
        minAvgLogProbChangeForEM = 0.00001)
    val unsupervisedTagger = unsupervisedTrainer.trainUnsupervised(tagDict, trainLab.map(_.map(_._1)))
    val output = unsupervisedTagger.tag(testRaw)
    val results = new TaggerEvaluator().evaluate(output, gold, tagDict)
    assertResultsEqual("""
		Total:   88.14 (21108/23949)
		Known:   88.14 (21108/23949)
		Unknown: NaN (0/0)
		Common Mistakes:
		#Err     Gold      Model
		693      D        N
		522      D        F
		277      V        N
		200      P        V
		198      J        N
    	""", results)
  }

  @Test
  def en_testInTagDict() {
    val trainLab = TaggedFile("data/postag/english/entrain") ++ TaggedFile("data/postag/english/entest")
    val tagDict = new SimpleTagDictFactory().make(trainLab)
    val results = runUnsupervisedTrainingTest(tagDict)
    assertResultsEqual("""
		Total:   94.80 (22703/23949)
		Known:   94.80 (22703/23949)
		Unknown: NaN (0/0)
		Common Mistakes:
		#Err     Gold      Model
		200      P        V
		126      N        J
		106      V        N
		105      I        R
		102      N        V
    	""", results)
  }

  @Test
  def en_largeTagDict() {
    val trainLab = TaggedFile("data/postag/english/entrain")
    val tagDict = new SimpleTagDictFactory().make(trainLab)
    val results = runUnsupervisedTrainingTest(tagDict)
    assertResultsEqual("""
		Total:   89.26 (21377/23949)
		Known:   93.84 (20495/21841)
		Unknown: 41.84 (882/2108)
		Common Mistakes:
		#Err     Gold      Model
		221      N        J
		200      P        V
		195      N        V
		184      V        N
		140      N        D
    	""", results)
  }

  def en_smallTagDict() {
    val (tagDictTrain, labeledTrain) = TaggedFile("data/postag/english/entrain").splitAt(3000)
    val tagDict = new SimpleTagDictFactory().make(tagDictTrain)
    val results = runUnsupervisedTrainingTest(tagDict)
    assertResultsEqual("""
		Total:   89.29 (21384/23949)
		Known:   94.65 (20320/21469)
		Unknown: 42.90 (1064/2480)
		Common Mistakes:
		#Err     Gold      Model
		318      N        J
		239      N        V
		188      V        N
		175      N        D
		132      J        N
    	""", results)
  }

  private def runUnsupervisedTrainingTest(tagDict: TagDict[String, String]) = {
    val trainRaw = RawFile("data/postag/english/enraw20k")
    val gold = TaggedFile("data/postag/english/entest")

    LOG.debug("tagDictTrain.size = " + tagDict.iterator.ungroup.size)
    LOG.debug("labeledTrain.size = " + 0)
    LOG.debug("rawTrain.size     = " + trainRaw.size)

    val unsupervisedTrainer: UnsupervisedTaggerTrainer[String, String] =
      new UnsupervisedHmmTaggerTrainer(
        initialUnsupervisedEmissionDist =
          new EstimatedRawCountUnsupervisedEmissionDistFactory(
            new PassthroughCountsTransformer(),
            tagDict,
            trainRaw).make(),
        estimatedTransitionCountsTransformer = TransitionCountsTransformer(tagDict),
        estimatedEmissionCountsTransformer = EmissionCountsTransformer(),
        maxIterations = 20,
        minAvgLogProbChangeForEM = 0.00001) {

        protected override def hmmExaminationHook(hmm: HmmTagger[String, String]) {
          val output = hmm.tag(gold.map(_.map(_._1)))
          val results = new TaggerEvaluator().evaluate(output, gold, tagDict)
          println(results)
        }

      }

    val unsupervisedTagger = unsupervisedTrainer.trainUnsupervised(tagDict, trainRaw)
    val output = unsupervisedTagger.tag(gold.map(_.map(_._1)))
    val results = new TaggerEvaluator().evaluate(output, gold, tagDict)
    results
  }

  object TaggedFile {
    def apply(filename: String) =
      Source.fromFile(filename).getLines
        .map(_.trim)
        .split("###/###")
        .filter(_.nonEmpty)
        .map(_.map(_.split("/").toSeq.toTuple2).toIndexedSeq)
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

object UnsupervisedHmmTaggerTrainerTests {

  @BeforeClass def turnOffLogging() {
    Logger.getRootLogger.setLevel(Level.OFF)
  }

}
