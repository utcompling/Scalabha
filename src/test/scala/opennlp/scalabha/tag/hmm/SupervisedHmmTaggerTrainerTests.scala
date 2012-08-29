package opennlp.scalabha.tag.hmm

import org.junit.Assert._
import org.junit._
import opennlp.scalabha.util.CollectionUtils._
import scala.io.Source
import opennlp.scalabha.tag.SupervisedTaggerTrainer
import opennlp.scalabha.tag.Tagger
import opennlp.scalabha.tag.hmm.support._
import opennlp.scalabha.tag.support._
import opennlp.scalabha.tag.TaggerEvaluator
import opennlp.scalabha.tag.ScoreResults
import org.apache.log4j.Logger
import org.apache.log4j.Level
import org.apache.commons.logging.LogFactory
import opennlp.scalabha.tag.OptionalTagDict
import opennlp.scalabha.tag.TagDict
import opennlp.scalabha.util.LogNum
import opennlp.scalabha.util.FileUtils

class SupervisedHmmTaggerTrainerTests {
  val LOG = LogFactory.getLog(classOf[SupervisedHmmTaggerTrainerTests])

  @Test
  def tiny_noSmoothing() {
    val train = List(
      "the/D dog/N walks/V ./.",
      "the/D cat/N walks/V ./.",
      "a/D dog/N barks/V ./.")
      .map(_.split(" ").map(_.split("/").toSeq.toTuple2).toIndexedSeq)

    val tagDict = new SimpleTagDictFactory().make(train)
    val trainer: SupervisedTaggerTrainer[String, String] =
      new SupervisedHmmTaggerTrainer(
        transitionCountsTransformer = TransitionCountsTransformer(),
        emissionCountsTransformer = EmissionCountsTransformer(),
        hmmTaggerFactory = new SimpleHmmTaggerFactory(tagDict.allTags))
    val tagger: Tagger[String, String] = trainer.trainSupervised(train)
    assertEquals(Vector(Vector("D", "N", "V", ".")), tagger.tag(Vector("the dog walks . ".split(" "))).map(_.map(_._2)))
    assertEquals(Vector(Vector("D", "N", "V", ".")), tagger.tag(Vector("a cat walks . ".split(" "))).map(_.map(_._2)))
  }

  @Test
  def tiny_smoothed() {
    val train = List(
      "the/D dog/N walks/V ./.",
      "the/D cat/N walks/V ./.",
      "a/D dog/N barks/V ./.")
      .map(_.split(" ").map(_.split("/").toSeq.toTuple2).toIndexedSeq)

    val tagDict = new SimpleTagDictFactory().make(train)
    val trainer: SupervisedTaggerTrainer[String, String] =
      new SupervisedHmmTaggerTrainer(
        transitionCountsTransformer =
          new TransitionCountsTransformer(
            EisnerSmoothingCondCountsTransformer(lambda = 1.0)),
        emissionCountsTransformer =
          new EmissionCountsTransformer(
            EisnerSmoothingCondCountsTransformer(lambda = 1.0, backoffCountsTransformer = AddLambdaSmoothingCountsTransformer(lambda = 1.0))),
        hmmTaggerFactory = new SimpleHmmTaggerFactory(tagDict.allTags))
    val tagger: Tagger[String, String] = trainer.trainSupervised(train)
    assertEquals(Vector(Vector("D", "N", "V", ".")), tagger.tag(Vector("the dog walks . ".split(" "))).map(_.map(_._2)))
    assertEquals(Vector(Vector("D", "N", "V", ".")), tagger.tag(Vector("a cat meows . ".split(" "))).map(_.map(_._2)))
    assertEquals(Vector(Vector("D", "N", "V", ".")), tagger.tag(Vector("this bird chirps . ".split(" "))).map(_.map(_._2)))
  }

  @Test
  def ic_noSmoothing() {
    val train = TaggedFile("data/postag/ic/ictrain.txt")

    val tagDict = new SimpleTagDictFactory().make(train)
    val trainer: SupervisedTaggerTrainer[String, String] =
      new SupervisedHmmTaggerTrainer(
        transitionCountsTransformer = TransitionCountsTransformer(),
        emissionCountsTransformer = EmissionCountsTransformer(),
        hmmTaggerFactory = new SimpleHmmTaggerFactory(tagDict.allTags))
    val tagger: Tagger[String, String] = trainer.trainSupervised(train)

    val output = tagger.tag(AsRawFile("data/postag/ic/ictest.txt"))

    val gold = TaggedFile("data/postag/ic/ictest.txt")
    val results = new TaggerEvaluator().evaluate(output, gold, tagDict)
    assertResultsEqual("""
		Total:   87.88 (29/33)
		Known:   87.88 (29/33)
		Unknown: NaN (0/0)
		Common Mistakes:
		#Err     Gold      Model
		3        C        H       
		1        H        C       
    	""", results)
  }

  @Test
  def en_noSmoothing() {
    val train = TaggedFile("data/postag/english/entrain")

    val tagDict = new SimpleTagDictFactory().make(train)
    val trainer: SupervisedTaggerTrainer[String, String] =
      new SupervisedHmmTaggerTrainer(
        transitionCountsTransformer = TransitionCountsTransformer(),
        emissionCountsTransformer = EmissionCountsTransformer(),
        hmmTaggerFactory = new SimpleHmmTaggerFactory(tagDict.allTags))
    val tagger: Tagger[String, String] = trainer.trainSupervised(train)

    val output = tagger.tag(AsRawFile("data/postag/english/entest"))

    val gold = TaggedFile("data/postag/english/entest")
    val results = new TaggerEvaluator().evaluate(output, gold, tagDict)
    assertResultsEqual("""
				Total:   16.82 (4028/23949)
				Known:   18.44 (4028/21841)
				Unknown: 0.00 (0/2108)
				Common Mistakes:
				#Err     Gold      Model
				6190     N        .
				2732     V        .
				2194     I        .
				1791     D        .
				1488     J        .
                """, results)
  }

  @Test
  def en_eisnerSmoothing() {
    val train = TaggedFile("data/postag/english/entrain")
    val tagDict = new SimpleTagDictFactory().make(train)
    val gold = TaggedFile("data/postag/english/entest")
    //val gold = List(Vector(("The", "D"), ("<unknown>", "N"), ("runs", "V"), (".", ".")))

    LOG.debug("tagDictTrain.size = " + tagDict.setIterator.ungroup.size)
    LOG.debug("labeledTrain.size = " + train.size)
    LOG.debug("rawTrain.size     = " + 0)

    val trainer =
      new SupervisedHmmTaggerTrainer[String, String](
        transitionCountsTransformer =
          new TransitionCountsTransformer(
            EisnerSmoothingCondCountsTransformer(lambda = 1.0)),
        emissionCountsTransformer =
          new EmissionCountsTransformer(
            EisnerSmoothingCondCountsTransformer(lambda = 1.0, backoffCountsTransformer = AddLambdaSmoothingCountsTransformer(lambda = 1.0))),
        hmmTaggerFactory = new SimpleHmmTaggerFactory(tagDict.allTags))
    val tagger = trainer.trainSupervised(train)

    //    train.flatMap(_.map(_._2).toSet).toSet
    //      .mapTo { t => tagger.emissions(Some(t))(Some("<unknown")) }
    //      .toList.sortBy(_._2).reverse
    //      .foreach { case (t, p) => println("%s \t%s".format(t, p)) }

    val output = tagger.tag(gold.map(_.map(_._1)))
    val results = new TaggerEvaluator().evaluate(output, gold, tagDict)
    assertResultsEqual("""
				Total:   94.02 (22517/23949)
				Known:   96.73 (21127/21841)
				Unknown: 65.94 (1390/2108)
				Common Mistakes:
				#Err     Gold      Model
				249      N        J
				228      V        N
				181      J        N
				131      N        V
				107      V        J
				""", results)
  }

  @Test
  def en_eisnerSmoothing_TagDictConstrainedTagging() {
    val train = TaggedFile("data/postag/english/entrain")
    val tagDict = new SimpleTagDictFactory().make(train)
    val gold = TaggedFile("data/postag/english/entest")
    //val gold = List(Vector(("The", "D"), ("<unknown>", "N"), ("runs", "V"), (".", ".")))

    LOG.debug("tagDictTrain.size = " + tagDict.setIterator.ungroup.size)
    LOG.debug("labeledTrain.size = " + train.size)
    LOG.debug("rawTrain.size     = " + 0)

    val trainer =
      new SupervisedHmmTaggerTrainer[String, String](
        transitionCountsTransformer =
          new TransitionCountsTransformer(
            EisnerSmoothingCondCountsTransformer(lambda = 1.0)),
        emissionCountsTransformer =
          new EmissionCountsTransformer(
            EisnerSmoothingCondCountsTransformer(lambda = 1.0, backoffCountsTransformer = AddLambdaSmoothingCountsTransformer(lambda = 1.0))),
        hmmTaggerFactory = new SimpleHmmTaggerFactory(tagDict.allTags))
    val tagger = trainer.trainSupervised(train)
    val constrainedTagger = new HardConstraintHmmTagger(tagger.transitions, tagger.emissions, OptionalTagDict(tagDict))

    val output = constrainedTagger.tag(gold.map(_.map(_._1)))
    val results = new TaggerEvaluator().evaluate(output, gold, tagDict)
    assertResultsEqual("""
				Total:   94.15 (22548/23949)
				Known:   96.86 (21155/21841)
				Unknown: 66.08 (1393/2108)
				Common Mistakes:
				#Err     Gold      Model
				244      N        J
				232      V        N
				178      J        N
				139      N        V
				91       V        J
				""", results)
  }

  @Test
  def en_eisnerSmoothing_TagDictConstrainedTraining() {
    val train = TaggedFile("data/postag/english/entrain")
    val tagDict = new SimpleTagDictFactory().make(train)
    val gold = TaggedFile("data/postag/english/entest")
    //val gold = List(Vector(("The", "D"), ("<unknown>", "N"), ("runs", "V"), (".", ".")))

    LOG.debug("tagDictTrain.size = " + tagDict.setIterator.ungroup.size)
    LOG.debug("labeledTrain.size = " + train.size)
    LOG.debug("rawTrain.size     = " + 0)

    val trainer =
      new SupervisedHmmTaggerTrainer[String, String](
        transitionCountsTransformer =
          new TransitionCountsTransformer(
            EisnerSmoothingCondCountsTransformer(lambda = 1.0)),
        emissionCountsTransformer =
          TagDictConstrainedEmissionCountsTransformer(tagDict,
            EisnerSmoothingCondCountsTransformer(lambda = 1.0, backoffCountsTransformer = AddLambdaSmoothingCountsTransformer(lambda = 1.0))),
        hmmTaggerFactory = new HardTagDictConstraintHmmTaggerFactory(OptionalTagDict(tagDict)))
    val tagger = trainer.trainSupervised(train)

    val output = tagger.tag(gold.map(_.map(_._1)))
    val results = new TaggerEvaluator().evaluate(output, gold, tagDict)
    assertResultsEqual("""
				Total:   94.11 (22538/23949)
				Known:   96.83 (21149/21841)
				Unknown: 65.89 (1389/2108)
				Common Mistakes:
				#Err     Gold      Model
				249      N        J
				233      V        N
				173      J        N
				142      N        V
				96       V        J
				""", results)
  }

  @Test
  def en_eisnerSmoothing_TagDictConstrainedTrainingAndTagging() {
    val train = TaggedFile("data/postag/english/entrain")
    val tagDict = new SimpleTagDictFactory().make(train)
    val gold = TaggedFile("data/postag/english/entest")
    //val gold = List(Vector(("The", "D"), ("<unknown>", "N"), ("runs", "V"), (".", ".")))

    LOG.debug("tagDictTrain.size = " + tagDict.setIterator.ungroup.size)
    LOG.debug("labeledTrain.size = " + train.size)
    LOG.debug("rawTrain.size     = " + 0)

    val trainer =
      new SupervisedHmmTaggerTrainer[String, String](
        transitionCountsTransformer =
          new TransitionCountsTransformer(
            EisnerSmoothingCondCountsTransformer(lambda = 1.0)),
        emissionCountsTransformer =
          TagDictConstrainedEmissionCountsTransformer(tagDict,
            EisnerSmoothingCondCountsTransformer(lambda = 1.0, backoffCountsTransformer = AddLambdaSmoothingCountsTransformer(lambda = 1.0))),
        hmmTaggerFactory = new HardTagDictConstraintHmmTaggerFactory(OptionalTagDict(tagDict)))
    val tagger = trainer.trainSupervised(train)
    val constrainedTagger = new HardConstraintHmmTagger(tagger.transitions, tagger.emissions, OptionalTagDict(tagDict))

    val output = constrainedTagger.tag(gold.map(_.map(_._1)))
    val results = new TaggerEvaluator().evaluate(output, gold, tagDict)
    assertResultsEqual("""
		        Total:   94.11 (22538/23949)
				Known:   96.83 (21149/21841)
				Unknown: 65.89 (1389/2108)
				Common Mistakes:
				#Err     Gold      Model
				249      N        J
				233      V        N
				173      J        N
				142      N        V
				96       V        J
				""", results)
  }

  object TaggedFile {
    def apply(filename: String) =
      FileUtils.readLines(filename)
        .map(_.trim)
        .split("###/###")
        .filter(_.nonEmpty)
        .map(_.map(_.split("/").toSeq match { case Seq(w, t) => (w, t) }).toIndexedSeq)
        .toList
  }

  object AsRawFile {
    def apply(filename: String) =
      TaggedFile(filename).map(_.map(_._1))
  }

  def assertResultsEqual(expectedString: String, results: ScoreResults[String, String]) {
    assertEquals(
      expectedString.split("\n").map(_.trim).filter(_.nonEmpty).mkString("\n"),
      results.toString.split("\n").map(_.trim).filter(_.nonEmpty).mkString("\n"))
  }
}

object SupervisedHmmTaggerTrainerTests {

  @BeforeClass def turnOffLogging() {
    Logger.getRootLogger.setLevel(Level.OFF)
  }

}
