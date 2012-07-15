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

class SupervisedHmmTaggerTrainerTests {
  val LOG = LogFactory.getLog(classOf[UnsupervisedHmmTaggerTrainerTests])

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
        transitionCountsTransformer = TransitionCountsTransformer(tagDict),
        emissionCountsTransformer = EmissionCountsTransformer())
    val tagger: Tagger[String, String] = trainer.trainSupervised(train, tagDict.allTags)
    assertEquals(List("D", "N", "V", "."), tagger.tagSequence("the dog walks . ".split(" ")))
    assertEquals(List("D", "N", "V", "."), tagger.tagSequence("a cat walks . ".split(" ")))
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
          new TransitionCountsTransformer(tagDict,
            EisnerSmoothingCondCountsTransformer(lambda = 1.0)),
        emissionCountsTransformer =
          new EmissionCountsTransformer(
            EisnerSmoothingCondCountsTransformer(lambda = 1.0, backoffCountsTransformer = AddLambdaSmoothingCountsTransformer(lambda = 1.0))))
    val tagger: Tagger[String, String] = trainer.trainSupervised(train, tagDict.allTags)
    assertEquals(List("D", "N", "V", "."), tagger.tagSequence("the dog walks . ".split(" ")))
    assertEquals(List("D", "N", "V", "."), tagger.tagSequence("a cat meows . ".split(" ")))
    assertEquals(List("D", "N", "V", "."), tagger.tagSequence("this bird chirps . ".split(" ")))
  }

  @Test
  def ic_noSmoothing() {
    val train = TaggedFile("data/postag/ic/ictrain.txt")

    val tagDict = new SimpleTagDictFactory().make(train)
    val trainer: SupervisedTaggerTrainer[String, String] =
      new SupervisedHmmTaggerTrainer(
        transitionCountsTransformer = TransitionCountsTransformer(tagDict),
        emissionCountsTransformer = EmissionCountsTransformer())
    val tagger: Tagger[String, String] = trainer.trainSupervised(train, tagDict.allTags)

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
        transitionCountsTransformer = TransitionCountsTransformer(tagDict),
        emissionCountsTransformer = EmissionCountsTransformer())
    val tagger: Tagger[String, String] = trainer.trainSupervised(train, tagDict.allTags)

    val output = tagger.tag(AsRawFile("data/postag/english/entest"))

    val gold = TaggedFile("data/postag/english/entest")
    val results = new TaggerEvaluator().evaluate(output, gold, tagDict)
    assertResultsEqual("""
                Total:   80.91 (19377/23949)
                Known:   88.72 (19377/21841)
                Unknown: 0.00 (0/2108)
                Common Mistakes:
                #Err     Gold      Model
                1182     N        .       
                692      D        N       
                361      D        F       
                332      V        .       
                310      J        .       
                """, results)
  }

  @Test
  def en_eisnerSmoothing() {
    val train = TaggedFile("data/postag/english/entrain")
    val tagDict = new SimpleTagDictFactory().make(train)
    val gold = TaggedFile("data/postag/english/entest")

    LOG.debug("tagDictTrain.size = " + tagDict.iterator.ungroup.size)
    LOG.debug("labeledTrain.size = " + train.size)
    LOG.debug("rawTrain.size     = " + 0)

    val trainer =
      new SupervisedHmmTaggerTrainer[String, String](
        transitionCountsTransformer =
          new TransitionCountsTransformer(tagDict,
            EisnerSmoothingCondCountsTransformer(lambda = 1.0)),
        emissionCountsTransformer =
          new EmissionCountsTransformer(
            EisnerSmoothingCondCountsTransformer(lambda = 1.0, backoffCountsTransformer = AddLambdaSmoothingCountsTransformer(lambda = 1.0))))
    val tagger = trainer.trainSupervised(train, tagDict.allTags)

    val output = tagger.tag(gold.map(_.map(_._1)))
    val results = new TaggerEvaluator().evaluate(output, gold, tagDict)
    assertResultsEqual("""
				Total:   91.54 (21924/23949)
				Known:   96.33 (21039/21841)
				Unknown: 41.98 (885/2108)
				Common Mistakes:
				#Err     Gold      Model
				152      V        N
				150      N        J
				142      N        D
				141      N        V
				121      J        N
				""", results)
  }

  @Test
  def en_eisnerSmoothing_TagDictConstrained() {
    val train = TaggedFile("data/postag/english/entrain")
    val tagDict = new SimpleTagDictFactory().make(train)
    val gold = TaggedFile("data/postag/english/entest")

    LOG.debug("tagDictTrain.size = " + tagDict.iterator.ungroup.size)
    LOG.debug("labeledTrain.size = " + train.size)
    LOG.debug("rawTrain.size     = " + 0)

    val trainer =
      new SupervisedHmmTaggerTrainer[String, String](
        transitionCountsTransformer =
          new TransitionCountsTransformer(tagDict,
            EisnerSmoothingCondCountsTransformer(lambda = 1.0)),
        emissionCountsTransformer =
          TagDictConstrainedEmissionCountsTransformer(tagDict, allowUnseenWordTypes = true,
            EisnerSmoothingCondCountsTransformer(lambda = 1.0,
              backoffCountsTransformer = AddLambdaSmoothingCountsTransformer(lambda = 1.0))))
    val tagger = trainer.trainSupervised(train, tagDict.allTags)

    val output = tagger.tag(gold.map(_.map(_._1)))
    val results = new TaggerEvaluator().evaluate(output, gold, tagDict)
    assertResultsEqual("""
				Total:   91.54 (21924/23949)
				Known:   96.33 (21039/21841)
				Unknown: 41.98 (885/2108)
				Common Mistakes:
				#Err     Gold      Model
				152      V        N
				150      N        J
				142      N        D
				141      N        V
				121      J        N
				""", results)
  }

  object TaggedFile {
    def apply(filename: String) =
      Source.fromFile(filename).getLines
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
