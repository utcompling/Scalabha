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

class SupervisedHmmTaggerTrainerTests {

  @Test
  def supervised_training_tiny_noSmoothing() {
    val train = List(
      "the/D dog/N walks/V ./.",
      "the/D cat/N walks/V ./.",
      "a/D dog/N barks/V ./.")
      .map(_.split(" ").map(_.split("/").toSeq.toTuple2).toIndexedSeq)

    val trainer: SupervisedTaggerTrainer[String, String] =
      new SupervisedHmmTaggerTrainer(
        transitionCounterFactory = new CondFreqCounterFactory[String, String] { def get() = new SimpleCondFreqCounter() },
        emissionCounterFactory = new CondFreqCounterFactory[String, String] { def get() = new SimpleCondFreqCounter[String, String]() },
        "<END>", "<END>",
        new SimpleTagDictFactory())
    val tagger: Tagger[String, String] = trainer.trainSupervised(train)
    assertEquals(List("D", "N", "V", "."), tagger.tagSequence("the dog walks . ".split(" ")))
    assertEquals(List("D", "N", "V", "."), tagger.tagSequence("a cat walks . ".split(" ")))
  }

  @Test
  def supervised_training_tiny_smoothed() {
    val train = List(
      "the/D dog/N walks/V ./.",
      "the/D cat/N walks/V ./.",
      "a/D dog/N barks/V ./.")
      .map(_.split(" ").map(_.split("/").toSeq.toTuple2).toIndexedSeq)

    val endedTrain = train.map(s => (("<END>", "<END>") +: s :+ ("<END>", "<END>")))
    val tagBigrams = endedTrain.map(_.map(_._2).sliding2).flatten

    val trainer: SupervisedTaggerTrainer[String, String] =
      new SupervisedHmmTaggerTrainer(
        transitionCounterFactory = new CondFreqCounterFactory[String, String] {
          def get() =
            //TODO: data = tagBigrams
            new SimpleSmoothingTransitionFreqCounter(1.0, "<END>",
              Map(),
              List(),
              new SimpleCondFreqCounter())
        },
        emissionCounterFactory = new CondFreqCounterFactory[String, String] {
          def get() =
            // data = endedTrain.flatten.map(_.swap)
            new SimpleSmoothingEmissionFreqCounter(1.0, "<END>", "<END>",
              Map(),
              List(),
              new SimpleCondFreqCounter())
        },
        "<END>", "<END>",
        new SimpleTagDictFactory())
    val tagger: Tagger[String, String] = trainer.trainSupervised(train)
    assertEquals(List("D", "N", "V", "."), tagger.tagSequence("the dog walks . ".split(" ")))
    assertEquals(List("D", "N", "V", "."), tagger.tagSequence("a cat meows . ".split(" ")))
    assertEquals(List("D", "N", "V", "."), tagger.tagSequence("this bird chirps . ".split(" ")))
  }

  @Test
  def supervised_training_ic_noSmoothing() {
    val train = TaggedFile("data/postag/ic/ictrain.txt")

    val trainer: SupervisedTaggerTrainer[String, String] =
      new SupervisedHmmTaggerTrainer(
        transitionCounterFactory = new CondFreqCounterFactory[String, String] { def get() = new SimpleCondFreqCounter() },
        emissionCounterFactory = new CondFreqCounterFactory[String, String] { def get() = new SimpleCondFreqCounter[String, String]() },
        "<END>", "<END>",
        new SimpleTagDictFactory())
    val tagger = trainer.trainSupervised(train)

    val output = tagger.tag(AsRawFile("data/postag/ic/ictest.txt"))

    val gold = TaggedFile("data/postag/ic/ictest.txt")
    val results = new TaggerEvaluator().evaluate(output, gold, tagger.asInstanceOf[HmmTagger[String, String]].tagDict)
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
  def supervised_training_en_noSmoothing() {
    val train = TaggedFile("data/postag/english/entrain")

    val trainer: SupervisedTaggerTrainer[String, String] =
      new SupervisedHmmTaggerTrainer(
        transitionCounterFactory = new CondFreqCounterFactory[String, String] { def get() = new SimpleCondFreqCounter() },
        emissionCounterFactory = new CondFreqCounterFactory[String, String] { def get() = new SimpleCondFreqCounter[String, String]() },
        "<END>", "<END>",
        new SimpleTagDictFactory())
    val tagger = trainer.trainSupervised(train)

    val output = tagger.tag(AsRawFile("data/postag/english/entest"))

    val gold = TaggedFile("data/postag/english/entest")
    val results = new TaggerEvaluator().evaluate(output, gold, tagger.asInstanceOf[HmmTagger[String, String]].tagDict)
    assertResultsEqual("""
                Total:   80.91 (19377/23949)
                Known:   88.72 (19377/21841)
                Unknown: 0.00 (0/2108)
                Common Mistakes:
                #Err     Gold      Model
                1182     N        E       
                692      D        N       
                361      D        F       
                332      V        E       
                310      J        E       
                """, results)
  }

  @Test
  def supervised_training_en_smoothed() {
    val train = TaggedFile("data/postag/english/entrain")

    val endedTrain = train.map(s => (("<END>", "<END>") +: s :+ ("<END>", "<END>")))
    val tagBigrams = endedTrain.map(_.map(_._2).sliding2).flatten

    val trainer: SupervisedTaggerTrainer[String, String] =
      new SupervisedHmmTaggerTrainer(
        transitionCounterFactory = new CondFreqCounterFactory[String, String] {
          def get() =
            //TODO: data = tagBigrams
            new SimpleSmoothingTransitionFreqCounter(1.0, "<END>",
              Map(),
              List(),
              new SimpleCondFreqCounter())
        },
        emissionCounterFactory = new CondFreqCounterFactory[String, String] {
          def get() =
            // data = endedTrain.flatten.map(_.swap)
            new SimpleSmoothingEmissionFreqCounter(1.0, "<END>", "<END>",
              Map(),
              List(),
              new SimpleCondFreqCounter())
        },
        "<END>", "<END>",
        new SimpleTagDictFactory())
    val tagger = trainer.trainSupervised(train)

    val output = tagger.tag(AsRawFile("data/postag/english/entest"))

    val gold = TaggedFile("data/postag/english/entest")
    val results = new TaggerEvaluator().evaluate(output, gold, tagger.asInstanceOf[HmmTagger[String, String]].tagDict)
    assertResultsEqual("""
                Total:   94.13 (22544/23949)
                Known:   96.86 (21155/21841)
                Unknown: 65.89 (1389/2108)
                Common Mistakes:
                #Err     Gold      Model
                244      N        J       
                232      V        N       
                174      J        N       
                139      N        V       
                91       V        J       
                """, results)
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
