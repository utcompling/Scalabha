package opennlp.scalabha.ngram

import org.junit.Assert._
import org.junit._
import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.util.Pattern.{ :+ }
import opennlp.scalabha.util.LogNum
import opennlp.scalabha.tag.support.PassthroughCondCountsTransformer
import opennlp.scalabha.tag.support.UnseenContextProbSettingCondCountsTransformer
import opennlp.scalabha.tag.support.AddLambdaSmoothingCondCountsTransformer
import opennlp.scalabha.tag.support.PassthroughCountsTransformer
import opennlp.scalabha.test.TestUtils._

class NgramTests {

  @Test
  def test_unsmoothed() {
    val ngram =
      NgramTrainer[Char](3)
        .apply("that dog hates the cat . . . . . . . .".split(" "))

    //    val c =
    //      "that dog hates the cat . . . . . . . .".split(" ").map(_.toSeq).toSeq
    //        .flatMap { sentence =>
    //          (Seq.fill(2)(None) ++ sentence.map(Option(_)) :+ None)
    //            .sliding(3)
    //            .map { case context :+ word => context -> word }
    //        }
    //        .groupByKey
    //        .mapVals(_.counts)
    //
    //    val contextCounts = c.ungroup.toSeq.map { case (a, (b, c)) => (a.map { case Some(c) => c; case None => '_' }.mkString(""), c) }.groupByKey.mapVals(_.sum)
    //    val counts = c.ungroup.toSeq.map { case (a, (b, c)) => ((a :+ b).map { case Some(c) => c; case None => '_' }.mkString(""), c) }.sorted
    //    counts.foreach { case (a, c) => println("%s %s %s".format(a, c, contextCounts(a.take(2)))) }

    assertEqualsProb(LogNum(2 / 2.), ngram.liftedSeqProb(Seq(None, Some('t'), Some('h'))))
    assertEqualsProb(LogNum(1 / 2.), ngram.liftedSeqProb(Seq(Some('t'), Some('h'), Some('e'))))
    assertEqualsProb(LogNum(2 / 13.), ngram.liftedSeqProb(Seq(None, None, Some('t'))))
    assertEqualsProb(LogNum(1 / 13.), ngram.liftedSeqProb(Seq(None, None, Some('c'))))
    assertEqualsProb(LogNum(1 / 1.), ngram.liftedSeqProb(Seq(Some('e'), Some('s'), None)))
    assertEqualsProb(LogNum(2 / 3.), ngram.liftedSeqProb(Seq(Some('a'), Some('t'), None)))
    assertEqualsProb(LogNum(0 / 3.), ngram.liftedSeqProb(Seq(Some('a'), Some('t'), Some('a'))))
    assertEqualsProb(LogNum(0 / 3.), ngram.liftedSeqProb(Seq(Some('a'), Some('t'), Some('z'))))
    assertEqualsProb(LogNum(0.), ngram.liftedSeqProb(Seq(Some('z'), Some('z'), Some('a'))))

    assertEqualsProb(LogNum((1 / 13.) * (1 / 1.) * (1 / 1.) * (1 / 1.)), ngram.sentenceProb("dog"))
    assertEqualsProb(LogNum((2 / 13.) * (2 / 2.) * (1 / 2.) * (1 / 1.)), ngram.sentenceProb("the"))
    assertEqualsProb(LogNum((2 / 13.) * (2 / 2.) * (1 / 2.) * (2 / 2.) * (2 / 3.)), ngram.sentenceProb("that"))
    assertEqualsProb(LogNum((1 / 13.) * (1 / 1.) * (2 / 2.) * (1 / 3.) * (0 / 1.)), ngram.sentenceProb("hate"))
    assertEqualsProb(LogNum((8 / 13.) * (8 / 8.)), ngram.sentenceProb("."))
    assertEqualsProb(LogNum((0 / 13.) * (0.) * (2 / 3.)), ngram.sentenceProb("at"))

    assertException(ngram.seqProb("x")) { case e: IllegalArgumentException => assertEquals("requirement failed: seq must have length at least N=3", e.getMessage) }
    assertException(ngram.liftedSeqProb(Seq(Option('x')))) { case e: IllegalArgumentException => assertEquals("requirement failed: seq must have length at least N=3", e.getMessage) }
  }

  @Test
  def test_smoothed() {
    val counter =
      UnseenContextProbSettingCondCountsTransformer[Seq[Option[Char]], Option[Char]](LogNum(.05),
        AddLambdaSmoothingCondCountsTransformer(1.))
    val ngram =
      NgramTrainer[Char](3, counter)
        .apply("that dog hates the cat . . . . . . . .".split(" "))

    //    val c =
    //      "that dog hates the cat . . . . . . . .".split(" ").map(_.toSeq).toSeq
    //        .flatMap { sentence =>
    //          (Seq.fill(2)(None) ++ sentence.map(Option(_)) :+ None)
    //            .sliding(3)
    //            .map { case context :+ word => context -> word }
    //        }
    //        .groupByKey
    //        .mapVals(_.counts)
    //    val tc = counter(c).simpleCounts
    //
    //    val contextCounts = tc.ungroup.toSeq.map { case (a, (b, c)) => (a.map { case Some(c) => c; case None => '_' }.mkString(""), c) }.groupByKey.mapVals(_.sum)
    //    val counts = tc.ungroup.toSeq.map { case (a, (b, c)) => ((a :+ b).map { case Some(c) => c; case None => '_' }.mkString(""), c) }.sorted
    //    counts.foreach { case (a, c) => println("%s %s %s".format(a, c, contextCounts(a.take(2)) + 1)) }

    assertEqualsProb(LogNum(3 / 14.), ngram.liftedSeqProb(Seq(None, Some('t'), Some('h'))))
    assertEqualsProb(LogNum(2 / 14.), ngram.liftedSeqProb(Seq(Some('t'), Some('h'), Some('e'))))
    assertEqualsProb(LogNum(3 / 25.), ngram.liftedSeqProb(Seq(None, None, Some('t'))))
    assertEqualsProb(LogNum(2 / 25.), ngram.liftedSeqProb(Seq(None, None, Some('c'))))
    assertEqualsProb(LogNum(2 / 13.), ngram.liftedSeqProb(Seq(Some('e'), Some('s'), None)))
    assertEqualsProb(LogNum(3 / 15.), ngram.liftedSeqProb(Seq(Some('a'), Some('t'), None)))
    assertEqualsProb(LogNum(1 / 15.), ngram.liftedSeqProb(Seq(Some('a'), Some('t'), Some('a'))))
    assertEqualsProb(LogNum(1 / 15.), ngram.liftedSeqProb(Seq(Some('a'), Some('t'), Some('z'))))
    assertEqualsProb(LogNum(.05), ngram.liftedSeqProb(Seq(Some('z'), Some('z'), Some('a'))))

    assertEqualsProb(LogNum((2 / 25.) * (2 / 13.) * (2 / 13.) * (2 / 13.)), ngram.sentenceProb("dog"))
    assertEqualsProb(LogNum((3 / 25.) * (3 / 14.) * (2 / 14.) * (2 / 13.)), ngram.sentenceProb("the"))
    assertEqualsProb(LogNum((3 / 25.) * (3 / 14.) * (2 / 14.) * (3 / 14.) * (3 / 15.)), ngram.sentenceProb("that"))
    assertEqualsProb(LogNum((2 / 25.) * (2 / 13.) * (3 / 14.) * (2 / 15.) * (1 / 13.)), ngram.sentenceProb("hate"))
    assertEqualsProb(LogNum((9 / 25.) * (9 / 20.)), ngram.sentenceProb("."))
    assertEqualsProb(LogNum((1 / 25.) * (.05) * (3 / 15.)), ngram.sentenceProb("at"))

    assertException(ngram.seqProb("x")) { case e: IllegalArgumentException => assertEquals("requirement failed: seq must have length at least N=3", e.getMessage) }
    assertException(ngram.liftedSeqProb(Seq(Option('x')))) { case e: IllegalArgumentException => assertEquals("requirement failed: seq must have length at least N=3", e.getMessage) }
  }

  @Test
  def test_main_wordNgrams() {
    val N = 2
    val countsTransformer = PassthroughCondCountsTransformer[Seq[Option[String]], Option[String]]()
    //val countsTransformer = AddLambdaSmoothingCondCountsTransformer[Seq[Option[String]], Option[String]](0.01)
    val trainer = NgramTrainer(N, countsTransformer)

    val filename = "data/postag/english/enraw20k"
    val lines = io.Source.fromFile(filename).getLines
    val sentences = lines.take(20000).split("###")
    val model = trainer(sentences)

    (1 to 10).par.foreach { _ => println(model.generate.mkString(" ")) }

    val Ngram(n, cfd) = model
    val model2 = Ngram(n, cfd)
    model2.generate
  }

  @Test
  def test_main_letterNgrams() {
    val N = 4
    val countsTransformer = PassthroughCondCountsTransformer[Seq[Option[Char]], Option[Char]]()
    //val countsTransformer = UnseenContextProbSettingCondCountsTransformer[Seq[Option[Char]], Option[Char]](LogNum(.05), AddLambdaSmoothingCondCountsTransformer(1.))
    val trainer = NgramTrainer(N, countsTransformer)

    val filename = "data/postag/english/enraw20k"
    val lines = io.Source.fromFile(filename).getLines
    val sentences = lines.take(20000).split("###")
    val LettersRe = """([a-z]+)""".r
    val words = sentences.flatten.collect { case LettersRe(w) => w }
    val model = trainer(words)

    (1 to 20).foreach { _ => println(model.generate) }

    val Ngram(n, cfd) = model
    val model2 = Ngram(n, cfd)
    model2.generate
  }

}
