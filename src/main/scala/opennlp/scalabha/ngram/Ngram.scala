package opennlp.scalabha.ngram

import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.util.Pattern.{ :+, +: }
import opennlp.scalabha.tag.support._
import opennlp.scalabha.util.LogNum
import scala.collection.immutable.TreeMap
import scala.annotation.tailrec
import scala.collection.generic.CanBuildFrom

trait Ngram[T, S] {
  def n: Int
  def cfd: Seq[Option[T]] => MultinomialFreqDist[Option[T]]

  /**
   * Calculate the probability of the given COMPLETE sequence.  This method
   * appends start and end symbols to the given sequence before calculating
   * the probability.
   */
  def sentenceProb(sentence: Seq[T]): LogNum = {
    liftedSeqProb(Seq.fill(n - 1)(None) ++ sentence.map(Option(_)) :+ None)
  }

  /**
   * Calculate the probability of the given SUB-sequence.  This method
   * DOES NOT append start or end symbols to the sequence before calculating
   * the probability.
   */
  def seqProb(seq: Seq[T]): LogNum = {
    liftedSeqProb(seq.map(Option(_)))
  }

  /**
   * Calculate the probability of the given SUB-sequence.  This method
   * DOES NOT append start or end symbols to the sequence before calculating
   * the probability, but it DOES allow start or end symbols (None) to be
   * included in the given sequence
   */
  def liftedSeqProb(seq: Seq[Option[T]]): LogNum = {
    seq
      .sliding(n)
      .map { case context :+ word => cfd(context.toSeq)(word) }
      .product
  }

  /**
   * Generate a random complete sequence based on this n-gram model.
   */
  def generate(implicit bf: CanBuildFrom[S, T, S]): S = {
    val b = bf()
    @tailrec def inner(cur: Seq[Option[T]]) {
      cfd(cur).sample match {
        case None =>
        case next =>
          b += next.get
          inner(cur.drop(1) :+ next)
      }
    }
    inner(Seq.fill(n - 1)(None))
    b.result
  }

}

case class NgramTrainer[T](
  n: Int,
  countsTransformer: CondCountsTransformer[Seq[Option[T]], Option[T]]) {

  def apply[S <% Seq[T]](sentences: TraversableOnce[S]): Ngram[T, S] = {
    new Ngram.ImplicitGeneratingNgram[T, S](n, CondFreqDist(
      countsTransformer(
        sentences
          .flatMap { sentence =>
            (Seq.fill(n - 1)(None) ++ sentence.map(Option(_)) :+ None)
              .sliding(n)
              .map { case context :+ word => context -> word }
          }
          .toIterator
          .groupByKey
          .mapVals(_.counts))))
  }
}

object Ngram {
  class SeqGeneratingNgram[T](override val n: Int, override val cfd: Seq[Option[T]] => MultinomialFreqDist[Option[T]]) extends Ngram[T, Seq[T]]
  class ImplicitGeneratingNgram[T, S](override val n: Int, override val cfd: Seq[Option[T]] => MultinomialFreqDist[Option[T]]) extends Ngram[T, S]

  def apply[T](n: Int, cfd: Seq[Option[T]] => MultinomialFreqDist[Option[T]]) = new SeqGeneratingNgram[T](n, cfd)
  def unapply[T, S](ngram: Ngram[T, S]) = Some(ngram.n, ngram.cfd)

  def main(args: Array[String]) {

    {
      val N = 2
      val countsTransformer = PassthroughCondCountsTransformer[Seq[Option[String]], Option[String]]()
      //val countsTransformer = AddLambdaSmoothingCondCountsTransformer[Seq[Option[String]], Option[String]](0.01)
      val trainer = new NgramTrainer(N, countsTransformer)

      val filename = "data/postag/english/enraw20k"
      val lines = io.Source.fromFile(filename).getLines
      val sentences = lines.take(20000).split("###")
      val model = trainer(sentences)

      println("P('of the') = " + model.seqProb(List("of", "the"))) // LogNum(0.327944068166918)
      println("P('of a') = " + model.seqProb(List("of", "a"))) // LogNum(0.04391522831549001)
      println("P('of some') = " + model.seqProb(List("of", "some"))) // LogNum(2.18483722962637E-4)

      (1 to 10).par.foreach { _ => println(model.generate.mkString(" ")) }

      val Ngram(n, cfd) = model
      val model2 = Ngram(n, cfd)
      model2.generate
    }

    {
      val N = 4
      val countsTransformer = PassthroughCondCountsTransformer[Seq[Option[Char]], Option[Char]]()
      //val countsTransformer = UnseenContextProbSettingCondCountsTransformer[Seq[Option[Char]], Option[Char]](LogNum(.05), AddLambdaSmoothingCondCountsTransformer(1.))
      val trainer = new NgramTrainer(N, countsTransformer)

      val filename = "data/postag/english/enraw20k"
      val lines = io.Source.fromFile(filename).getLines
      val sentences = lines.take(20000).split("###")
      val LettersRe = """([a-z]+)""".r
      val words = sentences.flatten.collect { case LettersRe(w) => w }
      val model = trainer(words)

      (1 to 50).foreach { _ => println(model.generate) }

      val Ngram(n, cfd) = model
      val model2 = Ngram(n, cfd)
      model2.generate
    }

  }

}
