package opennlp.scalabha.ngram

import scala.annotation.tailrec
import scala.collection.generic.CanBuildFrom
import opennlp.scalabha.tag.support.CondCountsTransformer
import opennlp.scalabha.tag.support.CondFreqDist
import opennlp.scalabha.tag.support.MultinomialFreqDist
import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.util.CollectionUtil._
import opennlp.scalabha.util.LogNum
import opennlp.scalabha.util.Pattern.{ :+ }
import opennlp.scalabha.tag.support.PassthroughCondCountsTransformer

class Ngram[T, S](
  val n: Int,
  val cfd: Seq[Option[T]] => MultinomialFreqDist[Option[T]]) {

  /**
   * Calculate the probability of the given COMPLETE sequence.  This method
   * appends start and end symbols to the given sequence before calculating
   * the probability.
   *
   * The given sequence can be any length.
   */
  def sentenceProb(sentence: Seq[T]): LogNum = {
    liftedSeqProb(Vector.fill(n - 1)(None) ++ sentence.map(Option(_)) :+ None)
  }

  /**
   * Calculate the probability of the given SUB-sequence.  This method
   * DOES NOT append start or end symbols to the sequence before calculating
   * the probability.
   *
   * The given sequence must be at least N items in length.
   */
  def seqProb(seq: Seq[T]): LogNum = {
    liftedSeqProb(seq.map(Option(_)))
  }

  /**
   * Calculate the probability of the given SUB-sequence.  This method
   * DOES NOT append start or end symbols to the sequence before calculating
   * the probability, but it DOES allow start or end symbols (None) to be
   * included in the given sequence.
   *
   * The given sequence must be at least N items in length.
   */
  def liftedSeqProb(seq: Seq[Option[T]]): LogNum = {
    require(seq.length >= n, "seq must have length at least N=%s".format(n))
    seq
      .sliding(n)
      .map { case context :+ word => cfd(context.toIndexedSeq)(word) }
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
    inner(Vector.fill(n - 1)(None))
    b.result
  }

}

object Ngram {
  def apply[T](n: Int, cfd: Seq[Option[T]] => MultinomialFreqDist[Option[T]]) = new Ngram[T, Seq[T]](n, cfd)
  def unapply[T, S](ngram: Ngram[T, S]) = Some(ngram.n, ngram.cfd)
}

case class NgramTrainer[T](
  n: Int,
  countsTransformer: CondCountsTransformer[Seq[Option[T]], Option[T]] = PassthroughCondCountsTransformer[Seq[Option[T]], Option[T]]()) {

  def apply[S <% Seq[T]](sentences: TraversableOnce[S]): Ngram[T, S] = {
    new Ngram[T, S](n, CondFreqDist(
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
