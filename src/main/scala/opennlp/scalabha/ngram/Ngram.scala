package opennlp.scalabha.ngram

import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.util.Pattern.{ :+, +: }
import opennlp.scalabha.tag.support._
import opennlp.scalabha.util.LogNum
import scala.collection.immutable.TreeMap
import scala.annotation.tailrec

class Ngram[T](cfd: Seq[Option[T]] => MultinomialFreqDist[Option[T]], n: Int) {

  def sentenceProb(sentence: Seq[T]): LogNum = {
    liftedSeqProb(Seq.fill(n - 1)(None) ++ sentence.map(Option(_)) :+ None)
  }

  def seqProb(seq: Seq[T]): LogNum = {
    liftedSeqProb(seq.map(Option(_)))
  }

  def liftedSeqProb(seq: Seq[Option[T]]): LogNum = {
    seq
      .sliding(n)
      .map { case context :+ word => cfd(context.toSeq)(word) }
      .product
  }

  def generate(): List[T] = {
    @tailrec def inner(cur: Seq[Option[T]], accum: List[T]): List[T] = {
      cfd(cur).sample match {
        case None => accum
        case next => inner(cur.drop(1) :+ next, next.get :: accum)
      }
    }
    inner(Seq.fill(n - 1)(None), Nil).reverse
  }

}

class NgramTrainer[T](
  n: Int,
  countsTransformer: CondCountsTransformer[Seq[Option[T]], Option[T]]) {

  def train(sentences: TraversableOnce[Seq[T]]) = {
    new Ngram(CondFreqDist(
      countsTransformer(
        sentences
          .flatMap { sentence =>
            (Seq.fill(n - 1)(None) ++ sentence.map(Option(_)) :+ None)
              .sliding(n)
              .map { case context :+ word => context -> word }
          }
          .toIterator
          .groupByKey
          .mapValuesStrict(_.counts))), n)
  }
}

object Ngram {

  def main(args: Array[String]) {

    val N = 2
    val countsTransformer = PassthroughCondCountsTransformer[Seq[Option[String]], Option[String]]()
    //val countsTransformer = AddLambdaSmoothingCondCountsTransformer[Seq[Option[String]], Option[String]](0.01)
    val trainer = new NgramTrainer(N, countsTransformer)

    val filename = "data/postag/english/enraw20k"
    val lines = io.Source.fromFile(filename).getLines
    val sentences = lines.take(20000).split("###")
    val model = trainer.train(sentences)

    println("P('of the') = " + model.seqProb(List("of", "the")))   // LogNum(0.327944068166918)
    println("P('of a') = " + model.seqProb(List("of", "a")))       // LogNum(0.04391522831549001)
    println("P('of some') = " + model.seqProb(List("of", "some"))) // LogNum(2.18483722962637E-4)

    (1 to 10).par.foreach { _ => println(model.generate.mkString(" ")) }

  }

}
