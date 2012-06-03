package opennlp.scalabha.tag.support

import opennlp.scalabha.util.LogNum
import opennlp.scalabha.util.LogNum._
import opennlp.scalabha.util.CollectionUtils._
import org.apache.commons.logging.LogFactory

class BinomialFreqDist[T](label0: T, label1: T, firstProb: LogNum)
  extends MultinomialFreqDist((List(label0, label1) zip List(firstProb, 1 - firstProb)).toMap, LogNum.zero) {

  override def sample(): T = {
    if (random.nextDouble.toLogNum < firstProb)
      label0
    else
      label1
  }

  override def toString = "BinomialFreqDist(%s, %s, %s)".format(label0, label1, firstProb.toDouble)
}

object BinomialFreqDist {
  def apply[T](label0: T, label1: T, firstProb: LogNum) = {
    new BinomialFreqDist[T](label0, label1, firstProb)
  }

  def apply[T](labels: Seq[T], firstProb: LogNum) = {
    require(labels.size == 2, "BinomialFreqDist must have exactly two labels")
    val Seq(l0, l1) = labels
    new BinomialFreqDist[T](l0, l1, firstProb)
  }

  def main(args: Array[String]) {
    val dist = new BinomialFreqDist("H", "T", 0.6.toLogNum)
    println(dist.sample)
    println((1 to 100000).map(_ => dist.sample).counts.normalizeValues)
  }

}

object BooleanFreqDist {
  def apply(propTrue: LogNum): BinomialFreqDist[Boolean] = BinomialFreqDist(true, false, propTrue)
  def apply(propTrue: Double): BinomialFreqDist[Boolean] = apply(propTrue.toLogNum)
}
