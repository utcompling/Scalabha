package opennlp.scalabha.tag.support

import scala.util.Random

import org.apache.commons.logging.LogFactory

import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.util.LogNum
import opennlp.scalabha.util.LogNum._

class MultinomialFreqDist[T](val distValues: Iterable[(T, LogNum)], default: LogNum = LogNum.zero) extends DiscreteDistribution[T] {
  private val LOG = LogFactory.getLog(MultinomialFreqDist.getClass)

  val dist = distValues.toMap

  /*protected*/ lazy val (sampler, lastSampleKey) = {
    val vs = distValues.toIndexedSeq.filter(_._2 > LogNum.zero).sortBy(_._2).reverse
    var running = LogNum.zero
    val xs =
      for ((v, n) <- vs) yield {
        running += n
        v -> running
      }
    (xs, running)
  }

  protected lazy val random = new Random

  override def apply(key: T) = dist.getOrElse(key, default)
  def iterator = distValues.iterator

  /*protected*/ def findSample(key: LogNum) = sampler.find(_._2 >= key)

  override def sample(): T = {
    findSample(lastSampleKey * random.nextDouble) match {
      case Some((t, _)) => t
      case None => throw new RuntimeException("Could not sample from: " + { val s = distValues.toString; if (s.length <= 50) s else s.take(47) + "..." })
    }
  }

  override def toString = "MultinomialFreqDist(%s)".format(distValues)
}

object MultinomialFreqDist {
  def apply[T](dist: Iterable[(T, LogNum)], default: LogNum = LogNum.zero) = {
    new MultinomialFreqDist[T](dist, default)
  }

  def main(args: Array[String]) {
    {
      val values = (1 to 10).mapTo(i => 1.toLogNum)
      val dist = new MultinomialFreqDist(values)
      val sampler = dist.sampler
      println(sampler)
      println(dist.lastSampleKey)
      for (x <- (0 to 10))
        println(dist.findSample(x + LogNum(.5)))
    }
    {
      val probs = Map('N -> .5, 'V -> .3, 'D -> .2).mapVals(_.toLogNum)
      val dist = new MultinomialFreqDist(probs)
      println((1 to 100000).map(_ => dist.sample).counts.normalizeValues.toVector.sortBy(-_._2))
    }
    {
      val probs = { var i = .5; (1 to 100).mapToVal({ i *= 2; 1 / i }).normalizeValues }.toIndexedSeq
      println(probs)
      val dist = new MultinomialFreqDist(probs.toMap.mapVals(_.toLogNum))
      println((1 to 100000).map(_ => dist.sample).counts.normalizeValues.toVector.sortBy(-_._2))
    }
  }
}
