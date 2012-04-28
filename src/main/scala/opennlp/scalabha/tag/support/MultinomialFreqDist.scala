package opennlp.scalabha.tag.support

import opennlp.scalabha.util.LogNum
import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.util.Pattern.{ +:, :+ }
import scala.util.Random

class MultinomialFreqDist[T](dist: Map[T, LogNum]) {

  lazy val array = {
    (Vector() ++ dist)
      .sortBy(_._2).reverse
      .scanLeft((None: Option[T], LogNum.zero)) { case ((ta, tb), (xa, xb)) => (Some(xa), tb + xb) }
  }

  lazy val random = new Random

  def binarySearch(key: T) = {
    java.util.Arrays.binarySearch(array.asInstanceOf[Array[AnyRef]], key)
  }

  def sample(): T = {
    array.find(_._2 >= LogNum(random.nextDouble)).get._1.get
  }

}

object MutinomialFreqDist {
  def main(args: Array[String]) {
    val probs = Map('N -> .6, 'V -> .3, 'D -> .1).mapValuesStrict(LogNum.apply)
    val dist = new MultinomialFreqDist(probs)
    println(dist.sample)
  }
}
