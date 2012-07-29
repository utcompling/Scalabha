package opennlp.scalabha.tag.support

import opennlp.scalabha.util.LogNum
import opennlp.scalabha.util.LogNum._
import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.util.Pattern.{ +:, :+ }
import scala.annotation.tailrec
import scala.util.Random
import scala.collection.immutable.TreeMap
import scala.collection.mutable.Builder
import scala.collection.generic.CanBuildFrom
import scala.collection.generic.ImmutableSortedMapFactory
import scala.collection.immutable.RedBlack
import opennlp.scalabha.tag.support.MultinomialFreqDist._
import org.apache.commons.logging.LogFactory

class MultinomialFreqDist[T](val distValues: Iterable[(T, LogNum)], default: LogNum = LogNum.zero) extends DiscreteDistribution[T] {
  private val LOG = LogFactory.getLog(MultinomialFreqDist.getClass)

  val dist = distValues.toMap

  /*protected*/ lazy val sampler =
    RedBlackTree(
      distValues.toList.filter(_._2 > LogNum.zero)
        .scanLeft((None: Option[T], LogNum.zero)) { case ((ta, tb), (xa, xb)) => (Some(xa), tb + xb) }
        .collect { case (Some(x), p) => (x, p) }
        .map(_.swap): _*)
  /*protected*/ lazy val lastSampleKey = sampler.tree.last

  protected lazy val random = new Random

  override def apply(key: T) = dist.getOrElse(key, default)
  def iterator = distValues.iterator

  override def sample(): T = {
    sampler.find(random.nextDouble * lastSampleKey).get
  }

  override def toString = "MultinomialFreqDist(%s)".format(distValues)
}

object MultinomialFreqDist {
  def apply[T](dist: Iterable[(T, LogNum)], default: LogNum = LogNum.zero) = {
    new MultinomialFreqDist[T](dist, default)
  }

  object RedBlackTree {
    def apply[A, B](items: (A, B)*)(implicit ord: Ordering[A]) = {
      val redBlack = new TreeMap[A, B] ++ items
      val treeField = redBlack.getClass.getDeclaredField("tree")
      treeField.setAccessible(true)
      new RedBlackTree(treeField.get(redBlack).asInstanceOf[RedBlack[A]#Tree[B]])(ord)
    }
  }

  object Tree {
    def unapply[A, B](t: RedBlack[A]#Tree[B]) = t match {
      case n: RedBlack[A]#RedTree[B] => Some(n.key, n.value, n.left, n.right)
      case n: RedBlack[A]#BlackTree[B] => Some(n.key, n.value, n.left, n.right)
      case _ => None
    }
  }

  object Empty {
    def unapply[A, B](t: RedBlack[A]#Tree[B]) = t match {
      case n: RedBlack[A]#RedTree[B] => None
      case n: RedBlack[A]#BlackTree[B] => None
      case _ => Some()
    }
  }

  class RedBlackTree[A, B](val tree: RedBlack[A]#Tree[B])(implicit ord: Ordering[A]) {
    private val LOG = LogFactory.getLog(RedBlackTree.getClass)

    /*
     * Find the value of the smallest key greater than or equal to the search term
     */
    def find(searchKey: A): Option[B] = {
      @tailrec
      def inner(searchKey: A, tree: RedBlack[A]#Tree[B], bestSoFar: Option[B]): Option[B] = tree match {
        case Tree(`searchKey`, value, left, right) =>
          Some(value)
        case Tree(key, value, left @ Tree(lkey, lvalue, lleft, lright), right @ Tree(rkey, rvalue, rleft, rright)) =>
          if (ord.lt(searchKey, key))
            inner(searchKey, left, Some(value))
          else
            inner(searchKey, right, bestSoFar)
        case Tree(key, value, left @ Tree(lkey, lvalue, lleft, lright), _) =>
          if (ord.lteq(searchKey, lkey))
            inner(searchKey, left, Some(value))
          else
            Some(value)
        case Tree(key, value, _, right @ Tree(rkey, rvalue, rleft, rright)) =>
          if (ord.lt(searchKey, key))
            Some(value)
          else
            inner(searchKey, right, bestSoFar)
        case Tree(key, value, _, _) => // Terminal node
          if (ord.lt(searchKey, key))
            Some(value)
          else {
            bestSoFar
          }
      }
      inner(searchKey, tree, None)
    }
  }

  def main(args: Array[String]) {
    {
      val values = (1 to 10).mapTo(i => 1.toLogNum)
      val dist = new MultinomialFreqDist(values)
      val sampler = dist.sampler
      for (x <- (0 to 10))
        println(sampler.find(x + LogNum(.5)))
    }
    {
      val probs = Map('N -> .5, 'V -> .3, 'D -> .2).mapVals(_.toLogNum)
      val dist = new MultinomialFreqDist(probs)
      println((1 to 100000).map(_ => dist.sample).counts.normalizeValues.toSeq.sortBy(-_._2))
    }
    {
      val probs = { var i = .5; (1 to 10).mapToVal({ i *= 2; 1 / i }).normalizeValues }.toList
      println(probs)
      val dist = new MultinomialFreqDist(probs.toMap.mapVals(_.toLogNum))
      println((1 to 100000).map(_ => dist.sample).counts.normalizeValues.toList.sortBy(-_._2))
    }
  }
}
