package opennlp.scalabha.tag.support

import opennlp.scalabha.util.LogNum
import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.util.Pattern.{ +:, :+ }
import scala.util.Random
import scala.collection.immutable.TreeMap
import scala.collection.mutable.Builder
import scala.collection.generic.CanBuildFrom
import scala.collection.generic.ImmutableSortedMapFactory
import scala.collection.immutable.RedBlack
import opennlp.scalabha.tag.support.MultinomialFreqDist._

class MultinomialFreqDist[T](dist: Map[T, LogNum]) {

  lazy val sampler =
    RedBlackTree(
      dist.toList
        .scanLeft((None: Option[T], LogNum.zero)) { case ((ta, tb), (xa, xb)) => (Some(xa), tb + xb) }
        .collect { case (Some(x), p) => (x, p) }
        .map(_.swap): _*)

  lazy val random = new Random

  def sample(): T = {
    //List(.1, .6, .9).foreach(r => println(sampler.find(LogNum(r))))
    sampler.find(LogNum(random.nextDouble))
  }

}

object MultinomialFreqDist {
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

  class RedBlackTree[A, B](tree: RedBlack[A]#Tree[B])(implicit ord: Ordering[A]) {
    /*
       * Find the value of the smallest key greater than or equal to the search term
       */
    def find(searchKey: A): B = {
      def inner(searchKey: A, tree: RedBlack[A]#Tree[B]): Option[RedBlack[A]#Tree[B]] = tree match {
        case Tree(`searchKey`, value, left, right) =>
          Some(tree)
        case Tree(key, value, left @ Tree(lkey, lvalue, lleft, lright), right) =>
          if (ord.lt(searchKey, key))
            if (ord.lteq(searchKey, lkey))
              inner(searchKey, left)
            else
              Some(tree)
          else
            inner(searchKey, right)
        case Tree(key, value, _, _) => // Terminal node
          if (ord.lt(searchKey, key))
            Some(tree)
          else
            None
      }
      val Some(Tree(_, value, _, _)) = inner(searchKey, tree)
      value
    }
  }

  def main(args: Array[String]) {
    val probs = Map('N -> .5, 'V -> .3, 'D -> .2).mapValuesStrict(LogNum.apply)
    val dist = new MultinomialFreqDist(probs)
    println(dist.sample)
    println((1 to 100000).map(_ => dist.sample).counts.normalizeValues)
  }
}
