package opennlp.scalabha.tag.support

import opennlp.scalabha.util.CollectionUtils._
import scala.collection.immutable.MapProxy
import scala.collection.generic.CanBuildFrom
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.MapBuilder
import scala.collection.GenTraversableOnce
import scala.collection.immutable.SetProxy
import scala.collection.GenTraversable

/**
 * Convenience wrapper of a map from items to their counts.  Its primary
 * function is to make adding counts easier.  The "++" operator adds the
 * counts for each respective entry (unlike Map's standard behavior).
 *
 * @tparam B	the item being counted
 * @tparam N	the Numeric type of the count
 */
class FreqCounts[B, N: Numeric](private val self: Map[B, N]) {
  def ++(that: FreqCounts[B, N]) = new FreqCounts((self.iterator ++ that.iterator).groupByKey.mapValuesStrict(_.sum))
  def --(bs: TraversableOnce[B]) = new FreqCounts(self -- bs)
  def -(b: B) = new FreqCounts(self - b)
  def iterator = self.iterator
  def toMap = self
  override def toString = "FreqCounts(%s)".format(self)
}

object FreqCounts {
  def apply[B, N: Numeric]() = new FreqCounts[B, N](Map())
  def apply[B, N: Numeric](self: Map[B, N]) = new FreqCounts(self)
}

/**
 * Convenience wrapper of a map from item pairs to their counts.  Its primary
 * function is to make adding counts easier.  The "++" operator adds the
 * counts for each respective entry (unlike Map's standard behavior).
 *
 * @tparam A	the conditioning item being counted; P(B|A).
 * @tparam B	the conditioned item being counted; P(B|A).
 * @tparam N	the Numeric type of the count
 */
class CondFreqCounts[A, B, N: Numeric](private val self: Map[A, FreqCounts[B, N]]) {
  def ++(that: CondFreqCounts[A, B, N]) =
    new CondFreqCounts((self.iterator ++ that.iterator).groupByKey.mapValuesStrict(_.reduce(_ ++ _)))
  def iterator = self.iterator
  def map[C](f: ((A, FreqCounts[B, N])) => C) = iterator.map(f)
  def values = iterator.map(_._2)
  def toMap = self.mapValuesStrict(_.toMap)
  def toDouble = CondFreqCounts(self.mapValuesStrict(_.toMap.mapValuesStrict(implicitly[Numeric[N]].toDouble(_))))
  override def toString = "CondFreqCounts(%s)".format(self.mapValues(_.toMap))
}

object CondFreqCounts {
  def apply[A, B, N: Numeric]() = new CondFreqCounts[A, B, N](Map())
  def apply[A, B, N: Numeric](self: Map[A, Map[B, N]]) = new CondFreqCounts(self.mapValuesStrict(FreqCounts(_)))
}
