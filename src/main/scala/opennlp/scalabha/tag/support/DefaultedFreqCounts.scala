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
case class DefaultedFreqCounts[B, N: Numeric](val counts: Map[B, N], val totalAddition: N, val defaultCount: N) {
  def ++(that: DefaultedFreqCounts[B, N]) = {
    val countSum = (counts.iterator ++ that.counts).groupByKey.mapValuesStrict(_.sum)

    if (totalAddition == implicitly[Numeric[N]].zero && defaultCount == implicitly[Numeric[N]].zero)
      new DefaultedFreqCounts(countSum, that.totalAddition, that.defaultCount)

    else if (that.totalAddition == implicitly[Numeric[N]].zero && that.defaultCount == implicitly[Numeric[N]].zero)
      new DefaultedFreqCounts(countSum, totalAddition, defaultCount)

    else {
      assert(totalAddition == that.totalAddition)
      assert(defaultCount == that.defaultCount)
      new DefaultedFreqCounts(countSum, totalAddition, defaultCount)
    }
  }

  def simpleCounts = counts
}

object DefaultedFreqCounts {
  def apply[B, N: Numeric](counts: Map[B, N]) =
    new DefaultedFreqCounts(counts, implicitly[Numeric[N]].zero, implicitly[Numeric[N]].zero)
}

/**
 * Convenience wrapper of a map from items to their counts.  Its primary
 * function is to make adding counts easier.  The "++" operator adds the
 * counts for each respective entry (unlike Map's standard behavior).
 *
 * @tparam A	the conditioning item being counted; P(B|A).
 * @tparam B	the conditioned item being counted; P(B|A).
 * @tparam N	the Numeric type of the count
 */
class DefaultedCondFreqCounts[A, B, N: Numeric](val counts: Map[A, DefaultedFreqCounts[B, N]]) {
  def ++(that: DefaultedCondFreqCounts[A, B, N]): DefaultedCondFreqCounts[A, B, N] = {
    new DefaultedCondFreqCounts((counts.iterator ++ that.counts).groupByKey.mapValuesStrict(_.reduce(_ ++ _)))
  }

  def simpleCounts = counts.mapValuesStrict(_.simpleCounts)
}

object DefaultedCondFreqCounts {
  def apply[A, B, N: Numeric](counts: Map[A, Map[B, N]]) =
    new DefaultedCondFreqCounts(counts.mapValuesStrict(c => DefaultedFreqCounts(c)))
}
