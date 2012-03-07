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
 */
case class DefaultedFreqCounts[B, N: Numeric](val counts: FreqCounts[B, N], val totalAddition: N, val defaultCount: N) {
  def ++(that: DefaultedFreqCounts[B, N]) = {
    if (totalAddition == implicitly[Numeric[N]].zero && defaultCount == implicitly[Numeric[N]].zero)
      new DefaultedFreqCounts(counts ++ that.counts, that.totalAddition, that.defaultCount)
    else if (that.totalAddition == implicitly[Numeric[N]].zero && that.defaultCount == implicitly[Numeric[N]].zero)
      new DefaultedFreqCounts(counts ++ that.counts, totalAddition, defaultCount)
    else {
      assert(totalAddition == that.totalAddition)
      assert(defaultCount == that.defaultCount)
      new DefaultedFreqCounts(counts ++ that.counts, totalAddition, defaultCount)
    }
  }

  def simpleCounts = counts
}

object DefaultedFreqCounts {
  def apply[B, N: Numeric](counts: FreqCounts[B, N]) =
    new DefaultedFreqCounts(counts, implicitly[Numeric[N]].zero, implicitly[Numeric[N]].zero)
  def apply[B, N: Numeric](counts: Map[B, N], totalAddition: N, defaultCount: N) =
    new DefaultedFreqCounts(FreqCounts(counts), totalAddition, defaultCount)
}

/**
 * Convenience wrapper of a map from items to their counts.  Its primary
 * function is to make adding counts easier.  The "++" operator adds the
 * counts for each respective entry (unlike Map's standard behavior).
 */
case class DefaultedCondFreqCounts[A, B, N: Numeric](val counts: Map[A, DefaultedFreqCounts[B, N]], val totalAddition: N, val defaultCount: N) {
  def ++(that: DefaultedCondFreqCounts[A, B, N]): DefaultedCondFreqCounts[A, B, N] = {
    if (totalAddition == implicitly[Numeric[N]].zero && defaultCount == implicitly[Numeric[N]].zero)
      add(that.counts, that.totalAddition, that.defaultCount)
    else if (that.totalAddition == implicitly[Numeric[N]].zero && that.defaultCount == implicitly[Numeric[N]].zero)
      add(that.counts, totalAddition, defaultCount)
    else {
      assert(totalAddition == that.totalAddition)
      assert(defaultCount == that.defaultCount)
      add(that.counts, totalAddition, defaultCount)
    }
  }

  private def add(thatCounts: Map[A, DefaultedFreqCounts[B, N]], totalAddition: N, defaultCount: N) = {
    val totalCounts = (counts.iterator ++ thatCounts).groupByKey.mapValuesStrict(_.reduce(_ ++ _))
    new DefaultedCondFreqCounts(totalCounts, totalAddition, defaultCount)
  }

  def simpleCounts = new CondFreqCounts(counts.mapValuesStrict(_.simpleCounts))
}

object DefaultedCondFreqCounts {
  def apply[A, B, N: Numeric](counts: CondFreqCounts[A, B, N]) =
    new DefaultedCondFreqCounts(counts.toMap.mapValuesStrict(c => DefaultedFreqCounts(FreqCounts(c))), implicitly[Numeric[N]].zero, implicitly[Numeric[N]].zero)
}
