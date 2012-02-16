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
abstract class DefaultedFreqCounts[B, N: Numeric] {
  def ++(that: DefaultedFreqCounts[B, N]): DefaultedFreqCounts[B, N] =
    new CompositeDefaultedFreqCounts(this, that)

}

class BaseDefaultedFreqCounts[B, N: Numeric](private val counts: FreqCounts[B, N], private val totalAddition: N, private val defaultCount: N) extends DefaultedFreqCounts[B, N] {
  override def ++(that: DefaultedFreqCounts[B, N]) = {
    that match {
      case base: BaseDefaultedFreqCounts[B, N] =>
        if (totalAddition == implicitly[Numeric[N]].zero && defaultCount == implicitly[Numeric[N]].zero)
          new BaseDefaultedFreqCounts(counts ++ base.counts, base.totalAddition, base.defaultCount)
        else if (base.totalAddition == implicitly[Numeric[N]].zero && base.defaultCount == implicitly[Numeric[N]].zero)
          new BaseDefaultedFreqCounts(counts ++ base.counts, totalAddition, defaultCount)
        else
          super.++(that)
      case _ => super.++(that)
    }
  }
}

class CompositeDefaultedFreqCounts[B, N: Numeric](private val a: DefaultedFreqCounts[B, N], private val b: DefaultedFreqCounts[B, N]) extends DefaultedFreqCounts[B, N] {

}

/**
 * Convenience wrapper of a map from items to their counts.  Its primary
 * function is to make adding counts easier.  The "++" operator adds the
 * counts for each respective entry (unlike Map's standard behavior).
 */
abstract class DefaultedCondFreqCounts[A, B, N: Numeric] {
  def ++(that: DefaultedCondFreqCounts[A, B, N]): DefaultedCondFreqCounts[A, B, N] =
    new CompositeDefaultedCondFreqCounts(this, that)

}

class BaseDefaultedCondFreqCounts[A, B, N: Numeric](private val counts: Map[A, DefaultedFreqCounts[B, N]], private val totalAddition: N, private val defaultCount: N) extends DefaultedCondFreqCounts[A, B, N] {
  override def ++(that: DefaultedCondFreqCounts[A, B, N]) = {
    that match {
      case base: BaseDefaultedCondFreqCounts[A, B, N] =>
        if (totalAddition == implicitly[Numeric[N]].zero && defaultCount == implicitly[Numeric[N]].zero)
          new BaseDefaultedCondFreqCounts(counts ++ base.counts, base.totalAddition, base.defaultCount)
        else if (base.totalAddition == implicitly[Numeric[N]].zero && base.defaultCount == implicitly[Numeric[N]].zero)
          new BaseDefaultedCondFreqCounts(counts ++ base.counts, totalAddition, defaultCount)
        else
          super.++(that)
      case _ => super.++(that)
    }
  }
}

class CompositeDefaultedCondFreqCounts[A, B, N: Numeric](private val a: DefaultedCondFreqCounts[A, B, N], private val b: DefaultedCondFreqCounts[A, B, N]) extends DefaultedCondFreqCounts[A, B, N] {

}
