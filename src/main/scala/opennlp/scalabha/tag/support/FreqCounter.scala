package opennlp.scalabha.tag.support

import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.util.Pattern._
import opennlp.scalabha.util.Probability
import opennlp.scalabha.util.Probability._

/**
 * A builder for frequency distributions.  Stores counts (in a mutable
 * fashion) and allows counts to be added.  A distribution based on the
 * counts is generated by calling 'toFreqDist'.
 *
 * This is the top of a hierarchy designed for a modular approach to
 * frequency distribution building.  SimpleFreqCounter serves as the basic
 * form of the counter; it stores and increments the actual counts.  Other
 * implementation of FreqCounter will be count-transforming decorators
 * extending DelegatingFreqCounter that wrap SimpleFreqCounter or wrap
 * wrappers thereof.  Multiple layers of decoration allow various
 * transformations to be applied to the counts, and in varying orders.
 *
 * The operation of the system is such that counts, when added via the
 * top-most layer are passed, untouched, all the way to the base where they
 * are stored.  When toFreqDist is called, the counts are gathered via
 * recursive calls to resultCounts that travel down the layers to the bottom,
 * where the true counts are retrieved.  Each layer, starting from the bottom,
 * then applies its transformation and returns the modified counts to be
 * received by the higher layers.  Once the (modified) counts reach the top,
 * they are used to calculate the distribution.
 *
 * For example, the following code will create a FreqCounter that, before
 * creating a distribution, will constrain its counts to those in validEntries
 * and then smooth the constrained counts:
 * {{{
 *   new SimpleSmoothingFreqCounter(lambda,
 *     new ConstrainingFreqCounter(validEntries, strict,
 *       new SimpleFreqCounter()))
 * }}}
 *
 * Implementing classes should define:
 * <ul>
 *   <li> increment: Add to counts. Should simply forward to delegate.
 *   <li> resultCounts: Apply transformation to delegate's resultCounts.
 * </ul>
 *
 * @tparam B	the item being counted
 */
abstract class FreqCounter[B] {
  def increment(b: B, n: Double)
  def resultCounts(): DefaultedFreqCounts[B, Double]

  final def ++=(other: FreqCounts[B, Double]): FreqCounter[B] = { other.toMap.foreach { case (b, n) => increment(b, n) }; this }
  final def ++=(other: TraversableOnce[B]): FreqCounter[B] = this ++= FreqCounts(other.toIterator.counts.mapValuesStrict(_.toDouble))

  final def toFreqDist(): B => Probability = {
    FreqDist(resultCounts())
  }
}

object FreqCounter {
  /**
   * Easily construct structured counter.
   *
   * apply("smooth" -> lambda, "const" -> grammar) => will smooth constrained counts
   * apply("const" -> grammar, "smooth" -> lambda) => will constrain smoothed counts
   */
  def apply[B](options: (String, Any)*) = {
    options.foldRight(new SimpleFreqCounter[B](): FreqCounter[B]) {
      case ((k, v), delegate) =>
        if (k.startsWith("smooth")) v match {
          case Double(lambda) => new SimpleSmoothingFreqCounter(lambda, delegate)
        }
        else if (k.startsWith("const")) v match {
          case (grammar: Set[B], strict: Boolean) => ConstrainingFreqCounter(grammar, strict, delegate)
          case (grammar: Option[Set[B]], strict: Boolean) => ConstrainingFreqCounter(grammar, strict, delegate)
        }
        else throw new MatchError("could not match '%s'".format(k))
    }
  }
}

//////////////////////////////////////
// Base Implementation
//////////////////////////////////////

/**
 * The base FreqCounter implementation that directly stores and updates a
 * (mutable) map of counts.
 */
class SimpleFreqCounter[B] extends FreqCounter[B] {
  private val storedCounts = collection.mutable.Map[B, Double]()
  override def increment(b: B, n: Double) { storedCounts(b) = storedCounts.getOrElse(b, 0.0) + n }
  override def resultCounts() = DefaultedFreqCounts(storedCounts.toMap, 0, 0)
  override def toString = storedCounts.toString
}

//////////////////////////////////////
// Delegating Implementation
//////////////////////////////////////

/**
 * The base FreqCounter decorator.  It handles the counter incrementing,
 * so subclasses need only implement resultCounts.
 *
 * @param delegate	the delegate counter upon which the transformation is performed
 */
abstract class DelegatingFreqCounter[B](delegate: FreqCounter[B]) extends FreqCounter[B] {
  final override def increment(b: B, n: Double) { delegate.increment(b, n) }
}

//////////////////////////////////////
// Constraining Implementation
//////////////////////////////////////

/**
 * FreqCounter decorator that zero out counts for entries not found in
 * validEntries.
 *
 * @param validEntries	zero out entries not found in this set
 * @param strict	if true, default information will be zeroed as well
 * @param delegate	the delegate counter upon which the transformation is performed
 */
class ConstrainingFreqCounter[B](validEntries: Set[B], strict: Boolean, delegate: FreqCounter[B]) extends DelegatingFreqCounter[B](delegate) {
  override def resultCounts() = {
    val DefaultedFreqCounts(delegateResultCounts, delegateTotalAddition, delegateDefaultCount) = delegate.resultCounts
    val totalAddition = if (strict) 0 else delegateTotalAddition
    val defaultCount = if (strict) 0 else delegateDefaultCount
    DefaultedFreqCounts(delegateResultCounts.toMap.filterKeys(validEntries), totalAddition, defaultCount)
  }
}

object ConstrainingFreqCounter {
  def apply[B](validEntries: Set[B], strict: Boolean, delegate: FreqCounter[B]) =
    new ConstrainingFreqCounter(validEntries, strict, delegate)

  /**
   * Accept validEntries as an Option.  If None: don't bother wrapping.
   */
  def apply[B](validEntries: Option[Set[B]], strict: Boolean, delegate: FreqCounter[B]) =
    validEntries match {
      case Some(validEntries) => new ConstrainingFreqCounter(validEntries, strict, delegate)
      case None => delegate
    }
}

//////////////////////////////////////
// Smoothing Implementation
//////////////////////////////////////

/**
 * NOT YET IMPLEMENTED
 *
 * FreqCounter decorator that smoothes counts.
 *
 * Note: This class will always return counts of type Double, regardless of
 * the type of counts passed in.  This is because smoothing results in
 * fractional counts.
 *
 * @param lambda	smoothing parameter
 */
class SimpleSmoothingFreqCounter[B](lambda: Double, delegate: FreqCounter[B]) extends DelegatingFreqCounter[B](delegate) {
  override def resultCounts() = {
    sys.error("not implemented") //TODO:
  }
}

//////////////////////////////////////
// Factory
//////////////////////////////////////

/**
 * A factory for FreqCounter objects.  Exists so that procedures requiring
 * FreqCounters can create new ones at will.
 *
 * {{{
 *   new FreqCounterFactory {
 *     def get() = SimpleFreqCounter()
 *   }
 * }}}
 */
abstract class FreqCounterFactory[B] {
  def get(): FreqCounter[B]
  final def get(initial: TraversableOnce[B]): FreqCounter[B] = get() ++= initial
  final def get(initial: FreqCounts[B, Double]): FreqCounter[B] = get() ++= initial
}

object FreqCounterFactory {
  /**
   * Easily construct structured counter factory.
   *
   * apply("smooth" -> lambda, "const" -> grammar) => will smooth constrained counts
   * apply("const" -> grammar, "smooth" -> lambda) => will constrain smoothed counts
   */
  def apply[B](options: (String, Any)*) =
    new FreqCounterFactory[B] {
      def get() = FreqCounter(options: _*)
    }
}
