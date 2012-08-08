package opennlp.scalabha.tag

import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.util.LogNum
import opennlp.scalabha.tag.TagDict._

/**
 * TagDict base trait.  Exists only for use as a facade to make a TD appear
 * unweighted (because all valid TD implementations are actually weighted).
 *
 * This trait should NEVER be implemented directly.  ALL tag dictionary
 * implementations must extend from WeightedTagDict!
 */
sealed trait TagDict[Sym, Tag] {
  //
  // Base methods -- ALL of which are overridden by WeightedTagDict, meaning 
  // that these should NEVER be implemented by any extending class.
  //

  /** The set of tags for unknown symbols (symbols not appearing in the dict) */
  def defaultSet: Set[Tag]

  /** The set of tags associated with this symbol, or None if it does not exist.  Do NOT return the default. */
  def doGetSet(s: Sym): Option[Set[Tag]]

  /** An iterator over the symbols and their associated tag sets */
  def setIterator: Iterator[(Sym, Set[Tag])]

  //
  // Derived methods (all final)
  //

  /** The set of tags associated with this symbol, or the default set if it does not exist. */
  final def set(s: Sym): Set[Tag] = doGetSet(s).getOrElse(defaultSet)

  /** Does the symbol exist as an entry in the tag dict (excluding defaults)? */
  final def contains(s: Sym): Boolean = doGetSet(s).isDefined

  /** Does this symbol/tag entry exist in the tag dict (excluding defaults)? */
  final def contains(s: Sym, t: Tag): Boolean = doGetSet(s).exists(_(t))

  /** The set of all known tags */
  final def allTags = setIterator.flatMap(_._2).toSet ++ defaultSet

  /** The set of all known symbols */
  final def symbols: Set[Sym] = setIterator.map(_._1).toSet
}

trait WeightedTagDict[Sym, Tag] extends TagDict[Sym, Tag] {
  //
  // Base methods
  //

  /** The map of tag weights for unknown symbols (symbols not appearing in the dict) */
  def default: Map[Tag, LogNum]

  /** An iterator over the symbols and their associated tag/weight maps */
  def iterator: Iterator[(Sym, Map[Tag, LogNum])]

  /** The tag/weight map associated with this symbol, or None if it does not exist.  Do NOT return the default. */
  def doGetMap(s: Sym): Option[Map[Tag, LogNum]]

  //
  // Methods finalizing those on TagDict -- meaning they are final on ALL valid 
  // TagDict implementations.
  //

  final override def defaultSet = default.keySet
  final override def doGetSet(s: Sym) = doGetMap(s).map(_.keySet)
  final override def setIterator = iterator.mapVals(_.keySet)

  //
  // Derived methods
  //

  final def weights(s: Sym): Map[Tag, LogNum] = doGetMap(s).getOrElse(default)
}

/**
 * The base TagDict implementation from which all TagDict implementations are
 * derived (as wrappers of this class).
 */
final class SimpleWeightedTagDict[Sym, Tag](d: Map[Sym, Map[Tag, LogNum]], override val default: Map[Tag, LogNum]) extends WeightedTagDict[Sym, Tag] {
  override def doGetMap(s: Sym) = d.get(s)
  override def iterator = d.iterator
}

object SimpleWeightedTagDict {
  def apply[Sym, Tag](d: Map[Sym, Map[Tag, LogNum]], default: Map[Tag, LogNum]) = new SimpleWeightedTagDict(d, default)
}

/**
 * Convenience constructors for TagDict where every entry has weight 1.0 (unnormalized).
 */
object UniformWeightedTagDict {
  def apply[Sym, Tag](d: Map[Sym, Set[Tag]]): WeightedTagDict[Sym, Tag] = UniformWeightedTagDict(d, d.values.flatten.toSet)
  def apply[Sym, Tag](d: Map[Sym, Set[Tag]], default: Set[Tag]): WeightedTagDict[Sym, Tag] = new SimpleWeightedTagDict(d.mapVals(_.mapToVal(LogNum.one).toMap), default.mapToVal(LogNum.one).toMap)
}

/**
 * Convenience constructors for an "unweighted" tagdict. Underlyingly, it's
 * really returning a weighted tagdict where every weight is 1.0.
 */
object SimpleTagDict {
  def apply[Sym, Tag](d: Map[Sym, Set[Tag]]): TagDict[Sym, Tag] = SimpleTagDict(d, d.values.flatten.toSet)
  def apply[Sym, Tag](d: Map[Sym, Set[Tag]], default: Set[Tag]): TagDict[Sym, Tag] = new SimpleWeightedTagDict(d.mapVals(_.mapToVal(LogNum.one).toMap), default.mapToVal(LogNum.one).toMap)
}

/**
 * TagDict wrapper that allows None symbols, but, the only possible tag for
 * None is None.  They None entry never needs to be specified.
 */
object OptionalTagDict {
  def apply[Sym, Tag](tagDict: WeightedTagDict[Sym, Tag]): OptionalWeightedTagDict[Sym, Tag] =
    new WeightedTagDict[Option[Sym], Option[Tag]] {
      override def default = tagDict.default.mapKeys(Option(_))

      override def doGetMap(sym: Option[Sym]): Option[Map[Option[Tag], LogNum]] =
        sym match {
          case None => Some(Map(None -> LogNum.one))
          case Some(s) => tagDict.doGetMap(s).map(_.mapKeys(Option(_)))
        }

      override def iterator = tagDict.iterator.map { case (s, ts) => Option(s) -> ts.mapKeys(Option(_)) }
    }

  /**
   * Convenience constructor that provides an "unweighted" facade on an
   * OptionalTagDict
   */
  def apply[Sym, Tag](tagDict: TagDict[Sym, Tag]): OptionalTagDict[Sym, Tag] =
    OptionalTagDict(tagDict.asInstanceOf[WeightedTagDict[Sym, Tag]])
}

/**
 * A TagDict wrapper that has no default tags for unknown symbols
 */
object NoDefaultTagDict {
  def apply[Sym, Tag](tagDict: WeightedTagDict[Sym, Tag]): WeightedTagDict[Sym, Tag] =
    new WeightedTagDict[Sym, Tag] {
      override def default = Map()
      override def doGetMap(sym: Sym) = tagDict.doGetMap(sym)
      override def iterator = tagDict.iterator
    }

  /**
   * Convenience constructor that provides an "unweighted" facade on an
   * OptionalTagDict
   */
  def apply[Sym, Tag](tagDict: TagDict[Sym, Tag]): WeightedTagDict[Sym, Tag] =
    NoDefaultTagDict(tagDict.asInstanceOf[WeightedTagDict[Sym, Tag]])
}

/**
 * A TagDict wrapper that has no default tags for unknown symbols
 */
object SmoothingTagDict {
  def apply[Sym, Tag](tagDict: WeightedTagDict[Sym, Tag]): WeightedTagDict[Sym, Tag] =
    new WeightedTagDict[Sym, Tag] {
      override def default = Map()
      override def doGetMap(sym: Sym) = tagDict.doGetMap(sym)
      override def iterator = tagDict.iterator
    }

  /**
   * Convenience constructor that provides an "unweighted" facade on an
   * OptionalTagDict
   */
  def apply[Sym, Tag](tagDict: TagDict[Sym, Tag]): WeightedTagDict[Sym, Tag] =
    NoDefaultTagDict(tagDict.asInstanceOf[WeightedTagDict[Sym, Tag]])
}

object TagDict {
  type OptionalTagDict[Sym, Tag] = TagDict[Option[Sym], Option[Tag]]
  type OptionalWeightedTagDict[Sym, Tag] = WeightedTagDict[Option[Sym], Option[Tag]]
}
