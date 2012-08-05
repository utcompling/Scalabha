package opennlp.scalabha.tag

import opennlp.scalabha.util.LogNum
import opennlp.scalabha.util.CollectionUtils._

////////////////////////////////
// TagDict interface
////////////////////////////////

trait TagDict[Sym, Tag] {
  def defaultSet: Set[Tag]

  final def set(s: Sym): Set[Tag] = doGetSet(s).getOrElse(defaultSet)

  /**
   * Return the tagset for the symbol, or None if it does not exist.  Do not
   * return the default.
   */
  def doGetSet(s: Sym): Option[Set[Tag]]

  /**
   * Does the symbol exist as an entry in the tag dict (excluding defaults)?
   */
  final def contains(s: Sym): Boolean = doGetSet(s).isDefined

  /**
   * Does this symbol/tag entry exist in the tag dict (excluding defaults)?
   */
  final def contains(s: Sym, t: Tag): Boolean = doGetSet(s).exists(_(t))

  final def allTags = setIterator.flatMap(_._2).toSet ++ defaultSet
  final def symbols: Set[Sym] = setIterator.map(_._1).toSet
  def setIterator: Iterator[(Sym, Set[Tag])]
}

////////////////////////////////
// OptionalTagDict interface
////////////////////////////////

trait OptionalTagDict[Sym, Tag] extends TagDict[Option[Sym], Option[Tag]] {
  def unoptioned: TagDict[Sym, Tag]
}

object OptionalTagDict {
  def apply[Sym, Tag](tagDict: UnweightedTagDict[Sym, Tag]) = OptionalUnweightedTagDict(tagDict)
  def apply[Sym, Tag](tagDict: WeightedTagDict[Sym, Tag]) = OptionalWeightedTagDict(tagDict)
  def apply[Sym, Tag](tagDict: TagDict[Sym, Tag]) = tagDict match {
    case d: UnweightedTagDict[Sym, Tag] => OptionalUnweightedTagDict(d)
    case d: WeightedTagDict[Sym, Tag] => OptionalWeightedTagDict(d)
  }
}

////////////////////////////////
// Unweighted TagDict
////////////////////////////////

abstract class UnweightedTagDict[Sym, Tag](val default: Set[Tag]) extends TagDict[Sym, Tag] {
  final override def defaultSet = default
  def iterator: Iterator[(Sym, Set[Tag])]
  final override def setIterator = iterator
}

///////////////////////
// Simple Unweighted TagDict

class SimpleTagDict[Sym, Tag](d: Map[Sym, Set[Tag]], override val default: Set[Tag]) extends UnweightedTagDict[Sym, Tag](default) {
  override def doGetSet(s: Sym) = d.get(s)
  override def iterator = d.iterator
}

object SimpleTagDict {
  def apply[Sym, Tag](d: Map[Sym, Set[Tag]]) = new SimpleTagDict(d, d.values.flatten.toSet)
  def apply[Sym, Tag](d: Map[Sym, Set[Tag]], default: Set[Tag]) = new SimpleTagDict(d, default)
}

///////////////////////
// Optional Unweighted TagDict

class OptionalUnweightedTagDict[Sym, Tag](tagDict: UnweightedTagDict[Sym, Tag])
  extends UnweightedTagDict[Option[Sym], Option[Tag]](tagDict.defaultSet.map(Option(_)))
  with OptionalTagDict[Sym, Tag] {

  val optioned: Map[Option[Sym], Set[Option[Tag]]] = tagDict.setIterator.map { case (s, ts) => Option(s) -> ts.map(Option(_)) }.toMap
  override def unoptioned: UnweightedTagDict[Sym, Tag] = tagDict
  override def doGetSet(s: Option[Sym]): Option[Set[Option[Tag]]] = s match {
    case None => Some(Set(None))
    case _ => optioned.get(s)
  }
  override def iterator = optioned.iterator
}

object OptionalUnweightedTagDict {
  def apply[Sym, Tag](tagDict: UnweightedTagDict[Sym, Tag]) = new OptionalUnweightedTagDict(tagDict)
}

////////////////////////////////
// Weighted TagDict
////////////////////////////////

abstract class WeightedTagDict[Sym, Tag](val default: Map[Tag, LogNum]) extends TagDict[Sym, Tag] {
  final override def defaultSet = default.keySet
  final def weights(s: Sym): Map[Tag, LogNum] = doGetMap(s).getOrElse(default)
  def iterator: Iterator[(Sym, Map[Tag, LogNum])]
  final override def setIterator = iterator.mapVals(_.keySet)
  def doGetMap(s: Sym): Option[Map[Tag, LogNum]]
  final override def doGetSet(s: Sym) = doGetMap(s).map(_.keySet)
}

///////////////////////
// Simple Weighted TagDict

class SimpleWeightedTagDict[Sym, Tag](d: Map[Sym, Map[Tag, LogNum]], override val default: Map[Tag, LogNum]) extends WeightedTagDict[Sym, Tag](default) {
  override def doGetMap(s: Sym) = d.get(s)
  override def iterator = d.iterator
}

object SimpleWeightedTagDict {
  def apply[Sym, Tag](d: Map[Sym, Map[Tag, LogNum]], default: Map[Tag, LogNum]) = new SimpleWeightedTagDict(d, default)
}

///////////////////////
// Optional Weighted TagDict

class OptionalWeightedTagDict[Sym, Tag](tagDict: WeightedTagDict[Sym, Tag])
  extends WeightedTagDict[Option[Sym], Option[Tag]](tagDict.default.mapKeys(Option(_)))
  with OptionalTagDict[Sym, Tag] {

  val optioned: Map[Option[Sym], Map[Option[Tag], LogNum]] = tagDict.iterator.map { case (s, ts) => Option(s) -> ts.mapKeys(Option(_)) }.toMap
  override def unoptioned: WeightedTagDict[Sym, Tag] = tagDict

  override def doGetMap(s: Option[Sym]): Option[Map[Option[Tag], LogNum]] = s match {
    case None => Some(Map(None -> LogNum.one))
    case _ => optioned.get(s)
  }

  override def iterator = optioned.iterator
}

object OptionalWeightedTagDict {
  def apply[Sym, Tag](tagDict: WeightedTagDict[Sym, Tag]) = new OptionalWeightedTagDict(tagDict)
}

///////////////////////
// Fake Weighted TagDict

class FakeWeightedTagDict[Sym, Tag](tagDict: TagDict[Sym, Tag]) extends WeightedTagDict[Sym, Tag](tagDict.defaultSet.mapToVal(LogNum.one).toMap) {

  override def doGetMap(s: Sym): Option[Map[Tag, LogNum]] =
    tagDict.doGetSet(s).map(_.mapToVal(LogNum.one).toMap)

  override def iterator = tagDict.setIterator.mapValues(_.mapToVal(LogNum.one).toMap)
}

object FakeWeightedTagDict {
  def apply[Sym, Tag](tagDict: TagDict[Sym, Tag]) = new FakeWeightedTagDict(tagDict)
}
