package opennlp.scalabha.tag

import opennlp.scalabha.util.LogNum
import opennlp.scalabha.util.CollectionUtils._

////////////////////////////////
// TagDict interface
////////////////////////////////

abstract class TagDict[Sym, Tag](val defaultSet: Set[Tag]) {
  final def set(s: Sym): Set[Tag] = getSet(s).get
  final def getSet(s: Sym): Option[Set[Tag]] = Some(doGetSet(s).getOrElse(defaultSet))

  /**
   * Return the tagset for the symbol, or None if it does not exist.  Do not
   * return the default.
   */
  protected def doGetSet(s: Sym): Option[Set[Tag]]

  /**
   * Does the symbol exist as an entry in the tag dict (excluding defaults)?
   */
  final def contains(s: Sym): Boolean = doGetSet(s).isDefined

  final def allTags = setIterator.flatMap(_._2).toSet ++ defaultSet
  final def symbols: Set[Sym] = setIterator.map(_._1).toSet
  def setIterator: Iterator[(Sym, Set[Tag])]
}

////////////////////////////////
// Unweighted TagDict
////////////////////////////////

abstract class UnweightedTagDict[Sym, Tag](val default: Set[Tag]) extends TagDict[Sym, Tag](default) {
  def iterator: Iterator[(Sym, Set[Tag])]
  final override def setIterator = iterator
}

///////////////////////
// Simple Unweighted TagDict

class SimpleTagDict[Sym, Tag](d: Map[Sym, Set[Tag]], override val default: Set[Tag]) extends UnweightedTagDict[Sym, Tag](default) {
  protected override def doGetSet(s: Sym) = d.get(s)
  override def iterator = d.iterator
}

object SimpleTagDict {
  def apply[Sym, Tag](d: Map[Sym, Set[Tag]]) = new SimpleTagDict(d, d.values.flatten.toSet)
  def apply[Sym, Tag](d: Map[Sym, Set[Tag]], default: Set[Tag]) = new SimpleTagDict(d, default)
}

///////////////////////
// Optional Unweighted TagDict

class OptionalTagDict[Sym, Tag](tagDict: TagDict[Sym, Tag]) extends UnweightedTagDict[Option[Sym], Option[Tag]](tagDict.defaultSet.map(Option(_))) {
  val optioned: Map[Option[Sym], Set[Option[Tag]]] = tagDict.setIterator.map { case (s, ts) => Option(s) -> ts.map(Option(_)) }.toMap
  val unoptioned = tagDict
  protected override def doGetSet(s: Option[Sym]): Option[Set[Option[Tag]]] = s match {
    case None => Some(Set(None))
    case _ => optioned.get(s)
  }
  override def iterator = optioned.iterator
}

object OptionalTagDict {
  def apply[Sym, Tag](tagDict: TagDict[Sym, Tag]) = new OptionalTagDict(tagDict)
}

////////////////////////////////
// Weighted TagDict
////////////////////////////////

abstract class WeightedTagDict[Sym, Tag](val default: Map[Tag, LogNum]) extends TagDict[Sym, Tag](default.keySet) {
  def iterator: Iterator[(Sym, Map[Tag, LogNum])]
  final override def setIterator = iterator.mapValuesStrict(_.keySet)
  protected def doGetMap(s: Sym): Option[Map[Tag, LogNum]]
  final protected override def doGetSet(s: Sym) = doGetMap(s).map(_.keySet)
}

///////////////////////
// Simple Weighted TagDict

class SimpleWeightedTagDict[Sym, Tag](d: Map[Sym, Map[Tag, LogNum]], override val default: Map[Tag, LogNum]) extends WeightedTagDict[Sym, Tag](default) {
  protected override def doGetMap(s: Sym) = d.get(s)
  override def iterator = d.iterator
}

object SimpleWeightedTagDict {
  def apply[Sym, Tag](d: Map[Sym, Map[Tag, LogNum]]) = new SimpleWeightedTagDict(d, d.values.flatten.groupByKey.mapValues(_.sum).normalizeValues)
  def apply[Sym, Tag](d: Map[Sym, Map[Tag, LogNum]], default: Map[Tag, LogNum]) = new SimpleWeightedTagDict(d, default)
}

///////////////////////
// Optional Weighted TagDict

class OptionalWeightedTagDict[Sym, Tag](tagDict: WeightedTagDict[Sym, Tag]) extends WeightedTagDict[Option[Sym], Option[Tag]](tagDict.default.mapKeys(Option(_))) {
  val optioned: Map[Option[Sym], Map[Option[Tag], LogNum]] = tagDict.iterator.map { case (s, ts) => Option(s) -> ts.mapKeys(Option(_)) }.toMap
  val unoptioned = tagDict

  protected override def doGetMap(s: Option[Sym]): Option[Map[Option[Tag], LogNum]] = s match {
    case None => Some(Map(None -> LogNum.one))
    case _ => optioned.get(s)
  }

  override def iterator = optioned.iterator
}

object OptionalWeightedTagDict {
  def apply[Sym, Tag](tagDict: TagDict[Sym, Tag]) = new OptionalTagDict(tagDict)
}
