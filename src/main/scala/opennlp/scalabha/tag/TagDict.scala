package opennlp.scalabha.tag

trait TagDict[Sym, Tag] extends (Sym => Set[Tag]) { //}extends Map[Sym, Set[Tag]] with MapLike[Sym, Set[Tag], TagDict[Sym, Tag]] {
  val default: Set[Tag]

  final def apply(s: Sym) = get(s).get
  final def get(s: Sym): Option[Set[Tag]] = Some(doGet(s).getOrElse(default))

  /**
   * Return the tagset for the symbol, or None if it does not exist.  Do not
   * return the default.
   */
  protected def doGet(s: Sym): Option[Set[Tag]]

  /**
   * Does the symbol exist as an entry in the tag dict (excluding defaults)?
   */
  final def contains(s: Sym) = doGet(s).isDefined

  def iterator: Iterator[(Sym, Set[Tag])]
  final def symbols = iterator.map(_._1).toSet
  final def allTags = iterator.flatMap(_._2).toSet ++ default
  //  final def +[B1 >: Set[Tag]](kv: (Sym, B1)) = sys.error("not implemented")
  //  final def -(key: Sym): TagDict[Sym, Tag] = sys.error("not implemented")
  //  def empty: TagDict[Sym, Tag]
}

class SimpleTagDict[Sym, Tag](d: Map[Sym, Set[Tag]], val default: Set[Tag]) extends TagDict[Sym, Tag] {
  protected def doGet(s: Sym) = d.get(s)
  def iterator = d.iterator
  //override def empty: TagDict[Sym, Tag] = new SimpleTagDict(Map())
}

object SimpleTagDict {
  def apply[Sym, Tag](d: Map[Sym, Set[Tag]]) = new SimpleTagDict(d, d.values.flatten.toSet)
  def apply[Sym, Tag](d: Map[Sym, Set[Tag]], default: Set[Tag]) = new SimpleTagDict(d, default)
}

class OptionalTagDict[Sym, Tag](tagDict: TagDict[Sym, Tag]) extends TagDict[Option[Sym], Option[Tag]] {
  val optioned: Map[Option[Sym], Set[Option[Tag]]] = tagDict.iterator.map { case (s, ts) => Option(s) -> ts.map(Option(_)) }.toMap
  val default = tagDict.default.map(Option(_))
  def unoptioned = tagDict

  protected def doGet(s: Option[Sym]): Option[Set[Option[Tag]]] = s match {
    case None => Some(Set(None))
    case _ => optioned.get(s)
  }

  def iterator = optioned.iterator
  //override def empty: TagDict[Option[Sym], Option[Tag]] = new OptionalTagDict(new SimpleTagDict(Map()))
}

object OptionalTagDict {
  def apply[Sym, Tag](tagDict: TagDict[Sym, Tag]) = new OptionalTagDict(tagDict)
}
