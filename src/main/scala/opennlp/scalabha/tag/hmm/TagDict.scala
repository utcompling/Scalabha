package opennlp.scalabha.tag.hmm

//trait TagDict[Sym, Tag] {
//  def apply(s: Sym): Set[Tag]
//}

object OptionalTagDict {

  def apply[Sym, Tag](tagDict: Map[Sym, Set[Tag]]): Map[Option[Sym], Set[Option[Tag]]] = {
    val optioned = tagDict.map { case (k, vs) => Option(k) -> vs.map(Option(_)) }
    val allTags = optioned.values.flatten.toSet
    val end: (Option[Sym], Set[Option[Tag]]) = (None -> Set(None))
    val x = (optioned + end)
    x.withDefaultValue(allTags)
  }

  // TODO: Make a special class for this
  //  - Store a m: Map[Sym, Set[Tag]]
  //  - def get(key: Option[Sym]) = key match { case Some(k) => m(k); case None => Set(None) }

}
