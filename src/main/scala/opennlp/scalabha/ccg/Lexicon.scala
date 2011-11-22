package opennlp.scalabha.ccg

/** A lexical entry: a word and category associated with it. */
case class LexicalEntry (word: String, cat: Cat)

/**
 * A helper object that constructs a Map from words to the sets of
 * categories associated with them, based on a flat input lexicon.
 */
object Lexicon {

  lazy val catParser = new CatParser

  def apply (entries: List[String]) = {
    val lentries = entries.map(entry => catParser.parseLexEntry(entry))
    lentries.groupBy(_.word).mapValues {
      entries => entries.map(_.cat).toSet
    } 
  }

}

