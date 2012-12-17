package opennlp.scalabha.ccg

import scala.collection.breakOut

/** A CkyParser that uses the given lexicon and set of rules. */
class CkyParser(lexicon: Map[String, Set[Cat]], rules: List[Rule]) {

  def apply(sentence: String): Set[Cat] = apply(sentence.split(" ").toIndexedSeq)

  def apply(tokens: IndexedSeq[String]): Set[Cat] = {
    val numItems = tokens.length

    tokens.foreach { token =>
      if (!lexicon.contains(token))
        throw new MissingLexicalEntryException("\n\nError: word '" + token + "' not in lexicon!\n")
    }

    val chart: Array[Array[Set[Cat]]] = Array.fill(numItems, numItems)(Set[Cat]())

    for (j <- (0 until numItems)) {
      chart(j)(j) = lexicon(tokens(j))

      for (i <- (0 to j - 1).reverse) {
        chart(i)(j) =
          (for (
            k <- i to j - 1;
            left <- chart(i)(k);
            right <- chart(k + 1)(j);
            (leftUniq, rightUniq) = Cat.makeAllVarsUniq(left, right);
            rule <- rules;
            result <- rule(leftUniq, rightUniq)
          ) yield result)(breakOut)
      }
    }
    chart(0)(numItems - 1)
  }
}

/** A companion object for the CkyParser class. */
object CkyParser {

  /**
   * Create a CkyParser object with the provided lexicon and using all
   * possible rules.
   */
  def apply(lexicon: Map[String, Set[Cat]]) = new CkyParser(lexicon, Rule.allRules)

}
