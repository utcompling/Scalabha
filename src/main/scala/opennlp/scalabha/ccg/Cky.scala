package opennlp.scalabha.ccg

/** A CkyParser that uses the given lexicon and set of rules. */
class CkyParser (lexicon: Map[String,Set[Cat]], rules: List[Rule]) {

  import collection.mutable.HashSet

  def apply (sentence: String): Set[Cat] = apply(sentence.split(" ").toIndexedSeq)

  def apply (tokens: IndexedSeq[String]): Set[Cat] = {
    val numItems = tokens.length

    val chart: Array[Array[Set[Cat]]] = 
      Array.fill(numItems, numItems)(new collection.immutable.HashSet[Cat]())
    
    for (j <- (0 until numItems)) {
      chart(j)(j) = lexicon(tokens(j))

      for (i <- (0 to j-1).reverse) {
	val fill = new HashSet[Cat]

	for (k <- i to j-1) {

	  for (left <- chart(i)(k)) {

	    for (right <- chart(k+1)(j)) {

	      rules.foreach { rule =>
                val (leftUniq, rightUniq) = Cat.makeAllVarsUniq(left,right)
		rule(leftUniq, rightUniq) match {
		  case Some(result) => fill.add(result)
		  case None => 
		}
	      }
	    }
	  }
	  chart(i)(j) = fill.toSet
	}
      }
    }
    chart(0)(numItems-1)
  }
}    

/** A companion object for the CkyParser class. */
object CkyParser {
  
  // Create a CkyParser object with the provided lexicon and using all
  // possible rules.
  def apply (lexicon: Map[String, Set[Cat]]) = new CkyParser(lexicon, Rule.allRules)

}
