package opennlp.scalabha.ccg

import collection.mutable.HashSet

object CkyParser {
  def apply (lexicon: Map[String, Set[Cat]]) = new CkyParser(lexicon, Rule.allRules)
}

class CkyParser (lexicon: Map[String,Set[Cat]], rules: List[Rule]) {

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

object CcgParser {

  import org.clapper.argot._
  import ArgotConverters._

  def main (args: Array[String]) {

    val parser = new ArgotParser("CCG Parser", preUsage = Some("Version 0.0"))

    val help = parser.flag[Boolean](List("h", "help"), "print help")

    val good = parser.option[String](
      List("g", "good"), "FILE", 
      "A file cantaining good sentences (which should receive a parse according to the given lexicon.")

    val bad = parser.option[String](
      List("b", "bad"), "FILE", 
      "A file cantaining bad sentences (which should *not* receive a parse according to the given lexicon.")

    val rules = parser.option[String](
      List("r", "rules"), "STRING", 
      "The set of rules to use. The options are:\n"
      + "\t- AB (default): for just forward and backward application (the AB calculus)"
      + "\t- Harmonic: AB plus the harmonic composition rules"
      + "\t- English: Every rule except forward crossed composition"
      + "\t- All: Every rule"
    )

    val lexiconArgument = parser.parameter[String]("lexicon", "The lexicon to use.", false)

    val input = parser.multiParameter[String]("input", "An input string to parse.", false)

    try { 
      parser.parse(args) 
    } catch { 
      case e: ArgotUsageException => println(e.message)
      System.exit(0) 
    }

    if (help.value.isDefined) {
      parser.usage() 
      System.exit(0)
    }

    val lexiconLines = io.Source.fromFile(lexiconArgument.value.get).getLines.toList
    val lexicon = Lexicon(lexiconLines)
    val ccgParser = CkyParser(lexicon)
    println(ccgParser(input.value.toIndexedSeq))

  }
}
