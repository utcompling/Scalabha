package opennlp.scalabha.ccg


/**
 * A CcgParser application: given a lexicon and a set of rules, it constructs
 * a CkyParser and applies it to given lists of good and bad sentences.
 */
object CcgParser {

  import org.clapper.argot._
  import ArgotConverters._

  def main (args: Array[String]) {

    val parser = new ArgotParser("CCG Parser", preUsage = Some("Version 0.0"))

    val help = parser.flag[Boolean](List("h", "help"), "print help")

    val testbedOption = parser.option[String](
      List("t", "testbed"), "FILE", "A file cantaining a testbed of sentences.")

    val rulesOption = parser.option[String](
      List("r", "rules"), "STRING", 
      "The set of rules to use. The options are:\n"
      + "\t- AB (default): for just forward and backward application (the AB calculus)"
      + "\t- Harmonic: AB plus the harmonic composition rules"
      + "\t- All: Every rule"
    )

    val inputParameter = parser.multiParameter[String]("input", "The name of the lexicon to use followed by an option input string to parse. The string should be not enclosed in quotes -- essentially it is encoded as a series of command-line arguments. (Necessary due to the way the scalabha sh script works.) ", false)

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

    val rules = rulesOption.value match {
      case Some("All") => Rule.allRules
      case Some("Harmonic") => Rule.harmonicRules
      case _ => Rule.abRules
    }

    val input = inputParameter.value.toList

    val lexiconFile :: inputTokens = input
    val lexiconLines = io.Source.fromFile(lexiconFile).getLines.toList
    val lexicon = Lexicon(lexiconLines)
    val ccgParser = new CkyParser(lexicon, rules)

    testbedOption.value match {
      case Some(testbedFilename) => 
	println("\nRunning testbed: " + testbedFilename + "\n")
	val testbed = Testbed(io.Source.fromFile(testbedFilename).getLines.toList)
	testbed(ccgParser)

      case _ =>
    }

    if (inputTokens != Nil) {
      println("\nInput: " + inputTokens.mkString(" "))
      println("Result: " + ccgParser(inputTokens.toIndexedSeq))
      println()
    }

  }
}


class Testbed (sentences: List[(Boolean, String)]) {

  val sentenceCat = AtomCat("s")

  def apply (parser: CkyParser) {
    for ((judgment, sentence) <- sentences) {
      val result = parser(sentence)
      val didParse = hasSentenceCat(result)
      val successMessage = if (didParse == judgment) "SUCCESS" else "FAILURE"
      println(successMessage + ": " + result)
      val doStar = if (!judgment) "*" else ""
      println(doStar + sentence + "\n")
    }
  }

  def hasSentenceCat (result: Set[Cat]) = {
    result.find(
      x => {
	x.unifies(sentenceCat) match {
	  case Some(_) => true
	  case None => false
	}
      }
    ) match {
      case Some(_) => true
      case None => false
    }
  }

}

object Testbed {
  
  def apply (rawSentences: List[String]) = {
    new Testbed(rawSentences.map {
      sentence => {
	val isGood = !sentence.startsWith("*")
	// Strip off the * if it is a bad sentence
	(isGood, if (isGood) sentence else sentence.substring(1))
      }
    })
  }
  
}
