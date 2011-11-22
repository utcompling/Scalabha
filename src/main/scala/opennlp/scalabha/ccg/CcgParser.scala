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
