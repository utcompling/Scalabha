package opennlp.scalabha.postag

import scala.util.matching.Regex

/**
 * Code to support various compling instruction for part-of-speech tagging using Scala.
 * Some inspiration taken from the NLTK.
 *
 * @author Jason Baldridge
 */

/**
 * Part-of-speech tagger trait.
 */
trait PosTagger {
  def tag (words: List[String]): List[String]
}

/**
 * A tagger which maps each word to exactly one tag. The wordTagMap should have a default
 * for unknown words.
 */
class BaselineTagger (wordTagMap: Map[String,String]) extends PosTagger {
  def tag (tokenList: List[String]) = tokenList.map(wordTagMap(_))
}

/**
 * This class defines a part-of-speech tagger that assigns a part-of-speech to each word in isolation by:
 *  - returning the tag associated with the word in the exact match word-to-tag map
 *  - failing that, returning the tag associated with the first matching regular expression in an ordered list of regexes
 *  - failing that, returning the default tag
 *
 */
class RuleBasedTagger (
  exactMatchMap: Map[String, String],
  regexTagList: List[(Regex,String)], 
  defaultTag: String
) extends PosTagger {

  def tag (words: List[String]) = words map (word => tagWord(word))

  def tagWord (word: String) = {
    exactMatchMap.get(word.toLowerCase) match {
      case Some(tag) => tag
      case None =>
        regexTagList.find(reTagPair => reTagPair._1.pattern.matcher(word).matches) match {
	  case Some((_,tag)) => tag
	  case None => defaultTag
        }
    }
  }
}

/**
 * An object that contains English specific information for part-of-speech tagging.
 * 
 */
object EnglishTagInfo {

  // A map from words to tags for common words in English
  lazy val wordTagMap = {

    // Lists of words of that overwhelmingly take a particular part of speech
    val prepositions = List("of","in","for","on","from","with","by","that","at","as","than","about","after","because","if","into","over","under","out","while", "against","before","between")
    val conjunctions = List("and","or","but","&")
    val adverbs = List("n't","not","also","only","up","even","now","just","well","very")
    val articles = List("the","a","an","this","some","any","all","every","those","no")
    val modals = List("will","might","would","could","can","may","should")
    val whwords = List("who","what","when","where","why","how","which")
    val pronouns = List("'s","it","its","he","she","they","their","his","her","hers","i","my","we","'","them","you","our")
    val verbs = List("is","was","are","be","have","has","were","had","been","do","did")
    val adjectives = List("other","new","more","first","such","many","last","major","most" )
    val numerals = List("one","two","three","four","five","six","seven","eight","nine","ten")

    // Defines the one-to-one mapping for symbols plus "to" and "there"
    val symbolTags = List("###" -> "###", "," -> ",", "." -> ".", "$" -> "$", "''" -> "'", 
                          "``" -> "`", "--" -> ":", ":" -> ":", ";" -> ":", "-rrb-" -> "-", 
                          "-lrb-" -> "-", "to" -> "T", "there" -> "E")

    // An auxiliary function to help out with the next step
    def zipWithTag (items: List[String], tag: String) = items.zip(List.fill(items.length)(tag))

    // Creates a list of pairs of words with their associated tags, and then convert to a Map
    (symbolTags 
      ::: zipWithTag(prepositions, "I")
      ::: zipWithTag(conjunctions, "C")
      ::: zipWithTag(adverbs, "R")
      ::: zipWithTag(articles, "D")
      ::: zipWithTag(modals, "M")
      ::: zipWithTag(whwords, "W")
      ::: zipWithTag(pronouns, "P")
      ::: zipWithTag(verbs, "V")
      ::: zipWithTag(adjectives, "J")
      ::: zipWithTag(numerals, "C")
    ).toMap  
  }
 
}

