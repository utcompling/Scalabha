package opennlp.scalabha.classify.perceptron

import collection.mutable

/**
 * A class for "memoizing" words, i.e. mapping them to some other type
 * (e.g. Int) that should be faster to compare and potentially require
 * less space.
 */
abstract class Memoizer {
  /**
   * The type of a memoized word.
   */
  type Word
  /**
   * Map a word as a string to its memoized form.
   */
  def memoize_string(word: String): Word
  /**
   * Map a word from its memoized form back to a string.
   */
  def unmemoize_string(word: Word): String

  /**
   * The type of a mutable map from memoized words to Ints.
   */
  type WordIntMap
  /**
   * Create a mutable map from memoized words to Ints.
   */
  def create_word_int_map(): WordIntMap
  /**
   * The type of a mutable map from memoized words to Doubles.
   */
  type WordDoubleMap
  /**
   * Create a mutable map from memoized words to Doubles.
   */
  def create_word_double_map(): WordDoubleMap

  lazy val blank_memoized_string = memoize_string("")

  def lowercase_memoized_word(word: Word) =
    memoize_string(unmemoize_string(word).toLowerCase)
}

/**
 * The memoizer we actually use.  Maps word strings to Ints.  Uses Trove
 * for extremely fast and memory-efficient hash tables, making use of the
 * Trove-Scala interface for easy access to the Trove hash tables.
 */
class IntStringMemoizer extends Memoizer {
  type Word = Int
  val invalid_word: Word = 0

  protected var next_word_count: Word = 1

  def maximum_index = next_word_count - 1

  // For replacing strings with ints.  This should save space on 64-bit
  // machines (string pointers are 8 bytes, ints are 4 bytes) and might
  // also speed lookup.
  protected val word_id_map = mutable.Map[String,Word]()
  //protected val word_id_map = trovescala.ObjectIntMap[String]()

  // Map in the opposite direction.
  protected val id_word_map = mutable.Map[Word,String]()
  //protected val id_word_map = trovescala.IntObjectMap[String]()

  def memoize_string(word: String) = {
    val index = word_id_map.getOrElse(word, 0)
    if (index != 0) index
    else {
      val newind = next_word_count
      next_word_count += 1
      word_id_map(word) = newind
      id_word_map(newind) = word
      newind
    }
  }

  def unmemoize_string(word: Word) = id_word_map(word)

  //def create_word_int_map() = trovescala.IntIntMap()
  //type WordIntMap = trovescala.IntIntMap
  //def create_word_double_map() = trovescala.IntDoubleMap()
  //type WordDoubleMap = trovescala.IntDoubleMap
  def create_word_int_map() = mutable.Map[Word,Int]()
  type WordIntMap = mutable.Map[Word,Int]
  def create_word_double_map() = mutable.Map[Word,Double]()
  type WordDoubleMap = mutable.Map[Word,Double]
}
