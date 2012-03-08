package opennlp.scalabha.postag

/**
 * A basic implementation of an HMM with some simple smoothing, roughly following
 * the description wit the following homework:
 *
 *    http://nlp-s11.utcompling.com/assignments/hmm-tagging
 *
 * @author Jason Baldridge
 */
class HmmTagger(
  val transitions: IndexedSeq[IndexedSeq[Double]],
  val emissions: IndexedSeq[Map[String, Double]],
  val tagdict: Map[String, Set[Int]],
  val tagset: IndexedSeq[String]) extends PosTagger {

  val numTags = tagset.length

  // Use the viterbi algorithm to tag a token sequence. Could be much
  // improved, but works.
  def tag(tokenList: List[String]) = {

    // Lists are slow for indexing into, so change to IndexedSeq
    val tokens = tokenList.toIndexedSeq
    val validTags = tokens.map(tok => tagdict(tok))
    var backpointers = List[Array[Int]]()
    var viterbi = Array.fill(numTags)(scala.Double.NegativeInfinity)
    viterbi(0) = 0.0
    // Assumes the first (0th) element is ### in the input
    for (i <- 1 until tokens.length) {
      val updateViterbi = Array.fill(numTags)(scala.Double.NegativeInfinity)
      val currBackpointers = Array.fill(numTags)(-1)
      for (currTag <- validTags(i)) {
        var max = scala.Double.NegativeInfinity
        var prevBest = 0
        for (prevTag <- validTags(i - 1)) {
          val pathLogProb = viterbi(prevTag) + transitions(prevTag)(currTag) + emissions(currTag)(tokens(i))
          if (max < pathLogProb) {
            max = pathLogProb
            prevBest = prevTag
          }
        }
        updateViterbi(currTag) = max
        currBackpointers(currTag) = prevBest
      }
      viterbi = updateViterbi
      backpointers ::= currBackpointers
    }

    // Get the optimal tag sequence and map the tag indices back to their string values
    backtrack(backpointers).map(tagset(_))
  }

  // Backtrack through the backpointer maps to recover the optimal tag sequence.
  def backtrack(backpointers: List[Array[Int]]): List[Int] = {
    // Recursive inner method that make sure tail recursion is used.
    def inner(backpointers: List[Array[Int]], tags: List[Int]): List[Int] = backpointers match {
      case Nil => tags
      case currPointers :: previousPointers => inner(previousPointers, currPointers(tags.head) :: tags)
    }
    inner(backpointers, List(0))
  }

}

/**
 * A helper object that builds the emission and transition distributions from a stream
 * of tokens and tags.
 */
object HmmTrainer {

  def apply(wordTagSequence: List[(String, String)], lambda: Double) = {

    // Construct the transition probabilities (log values)
    val (wordSequence, rawTagSequence) = wordTagSequence.unzip

    // Create a Seq containing that ensures that the special EOS tag
    // ### has the index zero.
    val tagset = ("###" :: rawTagSequence.toSet.toList.filter(_ != "###")).toIndexedSeq

    // Create a Map from tags to their indices
    val tagIndices = tagset.zipWithIndex.toMap

    // The sequence of tags, now as their indices rather than string values
    val tagSequence = rawTagSequence.map(tag => tagIndices(tag))

    // Get the raw transition counts
    val tagTagPairs = tagSequence.sliding(2).map(tt => (tt(0), tt(1))).toList
    val transitionCounts = tagTagPairs.groupBy(tt => tt._1).mapValues(
      ttListForTag => ttListForTag.map(tt => tt._2).groupBy(x => x).mapValues(tagList => tagList.length))

    val numTags = tagset.length
    val numTokens = tagSequence.length
    val tagDist = tagSequence.groupBy(x => x).mapValues(_.length / numTokens.toDouble)

    // Get the raw unigram tag MLEs.
    val allTagBackoff = Array.fill(numTags)(0.0)
    tagDist.foreach { case (tag, unigramProb) => allTagBackoff(tag) = tagDist(tag) }

    // Create the transition probabilities (log values)
    val transitionProbabilities = (0 until numTags).map {
      tag =>
        {
          val singleCountNextTag = transitionCounts(tag).count(x => x._2 == 1)
          val tagLambda = lambda + singleCountNextTag
          val arrayOfCounts = allTagBackoff.map(_ * tagLambda)
          transitionCounts(tag).foreach { case (nextTag, count) => arrayOfCounts(nextTag) += count }
          val total = arrayOfCounts.sum
          arrayOfCounts.map(count => math.log(count / total)).toIndexedSeq
        }
    } toIndexedSeq

    // Get the unigram word probabilities, with add-one smoothing.
    val smoothedWordCounts = wordSequence.groupBy(x => x).mapValues(_.length + 1)
    val smoothedTotal = 1 + smoothedWordCounts.foldRight(0.0) { case ((word, count), sum) => sum + count }
    val unigramWordDist = smoothedWordCounts.mapValues(_ / smoothedTotal).withDefault(x => 1.0 / smoothedTotal)

    // Get the raw emission counts
    val emissionCounts = wordSequence.zip(tagSequence).groupBy(wt => wt._2).mapValues(
      wtListForTag => wtListForTag.map(wt => wt._1).groupBy(x => x).mapValues(wordList => wordList.length))

    // Construct the emission probabilities (log values)
    val emissionProbabilities = (0 until numTags).map {
      tag =>
        {
          val singleCountWordEmission = emissionCounts(tag).count(x => x._2 == 1)
          //println("** " + singleCountWordEmission)
          //val smoothedCounts = emissionCounts(tag).mapValues(_+lambda+singleCountWordEmission)
          val tagLambda = lambda + singleCountWordEmission
          val smoothedCounts = emissionCounts(tag).map {
            case (word, count) => (word, count + tagLambda * unigramWordDist(word))
          }
          val total = smoothedCounts.foldRight(lambda) { case ((word, count), sum) => sum + count }
          smoothedCounts.mapValues(count => math.log(count / total)).withDefault(x => math.log(tagLambda * unigramWordDist("") / total))
        }
    } toIndexedSeq

    // Construct the tag dictionary (words to their known possible
    // tags) and set the default to be the entire tagset so that
    // unknown words will have all possible tags in the tag
    // dictionary.
    val tagdict = wordSequence.zip(tagSequence).groupBy(wt => wt._1).mapValues(
      wtListForTag => wtListForTag.map(wt => wt._2).toSet).withDefault(x => emissionCounts.keySet - 0)

    // Create the HmmTagger
    new HmmTagger(transitionProbabilities, emissionProbabilities, tagdict, tagset)
  }

}
