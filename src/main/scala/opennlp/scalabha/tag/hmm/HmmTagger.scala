package opennlp.scalabha.tag.hmm

import opennlp.scalabha.tag.OptionalTagDict
import opennlp.scalabha.tag.TagDict._
import opennlp.scalabha.tag.Tagger
import opennlp.scalabha.tag.support.TagEdgeScorer
import opennlp.scalabha.util.CollectionUtil._
import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.util.LogNum
import opennlp.scalabha.util.Pattern
import opennlp.scalabha.util.Pattern.{ ->, :+, +: }
import opennlp.scalabha.tag.TagDict
import opennlp.scalabha.tag.hmm.HmmUtils._
import scala.annotation.tailrec
import scala.collection.breakOut
import opennlp.scalabha.tag.SimpleTagDict

/**
 * Hidden Markov Model for tagging.
 *
 * @tparam Sym	visible symbols in the sequences
 * @tparam Tag	tags applied to symbols
 *
 * @param transitions		function giving the conditional probability of a tag given its previous tag
 * @param emissions			function giving the probability of a symbol given a tag
 * @param tagSet			the set of all possible tags
 *
 * NOTE: Start and end symbols and tags are represented by None.
 */
case class HmmTagger[Sym, Tag](
  transitions: Option[Tag] => Option[Tag] => LogNum,
  emissions: Option[Tag] => Option[Sym] => LogNum,
  tagDict: OptionalTagDict[Sym, Tag],
  validTransitions: Map[Option[Tag], Set[Option[Tag]]])
  extends Tagger[Sym, Tag] {

  private[this] val fastHmmTagger = new FastHmmTagger[Sym, Tag]

  override def tagOptions(rawSequences: Seq[IndexedSeq[Sym]]): Seq[Option[IndexedSeq[(Sym, Tag)]]] = {
    val newSequences = addDistributionsToRawSequences[Sym, Tag](rawSequences, tagDict, transitions, emissions, validTransitions)
    fastHmmTagger.tagAllFast(newSequences).map(Some(_))
  }

  override def tagSequence(sequence: IndexedSeq[Sym]): Option[IndexedSeq[(Sym, Tag)]] = {
    val Seq(newSequence) = addDistributionsToRawSequences[Sym, Tag](Seq(sequence), tagDict, transitions, emissions, validTransitions)
    Some(fastHmmTagger.tagSequenceFast(newSequence))
  }
}

object HmmTagger {
  def apply[Sym, Tag](
    transitions: Option[Tag] => Option[Tag] => LogNum,
    emissions: Option[Tag] => Option[Sym] => LogNum,
    tagDict: OptionalTagDict[Sym, Tag]) = {
    val allTags = tagDict.allTags + None
    new HmmTagger[Sym, Tag](transitions, emissions, tagDict, allTags.mapToVal(allTags).toMap)
  }
}

class FastHmmTagger[Sym, Tag] {

  type OSym = Option[Sym]
  type OTag = Option[Tag]

  type TokTag = (OTag, (Map[OTag, LogNum], LogNum))
  type Tok = (OSym, Vector[TokTag])

  def tagAllFast(sequences: Seq[IndexedSeq[Tok]]): Seq[IndexedSeq[(Sym, Tag)]] = {
    sequences.par.map(tagSequenceFast).seq
  }

  def tagSequenceFast(sequence: IndexedSeq[Tok]): IndexedSeq[(Sym, Tag)] = {
    // viterbi(t)(j) = the probability of the most likely subsequence of states 
    // that accounts for the first t observations and ends in state j.

    // Set the initial values for the fold based on the initial observation
    val startViterbi = Vector[(Option[Tag], LogNum)](None -> LogNum.one)
    val startBackpointers = List[Map[Option[Tag], Option[Tag]]]()

    // Build up backpointers list by calculating viterbi scores for each subsequent observation
    val backpointers =
      (sequence.drop(1)).foldLeft((startViterbi, startBackpointers)) {
        case ((viterbi, backpointers), (currSym, currTags)) =>
          // for each possible tag, get the highest probability previous tag and its score
          val (nextViterbi, currBackpointers) =
            currTags.iterator // each legal tag for the current symbol
              .map {
                case (currTag, (currTagTransitions, currTagEmission)) =>
                  currTag -> viterbi.map {
                    case (prevTag, viterbiScore) =>
                      (prevTag, viterbiScore * currTagTransitions(prevTag) * currTagEmission)
                  }.maxBy(_._2)
              }
              .map { case (prev, (curr, viterbiScore)) => ((prev, viterbiScore), (prev, curr)) }
              .unzip
          (nextViterbi, currBackpointers.toMap :: backpointers)
      }._2

    // Get the optimal tag sequence and map the tag indices back to their string values
    sequence.flatMap(_._1) zipSafe backtrack(backpointers)
  }

  /**
   * Backtrack through the backpointer maps to recover the optimal tag sequence.
   */
  private def backtrack(backpointers: List[Map[OTag, OTag]]): IndexedSeq[Tag] = {
    @tailrec def inner(backpointers: List[Map[OTag, OTag]], curTag: OTag, tags: List[Tag]): List[Tag] =
      backpointers match {
        case Nil => assert(curTag == None); tags
        case currPointers :: previousPointers => inner(previousPointers, currPointers(curTag), curTag.get :: tags)
      }
    val Pattern.Map(None -> lastTag) :: previousPointers = backpointers
    inner(previousPointers, lastTag, Nil).toIndexedSeq
  }

}

//////////////////////////////
// HmmTagger Factories
//////////////////////////////

trait HmmTaggerFactory[Sym, Tag] {
  def apply(
    transitions: Option[Tag] => Option[Tag] => LogNum,
    emissions: Option[Tag] => Option[Sym] => LogNum): HmmTagger[Sym, Tag]
}

class UnconstrainedHmmTaggerFactory[Sym, Tag](tagSet: Set[Tag]) extends HmmTaggerFactory[Sym, Tag] {
  override def apply(
    transitions: Option[Tag] => Option[Tag] => LogNum,
    emissions: Option[Tag] => Option[Sym] => LogNum): HmmTagger[Sym, Tag] = {
    HmmTagger(transitions, emissions, SimpleTagDict(Map(), tagSet).opt)
  }
}

class HardTagDictConstraintHmmTaggerFactory[Sym, Tag](tagDict: OptionalTagDict[Sym, Tag]) extends HmmTaggerFactory[Sym, Tag] {
  override def apply(
    transitions: Option[Tag] => Option[Tag] => LogNum,
    emissions: Option[Tag] => Option[Sym] => LogNum): HmmTagger[Sym, Tag] = {
    HmmTagger(transitions, emissions, tagDict)
  }
}
