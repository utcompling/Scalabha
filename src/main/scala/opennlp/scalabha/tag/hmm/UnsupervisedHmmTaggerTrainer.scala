package opennlp.scalabha.tag.hmm

import org.apache.commons.logging.LogFactory
import opennlp.scalabha.tag.support._
import opennlp.scalabha.tag._
import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.util.LogNum._
import opennlp.scalabha.util.Pattern
import opennlp.scalabha.util.Pattern.{ -> }
import opennlp.scalabha.util.LogNum
import scala.collection.GenIterable
import opennlp.scalabha.tag.hmm.support._
import scala.annotation.tailrec

/**
 * Factory for training a Hidden Markov Model tagger from a combination of
 * labeled data and unlabeled data using the Expectation-Maximization (EM)
 * algorithm.
 *
 * @tparam Sym	visible symbols in the sequences
 * @tparam Tag	tags applied to symbols
 *
 * @param initialUnsupervisedEmissionDist
 * @param estimatedTransitionCountsTransformer		factory for generating builders that count tag occurrences and compute distributions during EM
 * @param estimatedEmissionCountsTransformer		factory for generating builders that count symbol occurrences and compute distributions during EM
 * @param startEndSymbol							a unique start/end symbol used internally to mark the beginning and end of a sentence
 * @param startEndTag								a unique start/end tag used internally to mark the beginning and end of a sentence
 * @param maxIterations								maximum number of iterations to be run during EM
 * @param minAvgLogProbChangeForEM					stop iterating EM if change in average log probability is less than this threshold
 */
class UnsupervisedHmmTaggerTrainer[Sym, Tag](
  initialUnsupervisedEmissionDist: Tag => Sym => LogNum,
  override protected val estimatedTransitionCountsTransformer: CondCountsTransformer[Tag, Tag],
  override protected val estimatedEmissionCountsTransformer: CondCountsTransformer[Tag, Sym],
  override protected val startEndSymbol: Sym,
  override protected val startEndTag: Tag,
  override protected val maxIterations: Int = 50,
  override protected val minAvgLogProbChangeForEM: Double = 0.00001)
  extends AbstractEmHmmTaggerTrainer[Sym, Tag]
  with UnsupervisedTaggerTrainer[Sym, Tag] {

  /**
   * Train a Hidden Markov Model tagger only on unlabeled data using the
   * Expectation-Maximization (EM) algorithm.
   *
   * @param tagDict					a mapping from symbols to their possible tags
   * @param rawTrainSequences		unlabeled sequences to be used as unsupervised training data
   * @return						a trained tagger
   */
  override def trainUnsupervised(tagDict: Map[Sym, Set[Tag]], rawTrainSequences: Iterable[IndexedSeq[Sym]]): Tagger[Sym, Tag] = {
    LOG.info("Beginning unsupervised training")
    LOG.info("Tag dict: %d symbols, %.3f avg tags/symbol".format(tagDict.size, tagDict.values.map(_.size).avg))

    // Correct tag dictionary for start/final symbols
    val tagDictWithEnds = tagDict + (startEndSymbol -> Set(startEndTag))

    // Create the initial distributions
    val allTags = tagDictWithEnds.values.flatten.toSet
    val initialTransitions = CondFreqDist(DefaultedCondFreqCounts(allTags.mapTo(_ => allTags.mapTo(_ => 1.0).toMap).toMap))
    val initialEmissions = initialUnsupervisedEmissionDist
    val initialHmm = new HmmTagger(initialTransitions, initialEmissions, tagDictWithEnds, startEndSymbol, startEndTag)

    hmmExaminationHook(initialHmm)

    // Re-estimate probability distributions using EM
    // Do not assume any known counts -- use only EM-estimated counts
    trainWithEm(rawTrainSequences, initialHmm)
  }

}

/**
 * Factory for training a Hidden Markov Model tagger from a combination of
 * labeled data and unlabeled data using the Expectation-Maximization (EM)
 * algorithm.
 *
 * @tparam Sym	visible symbols in the sequences
 * @tparam Tag	tags applied to symbols
 *
 * @param initialTransitionCountsTransformer	factory for generating builders that count tag occurrences and compute distributions for input to EM
 * @param initialEmissionCountsTransformer		factory for generating builders that count symbol occurrences and compute distributions for input to EM
 * @param startEndSymbol						a unique start/end symbol used internally to mark the beginning and end of a sentence
 * @param startEndTag							a unique start/end tag used internally to mark the beginning and end of a sentence
 * @param maxIterations							maximum number of iterations to be run during EM
 * @param minAvgLogProbChangeForEM				stop iterating EM if change in average log probability is less than this threshold
 */
class SemisupervisedHmmTaggerTrainer[Sym, Tag](
  initialTransitionCountsTransformer: CondCountsTransformer[Tag, Tag],
  initialEmissionCountsTransformer: CondCountsTransformer[Tag, Sym],
  override protected val estimatedTransitionCountsTransformer: CondCountsTransformer[Tag, Tag],
  override protected val estimatedEmissionCountsTransformer: CondCountsTransformer[Tag, Sym],
  override protected val startEndSymbol: Sym,
  override protected val startEndTag: Tag,
  override protected val maxIterations: Int = 50,
  override protected val minAvgLogProbChangeForEM: Double = 0.00001)
  extends SupervisedHmmTaggerTrainer[Sym, Tag](initialTransitionCountsTransformer, initialEmissionCountsTransformer, startEndSymbol, startEndTag)
  with AbstractEmHmmTaggerTrainer[Sym, Tag]
  with SemisupervisedTaggerTrainer[Sym, Tag] {

  /**
   * Train a Hidden Markov Model tagger from a combination of labeled data and
   * unlabeled data using the Expectation-Maximization (EM) algorithm.  Use
   * the provided tag dictionary instead of creating one from the labeled data.
   *
   * @param tagDict					a mapping from symbols to their possible tags
   * @param rawTrainSequences		unlabeled sequences to be used as unsupervised training data
   * @param taggedTrainSequences	labeled sequences to be used as supervised training data
   * @return						a trained tagger
   */
  override def trainSemisupervised(
    tagDict: Map[Sym, Set[Tag]],
    rawTrainSequences: Iterable[IndexedSeq[Sym]],
    taggedTrainSequences: Iterable[IndexedSeq[(Sym, Tag)]]): Tagger[Sym, Tag] = {

    // Correct tag dictionary for start/final symbols
    val tagDictWithEnds = tagDict + (startEndSymbol -> Set(startEndTag))

    // Get initial counts and probability distributions from the labeled data alone
    val (initialTransitionCounts, initialEmissionCounts) = getCountsFromTagged(taggedTrainSequences)

    // Create the initial HMM
    val initialTransitions = CondFreqDist(initialTransitionCountsTransformer(initialTransitionCounts))
    val initialEmissions = CondFreqDist(initialEmissionCountsTransformer(initialEmissionCounts))
    val initialHmm = new HmmTagger(initialTransitions, initialEmissions, tagDictWithEnds, startEndSymbol, startEndTag)

    hmmExaminationHook(initialHmm)

    // Re-estimate probability distributions using EM
    val hmm =
      trainWithEm(
        rawTrainSequences,
        initialHmm,
        CondFreqCounts(initialTransitionCounts).toDouble, CondFreqCounts(initialEmissionCounts).toDouble)

    hmm
  }

}

/**
 * Factory for training a Hidden Markov Model tagger from a combination of
 * labeled data and unlabeled data using the Expectation-Maximization (EM)
 * algorithm.
 *
 * @tparam Sym	visible symbols in the sequences
 * @tparam Tag	tags applied to symbols
 *
 * @param maxIterations							maximum number of iterations to be run during EM
 * @param minAvgLogProbChangeForEM				stop iterating EM if change in average log probability is less than this threshold
 */
trait AbstractEmHmmTaggerTrainer[Sym, Tag] {
  protected val estimatedTransitionCountsTransformer: CondCountsTransformer[Tag, Tag]
  protected val estimatedEmissionCountsTransformer: CondCountsTransformer[Tag, Sym]
  protected val startEndSymbol: Sym
  protected val startEndTag: Tag
  protected val maxIterations: Int = 50
  protected val minAvgLogProbChangeForEM: Double = 0.00001

  protected val LOG = LogFactory.getLog(classOf[AbstractEmHmmTaggerTrainer[Sym, Tag]])

  def trainWithEm(
    rawTrainSequences: Iterable[IndexedSeq[Sym]],
    initialHmm: HmmTagger[Sym, Tag]): HmmTagger[Sym, Tag] = {

    trainWithEm(
      rawTrainSequences,
      initialHmm,
      CondFreqCounts[Tag, Tag, Double](),
      CondFreqCounts[Tag, Sym, Double]())
  }

  def trainWithEm(
    rawTrainSequences: Iterable[IndexedSeq[Sym]],
    initialHmm: HmmTagger[Sym, Tag],
    initialTransitionCounts: CondFreqCounts[Tag, Tag, Double],
    initialEmissionCounts: CondFreqCounts[Tag, Sym, Double]): HmmTagger[Sym, Tag] = {

    if (LOG.isDebugEnabled) {
      val unknownWord = (rawTrainSequences.flatten.toSet -- initialHmm.tagDict.keySet).headOption

      LOG.debug("    initialEmissions")
      for (w <- List(unknownWord, Some("company"), Some("the")).flatten.map(_.asInstanceOf[Sym])) {
        val probs = initialHmm.tagDict.values.flatten.toSet.mapTo(initialHmm.emissions(_)(w).logValue)
        for ((t, p) <- probs.toList.sortBy(-_._2))
          LOG.debug("        p(%s|%s) = %.2f".format(if (w == unknownWord) "unk" else w, t, p))
      }
    }

    reestimateLogNumDistributions(
      rawTrainSequences: Iterable[IndexedSeq[Sym]],
      initialHmm: HmmTagger[Sym, Tag],
      1, Double.NegativeInfinity,
      initialTransitionCounts: CondFreqCounts[Tag, Tag, Double],
      initialEmissionCounts: CondFreqCounts[Tag, Sym, Double])
  }

  /**
   * Re-estimate probability distributions using EM.  Estimate counts for
   * each sequence in rawTrainSequences using the forward/backward procedure.
   * Calculate probability distributions from these counts.  Repeat until
   * convergence.
   */
  @tailrec
  final protected def reestimateLogNumDistributions(
    rawTrainSequences: Iterable[IndexedSeq[Sym]],
    initialHmm: HmmTagger[Sym, Tag],
    iteration: Int,
    prevAvgLogProb: Double,
    initialTransitionCounts: CondFreqCounts[Tag, Tag, Double],
    initialEmissionCounts: CondFreqCounts[Tag, Sym, Double]): HmmTagger[Sym, Tag] = {

    // E Step:  Use the forward/backward procedure to determine the 
    //          probability of various possible state sequences for 
    //          generating the training data

    val (expectedTransitionCounts, expectedEmmissionCounts, avgLogProb) =
      estimateCounts(rawTrainSequences, initialHmm, initialTransitionCounts, initialEmissionCounts)

    // M Step: Use these probability estimates to re-estimate the 
    //         probability distributions

    val transitions = CondFreqDist(estimatedTransitionCountsTransformer(expectedTransitionCounts.toMap))
    val emissions = CondFreqDist(new StartEndFixingEmissionCountsTransformer(startEndSymbol, startEndTag, estimatedEmissionCountsTransformer)(expectedEmmissionCounts.toMap))
    val hmm = HmmTagger(transitions, emissions, initialHmm.tagDict, initialHmm.startEndSymbol, initialHmm.startEndTag)

    LOG.info("\t" + iteration + ": " + avgLogProb)

    hmmExaminationHook(hmm)

    // Check each ending condition
    if (iteration >= maxIterations) {
      LOG.info("DONE: Max number of iterations reached")
      hmm
    }
    else if ((avgLogProb - prevAvgLogProb).abs < minAvgLogProbChangeForEM) { //check if converged
      LOG.info("DONE: Change in average log probability is less than " + minAvgLogProbChangeForEM)
      hmm
    }
    else if (avgLogProb < prevAvgLogProb) {
      throw new RuntimeException("DIVERGED: log probability decreased on iteration %d".format(iteration))
    }
    else if (avgLogProb == Double.NegativeInfinity) {
      throw new RuntimeException("averageLogProb == -Infinity on iteration %d".format(iteration))
    }
    else {
      // No ending condition met, re-estimate
      reestimateLogNumDistributions(
        rawTrainSequences: Iterable[IndexedSeq[Sym]],
        hmm: HmmTagger[Sym, Tag],
        iteration + 1, avgLogProb,
        initialTransitionCounts: CondFreqCounts[Tag, Tag, Double],
        initialEmissionCounts: CondFreqCounts[Tag, Sym, Double])
    }
  }

  /**
   * Estimate transition and emission counts for each sequence in
   * rawTrainSequences using the forward/backward procedure.
   *
   * TODO: This method should be rewritten as a MapReduce job.
   */
  protected def estimateCounts(
    rawTrainSequences: Iterable[IndexedSeq[Sym]],
    hmm: HmmTagger[Sym, Tag],
    initialTransitionCounts: CondFreqCounts[Tag, Tag, Double],
    initialEmissionCounts: CondFreqCounts[Tag, Sym, Double]) = {

    val (expectedTransitionCounts, expectedEmissionCounts, totalSeqProb, numSequences) =
      rawTrainSequences.par
        .map {
          sequence =>
            val (estTrCounts, estEmCounts, seqProb) = estimateCountsForSequence(sequence, hmm)
            (estTrCounts, estEmCounts, seqProb.logValue, 1) // number of sentences == 1
        }
        .fold((CondFreqCounts[Tag, Tag, Double](), CondFreqCounts[Tag, Sym, Double](), 0., 0)) {
          case ((aTC, aEC, aP, aN), (bTC, bEC, bP, bN)) =>
            (aTC ++ bTC, aEC ++ bEC, aP + bP, aN + bN) // sum up all the components
        }

    (expectedTransitionCounts ++ initialTransitionCounts, expectedEmissionCounts ++ initialEmissionCounts, totalSeqProb / numSequences)
  }

  /**
   * Estimate transition and emission counts for the given sequence using
   * the forward/backward procedure.
   */
  protected def estimateCountsForSequence(
    sequence: IndexedSeq[Sym],
    hmm: HmmTagger[Sym, Tag]) = {

    val (forwards, forwardProb) = forwardProbabilities(sequence, hmm)
    val (backwrds, backwrdProb) = backwrdProbabilities(sequence, hmm)
    assert(forwardProb approx backwrdProb, "forward=%s, backward=%s".format(forwardProb.logValue, backwrdProb.logValue))
    val seqProb = forwardProb // P(sequence | transition,emissions)

    // Get expected transition counts based on forward-backward probabilities
    //        Let expectedTransitionCounts(t)(i)(j) be the probability of being in 
    //            state i at time t and state j at time t+1
    val expectedTransitionCounts = estimateTransitionCounts(sequence, hmm, forwards, backwrds, seqProb)

    // Get expected emission counts based on forward-backward probabilities
    //        Let expectedEmissionCounts(t)(i)(j) be the probability of being in 
    //            state i at time t given the observations and the model
    val expectedEmissionCounts = estimateEmissionCounts(sequence, hmm, forwards, backwrds, seqProb)

    (expectedTransitionCounts, expectedEmissionCounts, seqProb)
  }

  /**
   * Calculate forward probabilities for the sequence based on the existing
   * transition and emission probabilities.
   *
   * Let forward(t)(j) be the probability of being in state j after seeing the
   *     first t observations (by summing over all initial paths leading to j).
   *
   *             forward(t)(j) = P(o1,o2,...,ot, q_t=j | lambda)
   */
  protected def forwardProbabilities(
    sequence: IndexedSeq[Sym],
    hmm: HmmTagger[Sym, Tag]): (IndexedSeq[Tag => LogNum], LogNum) = {

    // Initialization
    //     forward(1)(j) = a(start)(j) * b(j)(o1)   j in [1,N]
    // Recursion
    //     forward(t)(j) = (1 to N).sum(i => forward(t-1)(i) * a(i)(j)) * bj(ot)    j in [1,N], t in [1,T]
    // Termination
    //     P(O | lambda) = forward(final)(sf) = (1 to N).sum(i => forward(T)(i) * aif)

    val startEndTag = hmm.startEndTag
    val startForward = Map(startEndTag -> LogNum.one)

    val (lastForward @ Pattern.Map(`startEndTag` -> forwardProb), forwards) =
      (sequence :+ hmm.startEndSymbol).foldLeft((startForward, List[Map[Tag, LogNum]]())) {
        case ((prevForward, otherForwards), tok) =>
          val currForward =
            hmm.tagDict(tok).mapTo { currTag => // each legal tag for the current token
              val tProb =
                prevForward.sumMap {
                  case (prevTag, prevFwdScore) => prevFwdScore * hmm.transitions(prevTag)(currTag)
                }
              val eProb = hmm.emissions(currTag)(tok)
              tProb * eProb
            }.toMap
          (currForward, prevForward :: otherForwards)
      }

    ((lastForward :: forwards).reverse.toIndexedSeq, forwardProb)
  }

  /**
   * Calculate backward probabilities for the sequence based on the existing
   * transition and emission probabilities.
   *
   * Let backwrd(t)(j) be the probability of observing the final set of observations
   *     from time t+1 to T given that one is in state i at time t
   *
   *             backwrd(j) = P(o1,o2,...,ot, q_t=j | lambda)
   */
  protected def backwrdProbabilities(
    sequence: IndexedSeq[Sym],
    hmm: HmmTagger[Sym, Tag]): (IndexedSeq[Tag => LogNum], LogNum) = {

    // Initialization
    //     backwrd(T)(i) = a(i)(F)   i in [1,N]
    // Recursion
    //     backwrd(t)(i) = (1 to N).sum(j => a(i)(j) * b(j)(o_(t+1)) * backwrd(t+1)(j))    i in [1,N], t in [1,T]
    // Termination
    //     P(O | lambda) = backwrd(1)(s0) = (1 to N).sum(i => a(0)(j) * b(j)(o1) * backwrd(1)(j))

    val startEndTag = hmm.startEndTag
    val finalBackwrd = Map(startEndTag -> LogNum.one)

    val (firstBackwrd @ Pattern.Map(`startEndTag` -> backwrdProb), backwrds, lastTok) =
      (hmm.startEndSymbol +: sequence).foldRight((finalBackwrd, List[Map[Tag, LogNum]](), hmm.startEndSymbol)) {
        case (tok, (nextBackwrd, otherBackwrds, nextTok)) =>
          val currBackwrd =
            hmm.tagDict(tok).mapTo { currTag =>
              nextBackwrd.sumMap {
                case (nextTag, nextBkwdScore) =>
                  hmm.transitions(currTag)(nextTag) * hmm.emissions(nextTag)(nextTok) * nextBkwdScore
              }
            }.toMap
          (currBackwrd, nextBackwrd :: otherBackwrds, tok)
      }

    ((firstBackwrd :: backwrds).toIndexedSeq, backwrdProb)
  }

  /**
   * Estimate transition counts for the sequence based on forward and
   * backward probabilities.
   *
   *    estTrans(i,j) = sum_t(fwd(t)(i) * a(i)(j) * b(o(t+1)) * bkw(t+1)(j) / seqProb)
   */
  protected def estimateTransitionCounts(
    sequence: IndexedSeq[Sym],
    hmm: HmmTagger[Sym, Tag],
    forwards: IndexedSeq[Tag => LogNum],
    backwrds: IndexedSeq[Tag => LogNum],
    seqProb: LogNum) = {

    val validTagsByToken = sequence.map(hmm.tagDict)

    val nextTokens = sequence :+ hmm.startEndSymbol
    val currTagSets = Set(hmm.startEndTag) +: validTagsByToken
    val nextTagSets = validTagsByToken :+ Set(hmm.startEndTag)
    val currForwards = forwards.dropRight(1)
    val nextBackwrds = backwrds.drop(1)

    val expectedTransitionCounts =
      (nextTokens zipEqual currTagSets zipEqual nextTagSets zipEqual currForwards zipEqual nextBackwrds).map {
        case ((((nextTok, currTags), nextTags), currForward), nextBackwrd) =>
          currTags.mapTo { currTag =>
            nextTags.mapTo { nextTag =>
              (currForward(currTag) * hmm.transitions(currTag)(nextTag) * hmm.emissions(nextTag)(nextTok) * nextBackwrd(nextTag) / seqProb).toDouble
            }.toMap
          }.toMap
      }

    expectedTransitionCounts.map(CondFreqCounts(_)).reduce(_ ++ _)
  }

  /**
   * Estimate emission counts for the sequence based on forward and
   * backward probabilities.
   *
   *   estEmiss(t)(j) = fwd(t)(j) * bkw(t)(j) / seqProb
   */
  protected def estimateEmissionCounts(
    sequence: IndexedSeq[Sym],
    hmm: HmmTagger[Sym, Tag],
    forwards: IndexedSeq[Tag => LogNum],
    backwrds: IndexedSeq[Tag => LogNum],
    seqProb: LogNum) = {

    // TODO: Probably not necessary to count start/end states since it's 
    //       always the case that P(endSym|endTag)=1
    val fullSeq = hmm.startEndSymbol +: sequence :+ hmm.startEndSymbol

    val expectedEmissionCounts =
      (fullSeq zipEqual forwards zipEqual backwrds).map {
        case ((tok, forward), backwrd) =>
          hmm.tagDict(tok).mapTo(tag =>
            Map(tok -> (forward(tag) * backwrd(tag) / seqProb).toDouble)).toMap
      }

    expectedEmissionCounts.map(CondFreqCounts(_)).reduce(_ ++ _)
  }

  protected def hmmExaminationHook(hmm: HmmTagger[Sym, Tag]) {

  }
}

