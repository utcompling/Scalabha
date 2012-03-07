package opennlp.scalabha.tag.hmm

import org.apache.commons.logging.LogFactory
import opennlp.scalabha.tag.support.CondFreqCounts
import opennlp.scalabha.tag.support.TagDictFactory
import opennlp.scalabha.tag.support.CondFreqCounterFactory
import opennlp.scalabha.tag.Tagger
import opennlp.scalabha.tag.UnsupervisedTaggerTrainer
import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.util.Probability._
import opennlp.scalabha.util.Pattern
import opennlp.scalabha.util.Pattern.{ -> }
import opennlp.scalabha.util.Probability
import opennlp.scalabha.tag.support.SimpleTagDictFactory

/**
 * Factory for training a Hidden Markov Model tagger from a combination of
 * labeled data and unlabeled data using the Expectation-Maximization (EM)
 * algorithm.
 *
 * @tparam Sym	visible symbols in the sequences
 * @tparam Tag	tags applied to symbols
 *
 * @param initialTransitionCounterFactory		factory for generating builders that count tag occurrences and compute distributions for input to EM
 * @param initialEmissionCounterFactory			factory for generating builders that count symbol occurrences and compute distributions for input to EM
 * @param estimatedTransitionCounterFactory		factory for generating builders that count tag occurrences and compute distributions during EM
 * @param estimatedEmissionCounterFactory		factory for generating builders that count symbol occurrences and compute distributions during EM
 * @param startEndSymbol			a unique start/end symbol used internally to mark the beginning and end of a sentence
 * @param startEndTag				a unique start/end tag used internally to mark the beginning and end of a sentence
 * @param maxIterations				maximum number of iterations to be run during EM
 * @param minAvgLogProbChangeForEM	stop iterating EM if change in average log probability is less than this threshold
 * @param tagDictContribToDist		this much of a count will be contributed to calculating the initial frequency distributions
 * @param tagDictFactory			factory for generating tag dictionary from labeled data
 */
class HmmTaggerTrainer[Sym, Tag](
  initialTransitionCounterFactory: CondFreqCounterFactory[Tag, Tag],
  initialEmissionCounterFactory: CondFreqCounterFactory[Tag, Sym],
  estimatedTransitionCounterFactory: CondFreqCounterFactory[Tag, Tag],
  estimatedEmissionCounterFactory: CondFreqCounterFactory[Tag, Sym],
  startEndSymbol: Sym,
  startEndTag: Tag,
  maxIterations: Int = 50,
  minAvgLogProbChangeForEM: Double = 0.00001,
  tagDictFactory: TagDictFactory[Sym, Tag] = new SimpleTagDictFactory[Sym, Tag]())
  extends SupervisedHmmTaggerTrainer[Sym, Tag](initialTransitionCounterFactory, initialEmissionCounterFactory, startEndSymbol, startEndTag, tagDictFactory)
  with UnsupervisedTaggerTrainer[Sym, Tag] {

  protected val LOG = LogFactory.getLog(HmmTaggerTrainer.getClass)

  /**
   * Train a Hidden Markov Model tagger only on unlabeled data using the
   * Expectation-Maximization (EM) algorithm.
   *
   * @param tagDict					a mapping from symbols to their possible tags
   * @param rawTrainSequences		unlabeled sequences to be used as unsupervised training data
   * @return						a trained tagger
   */
  def trainUnsupervised(tagDict: Map[Sym, Set[Tag]], rawTrainSequences: Iterable[IndexedSeq[Sym]]): Tagger[Sym, Tag] = {
    LOG.info("Beginning unsupervised training")
    LOG.info("Tag dict: %d symbols, %.3f avg tags/symbol".format(tagDict.size, tagDict.values.map(_.size.toDouble).avg))

    // Do not assume any known counts -- use only EM-estimated counts
    val initialTransitionCounts = CondFreqCounts[Tag, Tag, Double]()
    val initialEmissionCounts = CondFreqCounts[Tag, Sym, Double]()

    trainFromInitialHmm(tagDict, rawTrainSequences,
      initialTransitionCounts, initialEmissionCounts)
  }

  /**
   * Train a Hidden Markov Model tagger from a combination of labeled data and
   * unlabeled data using the Expectation-Maximization (EM) algorithm.  Use the
   * tagDictFactory to create the tag dictionary from the labeled data.
   *
   * @param rawTrainSequences		unlabeled sequences to be used as unsupervised training data
   * @param taggedTrainSequences	labeled sequences to be used as supervised training data
   * @return						a trained tagger
   */
  override def trainSemisupervised(rawTrainSents: Iterable[IndexedSeq[Sym]], taggedTrainSents: Iterable[IndexedSeq[(Sym, Tag)]]): Tagger[Sym, Tag] = {
    // Make tag dictionary and correct for start/final symbols
    val tagDict = tagDictFactory.make(taggedTrainSents)

    // Train using this tag dictionary
    trainSemisupervised(tagDict, rawTrainSents, taggedTrainSents)
  }

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

    // Get initial counts and probability distributions from the labeled data alone
    val (initialTransitionCounts, initialEmissionCounts) = getCountsFromTagged(taggedTrainSequences)

    trainFromInitialHmm(tagDict, rawTrainSequences, initialTransitionCounts, initialEmissionCounts)
  }

  /**
   * Start EM from an initial set of transition and emission probability
   * distributions.
   *
   * @param tagDict						a mapping from symbols to their possible tags
   * @param rawTrainSequences			unlabeled sequences to be used as unsupervised training data
   * @param initialTransitionCounter	the transition counts to be added to each iteration of EM
   * @param initialEmissionCounter		the emission counts to be added to each iteration of EM
   * @return							a trained tagger
   */
  def trainFromInitialHmm(
    tagDict: Map[Sym, Set[Tag]],
    rawTrainSequences: Iterable[IndexedSeq[Sym]],
    initialTransitionCounts: CondFreqCounts[Tag, Tag, Double],
    initialEmissionCounts: CondFreqCounts[Tag, Sym, Double]) = {

    // Correct tag dictionary for start/final symbols
    val tagDictWithEnds = tagDict + (startEndSymbol -> Set(startEndTag))

    // Create the initial distributions
    val initialTransitions = initialTransitionCounterFactory.get(initialTransitionCounts).toFreqDist
    val initialEmissions = initialEmissionCounterFactory.get(initialEmissionCounts).toFreqDist

    LOG.debug("initialEmissions(NN)(default) = " + initialEmissions("NN".asInstanceOf[Tag])("unknown word".asInstanceOf[Sym]))
    LOG.debug("                    (man)     = " + initialEmissions("NN".asInstanceOf[Tag])("man".asInstanceOf[Sym]))
    LOG.debug("initialEmissions(IN)(default) = " + initialEmissions("IN".asInstanceOf[Tag])("unknown word".asInstanceOf[Sym]))
    LOG.debug("                    (man)     = " + initialEmissions("IN".asInstanceOf[Tag])("man".asInstanceOf[Sym]))

    // Re-estimate probability distributions using EM
    val (transitions, emissions) = reestimateProbabilityDistributions(
      tagDictWithEnds, rawTrainSequences,
      initialTransitionCounts, initialEmissionCounts,
      initialTransitions, initialEmissions)

//    LOG.debug(">>> tagDict(a) = " + tagDict("a".asInstanceOf[Sym]))
//    LOG.debug(">>> emissions(DT)(a) = " + emissions("DT".asInstanceOf[Tag])("a".asInstanceOf[Sym]))
//    LOG.debug(">>>     emissions(DT) = " + emissions("DT".asInstanceOf[Tag]).asInstanceOf[Map[String, Probability]].filter(_._2.toDouble >= 0.01).toList.sortBy(-_._2.toDouble).mapValuesStrict(p => "%.2f".format(p.toDouble)))
//    LOG.debug(">>> emissions(FW)(a) = " + emissions("FW".asInstanceOf[Tag])("a".asInstanceOf[Sym]))
//    LOG.debug(">>> emissions(SYM)(a) = " + emissions("SYM".asInstanceOf[Tag])("a".asInstanceOf[Sym]))

    // Construct the HMM tagger from the estimated probabilities
    new HmmTagger(transitions, emissions, tagDictWithEnds, startEndSymbol, startEndTag)
  }

  /**
   * Re-estimate probability distributions using EM.  Estimate counts for
   * each sequence in rawTrainSequences using the forward/backward procedure.
   * Calculate probability distributions from these counts.  Repeat until
   * convergence.
   */
  protected def reestimateProbabilityDistributions(
    tagDict: Map[Sym, Set[Tag]],
    rawTrainSequences: Iterable[IndexedSeq[Sym]],
    initialTransitionCounts: CondFreqCounts[Tag, Tag, Double],
    initialEmissionCounts: CondFreqCounts[Tag, Sym, Double],
    initialTransitions: Tag => Tag => Probability,
    initialEmissions: Tag => Sym => Probability) = {

    // initial transition and emission probability distributions to be 
    // re-estimated using EM.  
    var transitions = initialTransitions
    var emissions = initialEmissions

    var prevAvgLogProb = Double.NegativeInfinity
    var averageLogProb = Double.NegativeInfinity
    var iteration = 0
    do {
      // update iteration information
      iteration += 1
      prevAvgLogProb = averageLogProb

      // E Step:  Use the forward/backward procedure to determine the 
      //          probability of various possible state sequences for 
      //          generating the training data

      val (expectedTransitionCounter, expectedEmmissionCounter, avgLogProb) =
        estimateCounts(rawTrainSequences, tagDict, transitions, emissions, initialTransitionCounts, initialEmissionCounts)

      // M Step: Use these probability estimates to re-estimate the 
      //         probability distributions

      transitions = expectedTransitionCounter.toFreqDist
      emissions = expectedEmmissionCounter.toFreqDist

      // compute new iteration information
      averageLogProb = avgLogProb
      LOG.info("\t" + iteration + ": " + averageLogProb)

    } while (iteration < maxIterations &&
      (averageLogProb - prevAvgLogProb).abs > minAvgLogProbChangeForEM && //check if converged
      averageLogProb > prevAvgLogProb) // check for divergence

    if ((averageLogProb - prevAvgLogProb).abs < minAvgLogProbChangeForEM)
      LOG.info("DONE: Change in average log probability is less than " + minAvgLogProbChangeForEM)
    if (averageLogProb < prevAvgLogProb)
      LOG.info("DIVERGED: log probability decreased!!")

    (transitions, emissions)
  }

  /**
   * Estimate transition and emission counts for each sequence in
   * rawTrainSequences using the forward/backward procedure.
   */
  protected def estimateCounts(
    rawTrainSequences: Iterable[IndexedSeq[Sym]],
    tagDict: Map[Sym, Set[Tag]],
    transitions: Tag => Tag => Probability,
    emissions: Tag => Sym => Probability,
    initialTransitionCounts: CondFreqCounts[Tag, Tag, Double],
    initialEmissionCounts: CondFreqCounts[Tag, Sym, Double]) = {

    // iterate over all raw training sequences, estimating counts for each
    val estimatedCounts = estimatedCountsBySequence(rawTrainSequences, transitions, emissions, tagDict)

    // Create counters for accumulating the counts that we estimate for each sequence 
    val expectedTransitionCounter = estimatedTransitionCounterFactory.get ++= initialTransitionCounts
    val expectedEmmissionCounter = estimatedEmissionCounterFactory.get ++= initialEmissionCounts

    val (totalSeqProb, numSequences) =
      estimatedCounts.foldLeft((0.0), 0) {
        case ((accumSeqProb, numSequences), (expTransitionCounts, expEmissionCounts, seqProb)) =>
          // update accumulators to keep track of totals across all sequences
          expectedTransitionCounter ++= expTransitionCounts
          expectedEmmissionCounter ++= expEmissionCounts
          (accumSeqProb + seqProb.underlying, numSequences + 1)
      }

    (expectedTransitionCounter, expectedEmmissionCounter, totalSeqProb / numSequences)
  }

  protected def estimatedCountsBySequence(
    rawTrainSequences: Iterable[IndexedSeq[Sym]],
    transitions: Tag => Tag => Probability,
    emissions: Tag => Sym => Probability,
    tagDict: Map[Sym, Set[Tag]]) = {

    rawTrainSequences.par.map { sequence =>
      // estimate counts for this sequence based on current parameters (transmission/emission probability distributions)
      estimateCountsForSequence(sequence, transitions, emissions, tagDict)
    }.seq
  }

  /**
   * Estimate transition and emission counts for the given sequence using
   * the forward/backward procedure.
   */
  protected def estimateCountsForSequence(
    sequence: IndexedSeq[Sym],
    transitions: Tag => Tag => Probability,
    emissions: Tag => Sym => Probability,
    tagDict: Map[Sym, Set[Tag]]) = {

    val (forwards, forwardProb) = forwardProbabilities(sequence, transitions, emissions, tagDict)
    val (backwrds, backwrdProb) = backwrdProbabilities(sequence, transitions, emissions, tagDict)
    assert(forwardProb approx backwrdProb, "forward=%s, backward=%s".format(forwardProb.underlying, backwrdProb.underlying))
    val seqProb = forwardProb // P(sequence | transition,emissions)

    // Get expected transition counts based on forward-backward probabilities
    //        Let expectedTransitionCounts(t)(i)(j) be the probability of being in 
    //            state i at time t and state j at time t+1
    val expectedTransitionCounts = estimateTransitionCounts(sequence, transitions, emissions, tagDict, forwards, backwrds, seqProb)

    // Get expected emission counts based on forward-backward probabilities
    //        Let expectedEmissionCounts(t)(i)(j) be the probability of being in 
    //            state i at time t given the observations and the model
    val expectedEmissionCounts = estimateEmissionCounts(sequence, tagDict, forwards, backwrds, seqProb)

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
    transitions: Tag => Tag => Probability,
    emissions: Tag => Sym => Probability,
    tagDict: Map[Sym, Set[Tag]]): (IndexedSeq[Tag => Probability], Probability) = {

    // Initialization
    //     forward(1)(j) = a(start)(j) * b(j)(o1)   j in [1,N]
    // Recursion
    //     forward(t)(j) = (1 to N).sum(i => forward(t-1)(i) * a(i)(j)) * bj(ot)    j in [1,N], t in [1,T]
    // Termination
    //     P(O | lambda) = forward(final)(sf) = (1 to N).sum(i => forward(T)(i) * aif)

    val startForward = Map(startEndTag -> Probability.one)

    val (lastForward @ Pattern.Map(`startEndTag` -> forwardProb), forwards) =
      (sequence :+ startEndSymbol).foldLeft((startForward, List[Map[Tag, Probability]]())) {
        case ((prevForward, otherForwards), tok) =>
          val currForward =
            tagDict(tok).mapTo { currTag => // each legal tag for the current token
              val tProb =
                prevForward.sumMap {
                  case (prevTag, prevFwdScore) => prevFwdScore * transitions(prevTag)(currTag)
                }
              val eProb = emissions(currTag)(tok)
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
    transitions: Tag => Tag => Probability,
    emissions: Tag => Sym => Probability,
    tagDict: Map[Sym, Set[Tag]]): (IndexedSeq[Tag => Probability], Probability) = {

    // Initialization
    //     backwrd(T)(i) = a(i)(F)   i in [1,N]
    // Recursion
    //     backwrd(t)(i) = (1 to N).sum(j => a(i)(j) * b(j)(o_(t+1)) * backwrd(t+1)(j))    i in [1,N], t in [1,T]
    // Termination
    //     P(O | lambda) = backwrd(1)(s0) = (1 to N).sum(i => a(0)(j) * b(j)(o1) * backwrd(1)(j))

    val finalBackwrd = Map(startEndTag -> Probability.one)

    val (firstBackwrd @ Pattern.Map(`startEndTag` -> backwrdProb), backwrds, lastTok) =
      (startEndSymbol +: sequence).foldRight((finalBackwrd, List[Map[Tag, Probability]](), startEndSymbol)) {
        case (tok, (nextBackwrd, otherBackwrds, nextTok)) =>
          val currBackwrd =
            tagDict(tok).mapTo { currTag =>
              nextBackwrd.sumMap {
                case (nextTag, nextBkwdScore) =>
                  transitions(currTag)(nextTag) * emissions(nextTag)(nextTok) * nextBkwdScore
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
    transitions: Tag => Tag => Probability,
    emissions: Tag => Sym => Probability,
    tagDict: Map[Sym, Set[Tag]],
    forwards: IndexedSeq[Tag => Probability],
    backwrds: IndexedSeq[Tag => Probability],
    seqProb: Probability) = {

    val validTagsByToken = sequence.map(tagDict)

    val nextTokens = sequence :+ startEndSymbol
    val currTagSets = Set(startEndTag) +: validTagsByToken
    val nextTagSets = validTagsByToken :+ Set(startEndTag)
    val currForwards = forwards.dropRight(1)
    val nextBackwrds = backwrds.drop(1)

    val expectedTransitionCounts =
      (nextTokens zipEqual currTagSets zipEqual nextTagSets zipEqual currForwards zipEqual nextBackwrds).map {
        case ((((nextTok, currTags), nextTags), currForward), nextBackwrd) =>
          currTags.mapTo { currTag =>
            nextTags.mapTo { nextTag =>
              (currForward(currTag) * transitions(currTag)(nextTag) * emissions(nextTag)(nextTok) * nextBackwrd(nextTag) / seqProb).toDouble
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
    tagDict: Map[Sym, Set[Tag]],
    forwards: IndexedSeq[Tag => Probability],
    backwrds: IndexedSeq[Tag => Probability],
    seqProb: Probability) = {

    // TODO: Probably not necessary to count start/end states since it's 
    //       always the case that P(endSym|endTag)=1
    val fullSeq = startEndSymbol +: sequence :+ startEndSymbol

    val expectedEmissionCounts =
      (fullSeq zipEqual forwards zipEqual backwrds).map {
        case ((tok, forward), backwrd) =>
          tagDict(tok).mapTo(tag =>
            Map(tok -> (forward(tag) * backwrd(tag) / seqProb).toDouble)).toMap
      }

    expectedEmissionCounts.map(CondFreqCounts(_)).reduce(_ ++ _)
  }
}

