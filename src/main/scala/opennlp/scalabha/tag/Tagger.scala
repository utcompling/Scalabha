package opennlp.scalabha.tag

import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.tag.hmm.HmmUtils

/**
 * Tag sequences of symbols.
 *
 * @tparam Sym	visible symbols in the sequences
 * @tparam Tag	tags applied to symbols
 */
trait Tagger[Sym, Tag] {

  /**
   * Tag each sequence using this model.
   *
   * @param rawSequences	unlabeled data to be tagged
   * @return				sequences tagged by the model
   */
  final def tag(rawSequences: Seq[IndexedSeq[Sym]]): Seq[IndexedSeq[(Sym, Tag)]] =
    (rawSequences zip tagAll(rawSequences)).mapt((ws, tagged) =>
      tagged.getOrElse(throw new RuntimeException("could not tag sentence: '%s'".format(ws.mkString(" ")))))

  /**
   * Tag each sequence using this model.
   *
   * @param rawSequences	unlabeled data to be tagged
   * @return				sequences tagged by the model
   */
  final def tagAll(rawSequences: Seq[IndexedSeq[Sym]]): Seq[Option[IndexedSeq[(Sym, Tag)]]] =
    (rawSequences zip tags(rawSequences)).map { case (ws, ts) => ts.map(ws zip _) }

  /**
   * Tag each sequence using this model.
   *
   * @param rawSequences	unlabeled data to be tagged
   * @return				sequences of tags returned from the model
   */
  protected def tags(rawSequences: Seq[IndexedSeq[Sym]]): Seq[Option[IndexedSeq[Tag]]] =
    rawSequences.map(tagSequence)

  /**
   * Tag the sequence using this model.
   *
   * @param sequence 	a single sequence to be tagged
   * @return			the tagging of the input sequence assigned by the model
   */
  protected def tagSequence(sequence: IndexedSeq[Sym]): Option[IndexedSeq[Tag]] = sys.error("not implemented")

}

/**
 * Factory for training a Tagger directly from labeled data.
 *
 * @tparam Sym	visible symbols in the sequences
 * @tparam Tag	tags applied to symbols
 */
trait SupervisedTaggerTrainer[Sym, Tag] {

  /**
   * Train a Tagger directly from labeled data.
   *
   * @param taggedSequences		labeled sequences to use for training the model
   * @param tagDict				a tag dictionary
   * @return					a trained Tagger
   */
  final def train(taggedSequences: Iterable[IndexedSeq[(Sym, Tag)]]): Tagger[Sym, Tag] = {
    val (transitionCounts, emissionCounts) = HmmUtils.getCountsFromTagged(taggedSequences)
    makeTagger(transitionCounts, emissionCounts)
  }

  /**
   * Train a Tagger directly from counts.
   *
   * @param taggedSequences		labeled sequences to use for training the model
   * @param tagDict				a tag dictionary
   * @return					a trained Tagger
   */
  def makeTagger[N: Numeric](transitionCounts: Map[Option[Tag], Map[Option[Tag], N]], emissionCounts: Map[Option[Tag], Map[Option[Sym], N]]): Tagger[Sym, Tag]

}

/**
 * Factory for training a Tagger from unlabeled data.
 *
 * @tparam Sym	visible symbols in the sequences
 * @tparam Tag	tags applied to symbols
 */
trait TypesupervisedTaggerTrainer[Sym, Tag] {

  /**
   * Train a Tagger only on unlabeled data.  Initialize a starting point
   * (initial tagging) from random uniformed draws from the tag dictionary.
   *
   * @param rawSequences			unlabeled sequences to be used as unsupervised training data
   * @param tagDict					a mapping from symbols to their possible tags
   * @return						a trained Tagger
   */
  def train(
    rawSequences: Iterable[IndexedSeq[Sym]],
    tagDict: TagDict[Sym, Tag]): Tagger[Sym, Tag]

  /**
   * Train a Tagger from a combination of unlabeled data and GOLD labeled data.
   * The labeled data is ONLY used for prior counts; it is NOT iterated over
   * like the raw data.  The tagged data could even be EMPTY.
   *
   * @param rawSequences			unlabeled sequences to be used as unsupervised training data
   * @param goldTaggedSequences		labeled sequences to be used as supervised training data
   * @param tagDict					a mapping from symbols to their possible tags
   * @return						a trained Tagger
   */
  def trainWithSomeGoldLabeled(
    rawSequences: Iterable[IndexedSeq[Sym]],
    goldTaggedSequences: Iterable[IndexedSeq[(Sym, Tag)]],
    tagDict: TagDict[Sym, Tag]): Tagger[Sym, Tag]

  /**
   * Train a Tagger from a combination of unlabeled data and NOISY labeled data.
   * The labeled data is NOT used for prior counts, NOR is it iterated over
   * like the raw data.  The tagged data could even be EMPTY.
   *
   * @param rawSequences			unlabeled sequences to be used as unsupervised training data
   * @param noisyTaggedSequences	labeled sequences to be used as supervised training data
   * @param tagDict					a mapping from symbols to their possible tags
   * @return						a trained Tagger
   */
  def trainWithSomeNoisyLabeled(
    rawSequences: Iterable[IndexedSeq[Sym]],
    noisyTaggedSequences: Iterable[IndexedSeq[(Sym, Tag)]],
    tagDict: TagDict[Sym, Tag]): Tagger[Sym, Tag]

}
