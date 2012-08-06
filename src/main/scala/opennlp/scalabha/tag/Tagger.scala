package opennlp.scalabha.tag

import opennlp.scalabha.util.CollectionUtils._

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
   * @param taggedTrainSequences	labeled sequences to use for training the model
   * @param tagDict					a tag dictionary
   * @return						a trained Tagger
   */
  def trainSupervised(taggedTrainSequences: Iterable[IndexedSeq[(Sym, Tag)]]): Tagger[Sym, Tag]

}

/**
 * Factory for training a Tagger from a combination from unlabeled data.
 *
 * @tparam Sym	visible symbols in the sequences
 * @tparam Tag	tags applied to symbols
 */
trait UnsupervisedTaggerTrainer[Sym, Tag] {

  /**
   * Train a Tagger only on unlabeled data using the Expectation-Maximization (EM)
   * algorithm.
   *
   * @param tagDict					a mapping from symbols to their possible tags
   * @param rawTrainSequences		unlabeled sequences to be used as unsupervised training data
   * @return						a trained Tagger
   */
  def trainUnsupervised(tagDict: TagDict[Sym, Tag], rawTrainSequences: Iterable[IndexedSeq[Sym]]): Tagger[Sym, Tag]

}

/**
 * Factory for training a Tagger from a combination from unlabeled data.
 *
 * @tparam Sym	visible symbols in the sequences
 * @tparam Tag	tags applied to symbols
 */
trait SemisupervisedTaggerTrainer[Sym, Tag] {

  /**
   * Train a Tagger from a combination of labeled data and unlabeled data
   * using the Expectation-Maximization (EM) algorithm.  Use the provided tag
   * dictionary instead of creating one from the labeled data.
   *
   * @param tagDict					a mapping from symbols to their possible tags
   * @param rawTrainSequences		unlabeled sequences to be used as unsupervised training data
   * @param taggedTrainSequences	labeled sequences to be used as supervised training data
   * @return						a trained tagger
   */
  def trainSemisupervised(tagDict: TagDict[Sym, Tag], rawTrainSequences: Iterable[IndexedSeq[Sym]], taggedTrainSequences: Iterable[IndexedSeq[(Sym, Tag)]]): Tagger[Sym, Tag]

}
