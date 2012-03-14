package opennlp.scalabha.tag

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
  def tag(rawSequences: Iterable[IndexedSeq[Sym]]): Iterable[IndexedSeq[(Sym, Tag)]] =
    (rawSequences zip rawSequences.map(tagSequence)).map(_.zipped.toIndexedSeq)

  /**
   * Tag the sequence using this model.  Uses the Viterbi algorithm.
   *
   * @param sequence 	a single sequence to be tagged
   * @return			the tagging of the input sequence assigned by the model
   */
  def tagSequence(sequence: IndexedSeq[Sym]): List[Tag]

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
  def trainSupervised(taggedTrainSequences: Iterable[IndexedSeq[(Sym, Tag)]], tagDict: Map[Sym, Set[Tag]]): Tagger[Sym, Tag]

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
  def trainUnsupervised(tagDict: Map[Sym, Set[Tag]], rawTrainSequences: Iterable[IndexedSeq[Sym]]): Tagger[Sym, Tag]

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
  def trainSemisupervised(tagDict: Map[Sym, Set[Tag]], rawTrainSequences: Iterable[IndexedSeq[Sym]], taggedTrainSequences: Iterable[IndexedSeq[(Sym, Tag)]]): Tagger[Sym, Tag]

}
