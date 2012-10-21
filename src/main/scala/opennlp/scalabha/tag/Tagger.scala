package opennlp.scalabha.tag

import opennlp.scalabha.util.CollectionUtil._
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
  final def tag(rawSequences: Vector[Vector[Sym]]): Vector[Vector[(Sym, Tag)]] =
    (rawSequences zipSafe tagOptions(rawSequences)).mapt((ws, tagged) =>
      tagged.getOrElse(throw new RuntimeException("could not tag sentence: '%s'".format(ws.mkString(" ")))))

  /**
   * Tag each sequence using this model.
   *
   * @param rawSequences	unlabeled data to be tagged
   * @return				sequences tagged by the model
   */
  def tagOptions(rawSequences: Vector[Vector[Sym]]): Vector[Option[Vector[(Sym, Tag)]]]

  /**
   * Tag the sequence using this model.
   *
   * @param sequence 	a single sequence to be tagged
   * @return			the tagging of the input sequence assigned by the model
   */
  def tagSequence(sequence: Vector[Sym]): Option[Vector[(Sym, Tag)]]

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
  final def train(taggedSequences: Vector[Vector[(Sym, Tag)]]): Tagger[Sym, Tag] = {
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
    rawSequences: Vector[Vector[Sym]],
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
    rawSequences: Vector[Vector[Sym]],
    goldTaggedSequences: Vector[Vector[(Sym, Tag)]],
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
    rawSequences: Vector[Vector[Sym]],
    noisyTaggedSequences: Vector[Vector[(Sym, Tag)]],
    tagDict: TagDict[Sym, Tag]): Tagger[Sym, Tag]

}
