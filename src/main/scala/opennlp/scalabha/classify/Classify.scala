package opennlp.scalabha.classify

import opennlp.scalabha.util.Probability

/**
 * Trait for documents with fields and features
 * @tparam T the type of value used for features; should be immutable and
 *   serializable
 */
trait Document[T] {

  /**
   * The possibly different fields of a document; this is to support different
   * views of a document (e.g. link text vs. document body text) which may
   * be treated seperately by a classifier; currently the features for each
   * field must be of the same type
   */
  def fields:Iterable[(String, Iterable[T])]

  /**
   * A concatentation of the features from all the fields
   */
  def allFeatures = fields.map(_._2).flatten
}

/**
 * Trait for document classifiers
 * @tparam L the class of labels used by this classifier
 * @tparam T the ypt of value used for the features
 */
trait Classifier[L,T] {

  /** Iterate through the set of labels for this classifier */
  def labels:Iterable[L]

  /** 
   * Classify a document
   * @return a sequence of labels with their double-valued scores
   */
  def classify(document:Document[T]):Iterable[(L,Probability)]

  /** Return the best label provided this classifier */
  def bestLabel(document:Document[T]) = classify(document).maxBy(_._2)
}

/**
 * Trait for classifier trainers
 * @tparam L the class of labels used by the classifiers created
 * @tparam T the class of value used for the features
 */
trait ClassifierTrainer[L,T] {
  def train(documents:Iterator[(L,Document[T])]):Classifier[L,T]
}

/**
 * Trait for classifier trainers which use distributions over the labels
 * rather than hard label annotations to train new classifiers
 * @tparam L the class of labels used by the classifiers created
 * @tparam T the class of value used for the features
 */
trait SoftClassifierTrainer[L,T] {
  def train(documents:Iterable[(Iterable[(L,Probability)],_ <: Document[T])])
  :Classifier[L,T]
}
