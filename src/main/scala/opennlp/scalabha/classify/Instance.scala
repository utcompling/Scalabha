package opennlp.scalabha.classify

import scala.io.Source

/**
 * Trait for instances with attributes and feature values
 * @tparam T the type of value used for features; should be immutable and
 *   serializable
 */
trait Instance[T] {

  /**
   * The possibly different features of a document; this is to support different
   * views of a document (e.g. link text vs. document body text) which may
   * be treated separately by a classifier; currently the features for each
   * attribute must be of the same type.
   */
  def fields: Iterable[(String, Iterable[T])]

  /**
   * A concatenation of the features from all the fields
   */
  lazy val allFeatures = fields.flatMap(_._2)
}

/**
 * A default instance that has only one attribute type.
 */
class DefaultInstance[T](features: Iterable[T]) extends Instance[T] {
  def fields = List(("default", features))
}


trait LabeledInstanceSource[L, T] {
  def getLabeledInstances: Iterator[(L, Instance[T])]
}

class CsvLabeledInstanceSource(inputSource: Source)
  extends LabeledInstanceSource[String, String] {

  def getLabeledInstances = {
    inputSource.getLines.map { line =>
      val atts = line.split(",")
      val label = atts.last
      val features = atts.dropRight(1)
      (label, new DefaultInstance(features))
    }
  }
}

case class AttrVal(val attr: String, val value: String) {
  override def toString = attr + "=" + value
}

