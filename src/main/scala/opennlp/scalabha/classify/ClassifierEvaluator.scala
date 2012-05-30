package opennlp.scalabha.classify

import scala.collection.mutable.{ Map => MMap }

import org.apache.commons.logging.LogFactory

import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.util.LogNum._

trait ClassifierEvaluator[L, T] {
  def apply(classifier: Classifier[L, T]): Seq[ClassifierEvaluatorResult[L]]
}

class SimpleClassifierEvaluator[L, T](gold: Seq[(L, Seq[T])]) extends ClassifierEvaluator[L, T] {
  private val LOG = LogFactory.getLog("ClassifierEvaluator")

  override def apply(classifier: Classifier[L, T]) = {
    var correct = 0
    var total = 0
    val mistakes = MMap[L, MMap[L, Int]]() // expected, model

    val results = gold.map { case (l, d) => classifier.classify(new DefaultInstance(d)).maxBy(_._2)._1 }
    for (((l, _), r) <- gold zipEqual results) {
      if (r == l)
        correct += 1
      else {
        mistakes.getOrElseUpdate(l, MMap()).getOrElseUpdate(r, 0)
        mistakes(l)(r) += 1
      }
      total += 1
    }

    Seq(ClassifierEvaluatorResult(correct, total, mistakes.mapValuesStrict(_.toMap).toMap, results))
  }
}

case class ClassifierEvaluatorResult[L](
  val correct: Int,
  val total: Int,
  val mistakes: Map[L, Map[L, Int]],
  val results: Seq[L]) {

  def resultsDist = results.counts.normalizeValues
  def accuracyStr = "accuracy = %s (%s/%s)".format(correct.toDouble / total, correct, total)

  override def toString = {
    val sb = new StringBuilder
    sb.append(accuracyStr + "\n")
    if (total - correct > 0) {
      sb.append("  mistakes\n")
      sb.append("    gold\tmodel\tcount\n")
      for ((l, (r, c)) <- mistakes.ungroup.toList.sortBy { case (l, (r, c)) => -c })
        sb.append("    %s\t%s\t%s (%s)\n".format(l, r, c, c.toDouble / (total - correct)))
    }
    sb.append("  model output dist = %s\n".format(resultsDist))
    sb.result
  }
}

class CompositeClassifierEvaluator[L, T](evals: Seq[ClassifierEvaluator[L, T]]) extends ClassifierEvaluator[L, T] {
  override def apply(classifier: Classifier[L, T]) = evals.flatMap(_(classifier))
}
object CompositeClassifierEvaluator {
  def apply[L, T](gold: Seq[(L, Seq[T])]*) =
    new CompositeClassifierEvaluator(gold.filter(_.nonEmpty).map(new SimpleClassifierEvaluator(_)))
}
