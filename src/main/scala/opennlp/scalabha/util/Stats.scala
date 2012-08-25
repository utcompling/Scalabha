package opennlp.scalabha.util

import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.util.LogNum._
import breeze.stats.distributions.Dirichlet

object Stats {

  case class DirichletSampler[L, N: Numeric](labels: Iterable[L], pseudocounts: L => N) {
    private[this] val labelList = labels.toIndexedSeq
    private[this] val dirichlet = Dirichlet(labelList.map(l => implicitly[Numeric[N]].toDouble(pseudocounts(l))).toArray)

    def sample: Map[L, LogNum] = (labelList zipSafe dirichlet.sample.valuesIterator.map(_.toLogNum)).toMap
  }
  object DirichletSampler {
    def apply[L, N: Numeric](pseudocounts: Map[L, N]) = new DirichletSampler(pseudocounts.keys, pseudocounts)
  }

}
