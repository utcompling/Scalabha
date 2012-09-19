package opennlp.scalabha.util

import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.util.CollectionUtil._
import opennlp.scalabha.util.LogNum._
import breeze.stats.distributions.Dirichlet
import breeze.stats.distributions.Rand
import breeze.stats.distributions.RandBasis
import breeze.math.TensorSpace
import breeze.linalg.DenseVector

object Stats {

  case class DirichletSampler[L, N](labels: Iterable[L], pseudocounts: L => N)(implicit num: Numeric[N], rand: RandBasis = Rand) {
    private[this] val labelList = labels.toIndexedSeq
    private[this] val dirichlet = {
      implicit def randBasis = rand
      Dirichlet.apply(DenseVector(labelList.map(l => num.toDouble(pseudocounts(l))).toArray)) //(implicitly[TensorSpace[L, Int, Double]], rand)
    }

    def sample: Map[L, LogNum] = (labelList zipSafe dirichlet.sample.valuesIterator.map(_.toLogNum)).toMap
  }
  object DirichletSampler {
    def apply[L, N](pseudocounts: Map[L, N])(implicit num: Numeric[N]): DirichletSampler[L,N] = DirichletSampler(pseudocounts, Rand)(num)
    def apply[L, N](pseudocounts: Map[L, N], rand: RandBasis)(implicit num: Numeric[N]): DirichletSampler[L,N] = new DirichletSampler(pseudocounts.keys, pseudocounts)(num, rand)
  }

}
