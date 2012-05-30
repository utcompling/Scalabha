package opennlp.scalabha.util

import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.util.LogNum
import opennlp.scalabha.util.LogNum._
import scalanlp.stats.distributions.Dirichlet

object Stats {

  case class DirichletSampler[L, N: Numeric](labels: Iterable[L], pseudocounts: L => N) {
    private[this] val labelList = labels.toList
    private[this] val dirichlet = Dirichlet(labelList.map(l => implicitly[Numeric[N]].toDouble(pseudocounts(l))).toArray)

    def sample: Map[L, LogNum] = (labelList zipEqual dirichlet.sample.toList.map(_.toLogNum)).toMap
  }

}
