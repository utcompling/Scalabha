package opennlp.scalabha.tag.support

import opennlp.scalabha.util.LogNum

trait DiscreteDistribution[T] extends (T => LogNum) {

  def sample(): T

}
