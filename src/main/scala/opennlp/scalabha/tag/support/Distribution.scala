package opennlp.scalabha.tag.support

import opennlp.scalabha.util.LogNum

trait Distribution[T] extends (T => LogNum) {

  def sample(): T

}
