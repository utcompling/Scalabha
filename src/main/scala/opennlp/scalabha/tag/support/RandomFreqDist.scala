package opennlp.scalabha.tag.support

import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.util.Probability
import opennlp.scalabha.util.Probability._
import collection.mutable.{ Map => MMap }
import util.Random

/**
 * A frequency distribution that generates and returns random
 * probabilities when queried.  Returned values are memorized so that
 * future requests for the same keys yield the same values.
 */
class RandomFreqDist[B] extends (B => Probability) {

  private val mem = MMap[B, Probability]()
  private val rand = new Random(0) // static seed ensures results are reproducible

  override def apply(b: B) = {
    mem.getOrElseUpdate(b, Probability(rand.nextDouble))
  }

}

/**
 * A conditional frequency distribution that generates and returns random
 * probabilities when queried.  Returned values are memorized so that
 * future requests for the same keys yield the same values.
 */
class RandomCondFreqDist[A, B] extends (A => B => Probability) {

  private val mem = MMap[A, RandomFreqDist[B]]()

  override def apply(a: A) = {
    mem.getOrElseUpdate(a, new RandomFreqDist())
  }

}
