package opennlp.scalabha.util

import com.cloudera.scrunch.PCollection
import com.cloudera.scrunch.Pipeline
import com.cloudera.scrunch.From
import com.cloudera.scrunch.PTable

object Scrunch {

  class EnhancedPTable[K, V](self: PTable[K, V]) {
    def filterKeys(p: K => Boolean) = {
      self.filter((k, v) => p(k))
    }

    def filterNotKeys(p: K => Boolean) = {
      filterKeys(!p(_))
    }
  }
  implicit def enhancePTable[K, V](self: PTable[K, V]) = new EnhancedPTable(self)

  case class pipeline[R](block: Pipeline[EnhancedPipeline[String]] => PCollection[R]) {
    def to(target: String) {
      val pipeline = new Pipeline[EnhancedPipeline[String]]
      val pcollect = block(pipeline)
      pipeline.writeTextFile(pcollect, target)
      pipeline.done
    }

    def materialize = {
      val pipeline = new Pipeline[EnhancedPipeline[String]]
      val pcollect = block(pipeline)
      val result = pcollect.materialize
      pipeline.done
    }
  }

  class EnhancedPipeline[T](self: Pipeline[T]) {
    def from(source: String) = self.read(From.textFile(source))
  }
  implicit def enhancePipeline[T](self: Pipeline[T]) = new EnhancedPipeline(self)

}
