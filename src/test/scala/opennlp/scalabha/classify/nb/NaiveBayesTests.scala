package opennlp.scalabha.classify.nb

import scala.io._
import org.scalatest.FunSuite
import opennlp.scalabha.classify._

class MockDocument[T](features:Iterable[T]) extends Document[T] {
  def fields = List(("content", features))
}

object MockDocument {
  def apply[T](features:Iterable[T]) = new MockDocument(features)
}

class NaiveBayesTestSuite extends FunSuite {
  test("creates correct model from tennis.train") {
    val trainer = nb.OnlineNaiveBayesClassifierTrainer[String,String](0)
    val labeledDocs = Source.fromURL(getClass.getResource("/tennis.train.csv"))
                     .getLines.map(line => {
                       val atts = line.split(",")
                       val label = atts(4)
                       val features = atts.slice(0, 4)
                       (label, MockDocument(features))
                     })
    val classifier = trainer.train(labeledDocs)

    // verify expected basic feature counts
    assert(classifier.docCount === 14)
    assert(classifier.labelDocCount("No") === 5)
    assert(classifier.labelDocCount("Yes") === 9)
    assert(classifier.labelFeatureCount("Yes", "Outlook=Overcast") === 4)
    assert(classifier.labelFeatureCount("No", "Outlook=Rain") === 2)
    assert(classifier.labelFeatureCount("Yes", "Outlook=Sunny") === 2)
    assert(classifier.labelFeatureCount("No", "Outlook=Sunny") === 3)
    assert(classifier.labelFeatureCount("No", "Temperature=Cool") === 1)
    assert(classifier.labelFeatureCount("Yes", "Temperature=Cool") === 3)
    assert(classifier.labelFeatureCount("No", "Temperature=Hot") === 2)
    assert(classifier.labelFeatureCount("Yes", "Temperature=Hot") === 2)
    assert(classifier.labelFeatureCount("No", "Temperature=Mild") === 2)
    assert(classifier.labelFeatureCount("Yes", "Temperature=Mild") === 4)
    assert(classifier.labelFeatureCount("No", "Humidity=High") === 4)
    assert(classifier.labelFeatureCount("Yes", "Humidity=High") === 3)
    assert(classifier.labelFeatureCount("No", "Humidity=Normal") === 1)
    assert(classifier.labelFeatureCount("Yes", "Humidity=Normal") === 6)
    assert(classifier.labelFeatureCount("No", "Wind=Strong") === 3)
    assert(classifier.labelFeatureCount("Yes", "Wind=Strong") === 3)
    assert(classifier.labelFeatureCount("No", "Wind=Weak") === 2)
    assert(classifier.labelFeatureCount("Yes", "Wind=Weak") === 6)
}
}
