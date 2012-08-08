package opennlp.scalabha.tag.support

import org.junit.Assert._
import org.junit.Test

import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.util.LogNum

class TagDictFactoryTests {

  @Test
  def test_SimpleWeightedTagDictFactory_passthroughTransformer() {
    
  }
  
  @Test
  def test_ExternalFileTagDictFactory() {

    val fullTagset = Set("A", "B", "C")

    {
      val tdf = new ExternalFileTagDictFactory("data/human-tagdict-en-1hr-kj.txt", fullTagset)
      val td = tdf.make(new Iterable[IndexedSeq[(String, String)]] { def iterator = sys.error("do not call") })
      println(td.setIterator.ungroup.size)
    }

    {
      val tdf = new ExternalFileTagDictFactory("data/human-tagdict-en-1hr-vj.txt", fullTagset)
      val td = tdf.make(new Iterable[IndexedSeq[(String, String)]] { def iterator = sys.error("do not call") })
      println(td.setIterator.ungroup.size)
    }

  }

}
