package opennlp.scalabha.example

object ProcessXml {

  val validChunkTypes = Set("VP","NP")

  def main(args: Array[String]) {
    
    val chunkType = args(0)
    val textElementName = args(1)
    val doChunks = validChunkTypes(chunkType)
    val inputFile = args(2)
    val music = scala.xml.XML.loadFile(inputFile)
    val descriptions = (music \\ textElementName).map(_.text)
    descriptions
      .flatMap { description => 
	if (doChunks) EnglishPipeline.getChunks(description, chunkType)
	else EnglishPipeline.getPersons(description)
      }
      .foreach(println)
  }
  
}
