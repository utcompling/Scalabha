package opennlp.scalabha.util

object ListUtils {
  val collapseToOrdinalMap: (List[String])=>Map[String, Int] = 
  (list) => {
    val result = scala.collection.mutable.Map[String, Int]().withDefaultValue(0)
    for (string <- list) {
      result(string) = result(string) + 1
    }
    result.toMap
  }
}