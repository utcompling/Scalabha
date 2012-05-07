package opennlp.scalabha.example

import com.codahale.jerkson.Json._

// Parse JSON objects as case classes
case class Person(firstName: String, lastName: String)

object JsonExample {

  def main(args: Array[String]) {
    // Parse JSON arrays
    val listJson = parse[List[Int]]("[1,2,3]") 
    println(listJson)

    // Parse JSON objects
    val simpleJson = parse[Map[String, Int]]("""{"one":1,"two":2}""") 
    println(simpleJson)

    val me = Person("Jason", "Baldridge")
    println("Generating: " + generate(me))

    val personJson = parse[Person]("""{"firstName":"John","lastName":"Smith", "foo":"bar"}""") 
    //val personJson = parse[Person]("""{"firstName":"John", "foo":"bar"}""") 
    println(personJson)
  }
}


