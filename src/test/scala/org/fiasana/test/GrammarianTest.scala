package org.fiasana.test

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import opennlp.scalabha.log.SimpleLogger
import java.io.{StringWriter, BufferedWriter}
import opennlp.scalabha.model.Value._
import opennlp.scalabha.model.Node._
import opennlp.scalabha.model.{Node, Value}
import org.fiasana._

class GrammarianTest extends FlatSpec with ShouldMatchers {
  "getTreeGrammar" should "work" in {
    val source = "test"

    val n0 = Value("1")
    val (g01, g02) = Grammarian.getTreeGrammar(n0,source)
    assert(g01.getGrammar === Map())
    assert(g02.getGrammar === Map())

    val n1 = Node("a", List(Value("1")))
    val (g11, g12) = Grammarian.getTreeGrammar(n1,source)
    assert(g11.getGrammar === Map())
    assert(g12.getGrammar === Map("a" -> Map("1" ->(1, List("test")))))

    val n2 = Node("b", List(Node("a", List(Value("1")))))
    val (g21, g22) = Grammarian.getTreeGrammar(n2,source)
    assert(g21.getGrammar === Map("b" -> Map(List("a") ->(1, List("test")))))
    assert(g22.getGrammar === Map("a" -> Map("1" ->(1, List("test")))))

    val n3 = Node("b", List(Node("a", List(Value("1"))), Node("c", List(Value("2")))))
    val (g31, g32) = Grammarian.getTreeGrammar(n3,source)
    assert(g31.getGrammar === Map("b" -> Map(List("a", "c") -> (1, List("test")))))
    assert(g32.getGrammar === Map("a" -> Map("1" -> (1,List("test"))), "c" -> Map("2" -> (1, List("test")))))

    val n4 = Node("b", List(
      Node("a", List(Value("1"))), Node("c", List(Value("2"))),
      Node("a", List(Value("1"))), Node("b", List(
        Node("d", List(Value("4")))
      ))
    )
    )
    val (g41, g42) = Grammarian.getTreeGrammar(n4,source)
    assert(g41.getGrammar ===
      Map(
        "b" -> Map(
          List("a", "c", "a", "b") -> (1, List("test")),
          List("d") -> (1, List("test"))
        )
      )
    )
    assert(g42.getGrammar ===
      Map(
        "a" -> Map(
          "1" -> (2, List("test", "test"))
        ),
        "c" -> Map("2" -> (1, List("test"))),
        "d" -> Map("4" -> (1, List("test")))
      )
    )

  }

  "combineGrammars" should "work" in {
    val g1 = TerminalGrammar()
    val g2 = TerminalGrammar()
    g2.addObservation("a", "1", 1, "unit")
    val g3 = NonTerminalGrammar()
    g3.addObservation("b", List("a", "c", "a", "b"), 1, "unit")
    g3.addObservation("b", List("d"), 1, "unit")

    val g4 = NonTerminalGrammar()
    g4.addObservation("b", List("a"), 1, "unit")

    /*assert(Grammarian.combineTerminalGrammars(g1, g1) === g1)
    assert(Grammarian.combineTerminalGrammars(g2, g1) === g2)
    assert(Grammarian.combineTerminalGrammars(g1, g2) === g2)
    assert(Grammarian.combineTerminalGrammars(g2, g2) === Map("a" -> Map("1" -> 2)))
    assert(Grammarian.combineNonTerminalGrammars(g3, g4) === Map(
      "b" -> Map(
        List("a", "c", "a", "b") -> 1,
        List("a") -> 1,
        List("d") -> 1
      )
    ))
    assert(Grammarian.combineNonTerminalGrammars(g4, g3) === Map(
      "b" -> Map(
        List("a", "c", "a", "b") -> 1,
        List("a") -> 1,
        List("d") -> 1
      )
    ))*/
  }
}
