package org.fiasana.test

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import opennlp.scalabha.log.SimpleLogger
import java.io.{StringWriter, BufferedWriter}
import opennlp.scalabha.model.Value._
import opennlp.scalabha.model.Node._
import opennlp.scalabha.model.{Node, Value}
import org.fiasana.{Grammarian, X2TXT}

class GrammarianTest extends FlatSpec with ShouldMatchers {
  "getTreeGrammar" should "work" in {
    val n0 = Value("1")

    assert(Grammarian.getTreeGrammar(n0) ===(
      Map(),
      Map()))

    val n1 = Node("a", List(Value("1")))
    assert(Grammarian.getTreeGrammar(n1) ===(
      Map(),
      Map("a" -> Map("1" -> 1))))

    val n2 = Node("b", List(Node("a", List(Value("1")))))
    assert(Grammarian.getTreeGrammar(n2) ===(
      Map("b" -> Map(List("a") -> 1)),
      Map("a" -> Map("1" -> 1))))

    val n3 = Node("b", List(Node("a", List(Value("1"))), Node("c", List(Value("2")))))
    assert(Grammarian.getTreeGrammar(n3) ===(
      Map("b" -> Map(List("a", "c") -> 1)),
      Map("a" -> Map("1" -> 1), "c" -> Map("2" -> 1))))

    val n4 = Node("b", List(
      Node("a", List(Value("1"))), Node("c", List(Value("2"))),
      Node("a", List(Value("1"))), Node("b", List(
        Node("d", List(Value("4")))
      ))
    )
    )
    assert(Grammarian.getTreeGrammar(n4) ===(
      Map(
        "b" -> Map(
          List("a", "c", "a", "b") -> 1,
          List("d") -> 1
        )
      ),
      Map(
        "a" -> Map(
          "1" -> 2
        ),
        "c" -> Map("2" -> 1),
        "d" -> Map("4" -> 1))
      )
    )
  }

  "combineGrammars" should "work" in {
    val g1: org.fiasana.Grammarian.TerminalGrammar = Map().withDefaultValue(
      Map[String, Int]().withDefaultValue(0)
    )
    val g2: org.fiasana.Grammarian.TerminalGrammar = Map("a" -> Map("1" -> 1))
    val g3: org.fiasana.Grammarian.NonTerminalGrammar = Map(
      "b" -> Map(
        List("a", "c", "a", "b") -> 1,
        List("d") -> 1
      )
    )
    val g4: org.fiasana.Grammarian.NonTerminalGrammar = Map("b" -> Map(List("a") -> 1))

    assert(Grammarian.combineTerminalGrammars(g1, g1) === g1)
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
    ))
  }
}
