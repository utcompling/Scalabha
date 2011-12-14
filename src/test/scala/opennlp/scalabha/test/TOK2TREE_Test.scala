package opennlp.scalabha.test

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FlatSpec
import opennlp.scalabha.preproc.TOK2TREE
import opennlp.scalabha.model.{Value, Node}

class TOK2TREE_Test extends FlatSpec with ShouldMatchers {
  "1apply(line)" should "parse lines correctly" in
    assert(TOK2TREE("") === "(TOP )")
  "2apply(line)" should "parse lines correctly" in
    assert(TOK2TREE(" ") === "(TOP )")
  "3apply(line)" should "parse lines correctly" in
    assert(TOK2TREE("<EOS>") === "(TOP )")
  "4apply(line)" should "parse lines correctly" in
    assert(TOK2TREE("This is a tok line .") === "(TOP\n    (S (x This) (x is) (x a) (x tok) (x line) (. .) ) )")
  "5apply(line)" should "parse lines correctly" in
    assert(TOK2TREE("This is a tok line . <EOS>") === "(TOP\n    (S (x This) (x is) (x a) (x tok) (x line) (. .) ) )")
  "6apply(line)" should "parse lines correctly" in
    assert(TOK2TREE("This is a tok line . <EOS> This is another . <EOS>") === "(TOP\n    (S (x This) (x is) (x a) (x tok) (x line) (. .) )\n    (S (x This) (x is) (x another) (. .) ) )")
  "7apply(line)" should "parse lines correctly" in
    assert(TOK2TREE("This is a tok line . <EOS> This is another . ") === "(TOP\n    (S (x This) (x is) (x a) (x tok) (x line) (. .) )\n    (S (x This) (x is) (x another) (. .) ) )")
  "8apply(line)" should "parse lines correctly" in
    assert(TOK2TREE("This is a tok line . <EOS> This is another .") === "(TOP\n    (S (x This) (x is) (x a) (x tok) (x line) (. .) )\n    (S (x This) (x is) (x another) (. .) ) )")

  "0getTree(line)" should "return the correct tree" in assert(TOK2TREE.getTree("") === Node("TOP",Nil))
  "1getTree(line)" should "return the correct tree" in assert(TOK2TREE.getTree(" ") === Node("TOP",Nil))
  "2getTree(line)" should "return the correct tree" in assert(TOK2TREE.getTree("  <EOS>\t\t\n") === Node("TOP",Nil))
  "3getTree(line)" should "return the correct tree" in assert(TOK2TREE.getTree("This is a tok line .") ===
    Node("TOP",List(Node("S",List(Node("x",List(Value("This"))), Node("x",List(Value("is"))), Node("x",List(Value("a"))), Node("x",List(Value("tok"))), Node("x",List(Value("line"))), Node(".",List(Value(".")))))))
  )
}