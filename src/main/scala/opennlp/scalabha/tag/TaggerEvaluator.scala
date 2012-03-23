package opennlp.scalabha.tag

import opennlp.scalabha.util.CollectionUtils._
import collection.mutable.{ Map => MMap, Set => MSet, Buffer }

class TaggerEvaluator[Sym, Tag] {

  def evaluate(taggerOutput: Iterable[IndexedSeq[(Sym, Tag)]], goldData: Iterable[IndexedSeq[(Sym, Tag)]], tagDict: Map[Sym, Set[Tag]]): ScoreResults[Sym, Tag] = {
    var correct = 0
    var total = 0
    var knownCorrect = 0
    var knownTotal = 0
    var unkCorrect = 0
    var unkTotal = 0
    var mistakes = List[(Tag, Tag)]()

    assert(taggerOutput.size == goldData.size, "number of sequences in output does not match gold: %d vs %d".format(taggerOutput.size, goldData.size))
    for ((result, gold) <- taggerOutput zipEqual goldData) {
      assert(result.size == gold.size, "sequence length in result does not match gold: %s != %s".format(result, gold))
      for (((rsltSym, rsltTag), (goldSym, goldTag)) <- result zipEqual gold) {
        assert(rsltSym == goldSym, "result sentence and gold sentence are different: %s != %s".format(result.map(_._1), gold.map(_._1)))

        if (rsltTag == goldTag)
          correct += 1
        else
          mistakes ::= ((goldTag, rsltTag))
        total += 1

        if (tagDict.get(goldSym).isDefined /*.map(_(goldTag)).getOrElse(false)*/ ) {
          if (rsltTag == goldTag)
            knownCorrect += 1
          knownTotal += 1
        } else {
          if (rsltTag == goldTag)
            unkCorrect += 1
          unkTotal += 1
        }
      }
    }

    ScoreResults(correct, total, knownCorrect, knownTotal, unkCorrect, unkTotal, mistakes.counts)
  }

}

case class ScoreResults[Sym, Tag](
  correct: Int, total: Int,
  knownCorrect: Int, knownTotal: Int,
  unkCorrect: Int, unkTotal: Int,
  mistakes: Map[(Tag, Tag), Int]) {

  def acc(count: Int, total: Int) = count.toDouble / total
  def accStr(count: Int, total: Int) = "%.2f (%d/%d)".format(acc(count, total) * 100, count, total)

  override def toString = {
    val sb = Buffer[String]()
    sb.append("Total:   " + accStr(correct, total))
    sb.append("Known:   " + accStr(knownCorrect, knownTotal))
    sb.append("Unknown: " + accStr(unkCorrect, unkTotal))
    sb.append("Common Mistakes:")
    sb.append("#Err     Gold      Model")
    for (((goldTag, rsltTag), count) <- mistakes.toList.sortBy(-_._2).take(5))
      sb.append("%-8d %-8s %-8s".format(count, goldTag, rsltTag))
    sb.mkString("\n")
  }

}
