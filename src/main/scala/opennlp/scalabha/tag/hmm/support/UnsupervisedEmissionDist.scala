package opennlp.scalabha.tag.hmm.support

import scala.math
import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.util.LogNum
import opennlp.scalabha.tag.support._
import org.apache.commons.logging.LogFactory
import opennlp.scalabha.tag.TagDict

/**
 * Produce a conditional frequency distribution without labeled training data.
 */
trait UnsupervisedEmissionDistFactory[Tag, Sym] {
  def make(): Tag => Sym => LogNum
}

/**
 * Count occurrences of each word in the raw data and spread them evenly
 * among the tags to which the word is associated in the tag dictionary.
 *
 * So, if a word w appears C(w) times in the raw corpus, and is seen with
 * |TD(w)| tags in the tag dictionary, then assume
 * for each t in TD(w): C(w,t) = C(w) / |TD(w)|.
 *
 * If a word from the raw corpus does not appear in the tag dictionary, then
 *
 *
 * Known words not associated with the tag in the tag dictionary are given
 * zero counts.  Unseen words are assumed to have a 'default' count of 1 for
 * each tag.
 */
class EstimatedRawCountUnsupervisedEmissionDistFactory[Tag, Sym](
  countsTransformer: CountsTransformer[Sym],
  tagDict: TagDict[Sym, Tag],
  rawData: Iterable[Iterable[Sym]])
  extends UnsupervisedEmissionDistFactory[Option[Tag], Option[Sym]] {

  protected val LOG = LogFactory.getLog(classOf[EstimatedRawCountUnsupervisedEmissionDistFactory[Tag, Sym]])

  override def make(): Option[Tag] => Option[Sym] => LogNum = {
    val DefaultedFreqCounts(rawCounts, totalAddition, defaultCount) = countsTransformer(rawData.flatten.counts)

    val rawSymbolCounts = rawCounts.withDefaultValue(defaultCount) // number of times each symbol appears in the raw data
    val tagToSymbolDict = tagDict.setIterator.ungroup.map(_.swap).toSet.groupByKey // a reversed tag dict; Tag -> Set[Symbol]

    val vocabRaw = rawSymbolCounts.keySet // set of all symbols in raw data
    val vocabKnown = tagDict.symbols // set of all symbols in tag dict (known symbols)
    val vocabUnknown = vocabRaw -- vocabKnown // set of all symbols NOT found in tag dict (unknown symbols)

    val rawKnownCountByWord = rawSymbolCounts.filter(x => vocabKnown(x._1)) // counts of each known type from raw data
    val rawUnkwnCountByWord = rawSymbolCounts.filter(x => vocabUnknown(x._1)) // counts of each unknown type from raw data

    LOG.debug("totalRawWordCount = " + rawSymbolCounts.values.sum)
    LOG.debug("totalRawWordCount (known)   = " + rawKnownCountByWord.values.sum)
    LOG.debug("totalRawWordCount (unknown) = " + rawUnkwnCountByWord.values.sum)

    val knownCounts =
      tagToSymbolDict.mapVals {
        _.mapTo(s => (rawSymbolCounts(s)) / tagDict.set(s).size.toDouble).toMap // estimated C(w,t) for known symbols
      }

    val estimatedUnknownProportions = {
      val estUnkProportionsFromRaw = {
        LOG.debug("raw proportions =              " + knownCounts.mapValues(_.values.sum).normalizeValues.mapValues("%.3f".format(_)))

        knownCounts
          .mapVals(_.values.sum) // estimated number of unknown tokens for each tag 
          .normalizeValues // estimated count mass for unknowns spread across all unknown tokens
      }

      val estUnkProportionsFromTagDict = {
        LOG.debug("tagDict proportions =          " + tagToSymbolDict.mapValues(_.size).normalizeValues.mapValues("%.3f".format(_)))

        val x =
          tagToSymbolDict
            .mapVals(_.size) // number of symbols associated with each tag
            .mapVals(math.pow(_, 2)) // exaggerate the differences
            .normalizeValues

        LOG.debug("tagDict (skewed) proportions = " + x.normalizeValues.mapValues("%.3f".format(_)))

        x
      }

      estUnkProportionsFromRaw.keySet.mapTo {
        tag => estUnkProportionsFromTagDict(tag) * estUnkProportionsFromRaw(tag)
      }.normalizeValues.toMap
    }

    LOG.debug("combined =                     " + estimatedUnknownProportions.normalizeValues.mapValues("%.3f".format(_)))

    val counts =
      knownCounts.map {
        case (tag, estimatedKnownCounts) =>
          val estimatedUnknownProportion = estimatedUnknownProportions(tag)
          val unknownCounts = rawUnkwnCountByWord.mapVals(_ * estimatedUnknownProportion)
          val totalCounts = estimatedKnownCounts ++ unknownCounts // combine known and unknown estimates
          (tag, DefaultedFreqCounts(totalCounts, (totalAddition + defaultCount) * estimatedUnknownProportion, defaultCount * estimatedUnknownProportion)) // default for unseen words in test is one count 
      }

    LOG.debug("totalEstWordCount           = " + counts.values.map(_.simpleCounts.values.sum).sum)
    val totalEstKnown = counts.values.map(_.simpleCounts.filter(x => vocabKnown(x._1)).values.sum).sum; LOG.debug("totalEstWordCount (known)   = " + totalEstKnown)
    val totalEstUnkwn = counts.values.map(_.simpleCounts.filter(x => vocabUnknown(x._1)).values.sum).sum; LOG.debug("totalEstWordCount (unknown) = " + totalEstUnkwn)
    LOG.debug("totalEstWordCount (known + unknown) = " + (totalEstKnown + totalEstUnkwn))

    val liftedCounts =
      counts.map {
        case (tag, DefaultedFreqCounts(a, b, c)) =>
          (Option(tag), DefaultedFreqCounts(a.mapKeys(Option(_)), b, c))
      }
    val startEnd: (Option[Tag], DefaultedFreqCounts[Option[Sym], Double]) =
      (None -> DefaultedFreqCounts(Map(None -> 1.)))

    CondFreqDist(DefaultedCondFreqCounts(liftedCounts + startEnd))
  }
}
