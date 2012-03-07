package opennlp.scalabha.util

import org.apache.commons.lang3.StringEscapeUtils.unescapeHtml4

/**
 * A very simple tokenizer that pulls most puncuation off the characters.
 * Given a raw string, tokenize it with a simple regular expression, returning 
 * an IndexedSeq[String] with one token per element.
 */
object SimpleTokenizer {
  def apply(text: String): IndexedSeq[String] = text
    .replaceAll("""([\?!()\";\|\[\].,'])""", " $1 ")
    .trim
    .split("\\s+")
    .toIndexedSeq
}

object StringUtils {
  val PERMILLE = "\u2030"
  val DEGREE = "\u00b0"

  def tokenize(lang: String, str: String): String = {
    // apply these to all strings
    var res = str

    res = unescapeHtml4(res)

    res = res.replaceAll("\\s+", " ") // reduce spaces
      .replaceAll("(\\S)([,\"])", "$1 @$2") // split with @ marker
      .replaceAll("([,\"])(\\S)", "$1@ $2")
      .replaceAll("([,\"])([^ @])", "$1@ $2")
      .replaceAll(" @\"@ \" ", " @\" @\" ") // change: "" -> @"@ " -> @" @"
      .replaceAll("(\\S)([?!][!?.]*) ", "$1 $2 ") // question and exclamation mark
      .replaceAll("(\\d)(%|" + PERMILLE + "|" + DEGREE + "[CF])([^0-9a-fA-F])", "$1 $2$3") // percent, permille, degrees C or F
      .replaceAll("(?i)\\b(n" + DEGREE + ")(\\d)", "$1 $2") //n+degree ("number sign")
      .replaceAll("(?i)([a-z][0-9])(:)", "$1 @$2 ") // colon
      .replaceAll("('|\\!|\\?)(\\.+) ", "$1 $2 ")
      .replaceAll("\\(", " -LRB- ") // parens -> penn treebank tokenization
      .replaceAll("\\)", " -RRB- ")
      .replaceAll("(?i)([a-z])('s)\\b", "$1 $2")

    if (lang == "eng") {
      res = res
        .replaceAll("(?i)([a-z]+s)(') ", "$1 $2 ")
    }
    res = res
      .replaceAll("(?i)\\b(are|could|dare|did|do|does|had|has|have|is|must|need|should|was|were|would)(n)'(t)\\b", "$1 $2&nbt;'$3") // protect nt from furhter tokenization
      .replaceAll("(?i)\\b(can)(not)\\b", "$1 $2")
      .replaceAll("(?i)\\b(ca)(n)'(t)\\b", "$1$2 $2&nbt;'$3")
      .replaceAll("\\b([Ww])on't\\b", "$1ill n&nbt;'t")
      .replaceAll("\\bWON'T\\b", "WILL N&nbt;'T")
      .replaceAll("\\bain't\\b", "is n&nbt;'t")
      .replaceAll("\\bAin't\\b", "Is n&nbt;'t")
      .replaceAll("\\bAIN'T\\b", "IS N&nbt;'T")
      .replaceAll("(?i)\\b(I)'(m)\\b", "$1 '$2")
      .replaceAll("(?i)\\b(I|you|he|she|we|they)'(d|ll|ve)\\b", "$1 '$2")
      .replaceAll("(?i)\\b(you|we|they)'(re)\\b", "$1 '$2")

    // apply these to lang-specific strings
    if (lang == "eng") {
      res = res
        .replaceAll("([A-Z]) '(D|LL|M|RE|S|T|VE)\\b", "$1 &apos;$2") //protect THEY'VE
        .replaceAll(" '([A-Z]|[a-z]{3,}|to\\b)", " \\xE2\\x80\\x98 $1") //left single quote
        .replaceAll("([a-z])([.!?])' ", "$1 $2 \\xE2\\x80\\x99 ") //right single quote
        .replaceAll(" ([.!?])' ", " $1 \\xE2\\x80\\x99 ") //right single quote
        .replaceAll("(?i)([a-z]{2,})' ", "$1 \\xE2\\x80\\x99 ") //right single quote
        .replaceAll("(?i) (@,@) ' ", " $1 \\xE2\\x80\\x99 ") //right single quote
        .replaceAll("&apos;", "'")
    }

    res = res.replaceAll("(\\d) @,@ (\\d)", "$1,$2") // repair number,number
      .replaceAll("(\\d) @,@ (\\d)", "$1,$2")
      .replaceAll(" @,", " ,") // remove @ marker from ,
      .replaceAll(",@ ", ", ")
      .replaceAll("(?i)\\b(d'|l')([aehiou]|\\xC3[\\x89\\xA9])", "$1 $2")
      .replaceAll("(?i)\\b(c')(est)\\b", "$1 $2")

    if (lang == "kin") {
      res = res
        .replaceAll("(?i)\\b(ab|ah|ak|ay|b|bw|by|cy|h|iby|icy|iry|iy|iz|k|kubw|kw|m|mur|mw|n|ng|nk|nt|ny|nyir|rw|ry|ubw|ukw|urw|utw|uw|tw|w|y|z)'([aeiou])", "$1' $2")
        .replaceAll("(?i)\\b(n)'([bdgkmtz])", "$1' $2")
        .replaceAll("(?i)\\b([bcr]?y)'([bgmns])", "$1' $2")
        .replaceAll("(?i)\\b([a-z]{2,}[bhkmnrwyz]')((?:aba|aga|aha|aka|ama|i|ubu|ubw|udu|ugu|uko|uku|ukw|umu|umw|uru|urw|utu|utw)[a-z]*)\\b", "$1 $2")
        // and again, for some reason
        .replaceAll("(?i)\\b(ab|ah|ak|ay|b|bw|by|cy|h|iby|icy|iry|iy|iz|k|kubw|kw|m|mur|mw|n|ng|nk|nt|ny|nyir|rw|ry|ubw|ukw|urw|utw|uw|tw|w|y|z)'([aeiou])", "$1' $2")
        .replaceAll("(?i)\\b(n)'([bdgkmtz])", "$1' $2")
        .replaceAll("(?i)\\b([bcr]?y)'([bgmns])", "$1' $2")
        .replaceAll("(?i)\\b([a-z]{2,}[bhkmnrwyz]')((?:aba|aga|aha|aka|ama|i|ubu|ubw|udu|ugu|uko|uku|ukw|umu|umw|uru|urw|utu|utw)[a-z]*)\\b", "$1 $2")

        .replaceAll("(?i)(\\d)(fr|frs|frw)\\b", "$1 $2") // Rwanda Francs (currency)
        .replaceAll("(?i)\\b(frw)(\\d)", "$1 $2")
      //centr'afu?rika
    }

    if (lang == "mlg") {
      res = res
        .replaceAll("(?i)([bdfghjklmnprstvz]+')([aeio]|\\xC3\\[\\xA0\\xA8\\xAC\\xB2\\xB4]|\\xE1\\xBB\\xB3)", "$1 $2")
        .replaceAll("(?i)(n')(ny|ilay)\\b", "$1 $2") // article
        .replaceAll("((?:ak|n|tr)')([A-Z])", "$1 $2")
    }

    res = res
      .replaceAll("(\\d)(cm|g|ha|kg|km|Km|kwh|Kwh|m|mg|mm)\\b", "$1 $2") // gram and other measures
      // no-break space, narrow no-break space
      .replaceAll(" (\\d{1,3})(?:\\xC2\\xA0|\\xE2\\x80\\xAF)(\\d{3,3}) ", " $1.$2 ")
      .replaceAll(" (\\d{1,3})(?:\\xC2\\xA0|\\xE2\\x80\\xAF)(\\d{3,3})(?:\\xC2\\xA0|\\xE2\\x80\\xAF)(\\d{3,3}) ", " $1.$2.$3 ")
      .replaceAll(" (\\d{1,3})(?:\\xC2\\xA0|\\xE2\\x80\\xAF)(\\d{3,3})(?:\\xC2\\xA0|\\xE2\\x80\\xAF)(\\d{3,3})(?:\\xC2\\xA0|\\xE2\\x80\\xAF)(\\d{3,3}) ", " $1.$2.$3.$4 ")
      .replaceAll("(\\xC2\\xA0|\\xE2\\x80\\xAF)+ ", " ")
      .replaceAll(" (\\xC2\\xA0|\\xE2\\x80\\xAF)+", " ")
      .replaceAll("(?i)([a-z])(?:\\xC2\\xA0|\\xE2\\x80\\xAF)+([a-z])", "$1 $2")

      // hyphen, slash, colon
      .replaceAll(" (::[a-z][-_a-z0-9]*[a-z0-9]|end-of-article) ", " &nbt;$1&nbt; ") // no-break-token
      .replaceAll(" (::[a-z][-_a-z0-9]*[a-z0-9]|end-of-article) ", " &nbt;$1&nbt; ") // no-break-token

    var prev = ""
    while (res != prev) {
      prev = res
      res = res
        .replaceAll(" (\\d+|\\d{1,3}(?:\\.\\d{3,3})+)(-)(\\d+|\\d{1,3}(?:\\.\\d{3,3})+) ", " $1 \\@$2\\@ $3 ")
        .replaceAll(" (\\d+|\\d{1,3}(?:,\\d{3,3})+)(-)(\\d+|\\d{1,3}(?:,\\d{3,3})+) ", " $1 \\@$2\\@ $3 ")
        .replaceAll("(?i) ((?:[a-z]|\\xC3[\\xA0-\\xBF]|\\xE1\\xBB\\xB3)+)(-|:|\\/)((?:[-\\/a-z]|\\xC3[\\xA0-\\xBF]|\\xE1\\xBB\\xB3)+'?\\.*) ", " $1 \\@$2\\@ $3 ")
        .replaceAll("(?i) ((?:[a-z]|\\xC3[\\xA0-\\xBF]|\\xE1\\xBB\\xB3)+|-?\\d+(?:[.,]\\d+)*)(-|:|\\/|\\*+|\\++) ", " $1 \\@$2 ")
        .replaceAll("(?i) (-|:|\\/|\\*+|\\++)((?:[a-z]|\\xC3[\\xA0-\\xBF]|\\xE1\\xBB\\xB3)+\\.*) ", " $1\\@ $2 ")
    }
    res = res
      .replaceAll(" (Foto)\\/([A-Z])", " $1 \\@\\/\\@ $2")
      .replaceAll(" ([a-zA-Z]+)-((?:[A-Z]|Dr|Prof|St)\\.)([A-Z][a-z]+) ", " $1 \\@-\\@ $2 $3 ")
    if (lang == "mlg")
      res = res
        .replaceAll(" ($mlg_bible_books)\\.(\\d+\\.)", " $1. $2")
    res = res
      .replaceAll(" ((?:[0-2]?[1-9]|3[01])\\/(?:0?[1-9]|1[0-2])\\/) ((?:19|20)\\d\\d) ", " $1$2 ") // join date 28/2/ 2011

    res = res
      .replaceAll("(?i) (www)\\. ([a-z]{3,}) ", " $1.$2 ")
      .replaceAll(" ((?:$title_abbr_mc)\\.)([A-Z][a-z]+) ", " $1 $2 ")
      .replaceAll(" ([A-Z]\\.)([A-Z](?:[a-z]|\\xC3[\\xA0-\\xBF]|\\xE1\\xBB\\xB3){3,}) ", " $1 $2 ")
      .replaceAll(" ([a-zA-Z][a-z]{3,}|[A-Z]{3,}|\\d+|\\d{1,3}(?:\\.\\d{3,3})+|\\d{1,3}\\.\\d{1,3})\\.([A-Z][a-z]+'?) ", " $1. $2 ")
      .replaceAll("(?i) (Brig|Col|Lt)\\.(Gen|Jen)\\. ", " $1. $2. ")
      .replaceAll("(?i) (Gen|Jen)\\.(Maj)\\. ", " $1. $2. ")
      .replaceAll("(?i) (Prof|Pr)\\.(Dr)\\. ", " $1. $2. ")
      .replaceAll("([ \\/][A-Z]|\\b$title_abbr_mc|\\b$title_abbr_uc)\\. ([A-Z])", "$1.&nbsp;$2")
      .replaceAll("([ \\/][A-Z]|\\b$title_abbr_mc|\\b$title_abbr_uc)\\. ([A-Z])", "$1.&nbsp;$2")
      .replaceAll("([ \\/][A-Z]|\\b$title_abbr_mc|\\b$title_abbr_uc)\\.\\s*$", "$1&nbt;.")
      .replaceAll("^((?:|.*[ \\/][A-Z]))\\.\\s*$", "$1&nbt;.") // N
      .replaceAll("^(\\s*[A-Z])\\.\\s*$", "$1&nbt;.")
      .replaceAll("\\b($general_abbr)\\.\\s+", "$1&nbt;. ")
      .replaceAll("\\b($general_abbr)\\.$", "$1&nbt;.")

    if (!res.matches("[a-z]")) {
      //unless line ~= /[a-z]/
      res = res
        .replaceAll("\\b($general_abbr_uc)\\.\\s+", "$1&nbt;. ")
        .replaceAll("\\b($general_abbr_uc)\\.$", "$1&nbt;.")
    }

    res = res
      .replaceAll("\\b((?:[A-Za-z]\\.){1,3}[A-Za-z])\\.\\s+", "$1&nbt;. ") // e.g. a.m. O.B.E.
      .replaceAll("\\b((?:[A-Za-z]\\.){1,3}[A-Za-z])\\.$", "$1&nbt;.") // e.g. a.m. O.B.E.

    if (lang == "mlg") {
      res = res
        .replaceAll("(\\b(?:$mlg_bible_books))\\. (\\d+[.:])", "$1.&nbsp;$2")
    }

    res = res
      .replaceAll("((?:[a-z]|\\xC3[\\xA0-\\xBF]|\\xE1\\xBB\\xB3){5,5})\\. ", "$1 . ")
      .replaceAll("(?i)([a-z][aeiou][a-z][aeiou])\\. ", "$1 . ")
      .replaceAll("(?i)(\\xC3[\\xA0\\xA8\\xAC\\xB2\\xB4]|\\xE1\\xBB\\xB3)\\. ", "$1 . ") // Malagasy vowels
      .replaceAll("([aeio][bdfghjklmnprstvz][aeio]?[aeioy])\\. ", "$1 . ") // Malagasy
      .replaceAll("(?i) (\\d+|(?:[a-z]|\\xC3[\\xA0-\\xBF]|\\xE1\\xBB\\xB3)+)\\. *((?:(?:\"|\\@\"|\\xC2\\xBB|\\xE2\\x80[\\x99\\x9D]) )?(?:|(?:&nbt;)?::.*))$", " $1 . $2")
      .replaceAll(" ((?:19|20)\\d\\d)\\. ((?:(?:\"|\\@\"|\\xC2\\xAB|\\xC2\\xBB|\\xE2\\x80[\\x98\\x99\\x9C\\x9D]) )?[A-Z])", " $1 . $2")
      .replaceAll("(\\S) (-?\\d+|\\d+[-\\/:]\\d+|\\d{1,3}\\.\\d{1,3}|\\d{1,3}(?:\\.\\d{3,3})+|\\d{1,3},\\d{1,3}|\\d{1,3}(?:,\\d{3,3})+|(?:\\d\\d?-)?\\d\\d?\\/\\d\\d?(?:\\/\\d\\d\\d\\d)?|[a-zA-Z]\\d+)\\. ((?:(?:\"|\\@\"|\\xC2\\xAB|\\xC2\\xBB|\\xE2\\x80[\\x98\\x99\\x9C\\x9D]) )?(?:[A-Z]|::|&nbt;::))", "$1 $2 . $3")
      .replaceAll("(\\S) (\\S+[aeiou]|[a-z]\\S+[a-z]|[A-Z]{2,}|frw|Frw|[A-Z]\\S*[a-z][A-Za-z\\x80-\\xFF])\\. ((?:(?:\"|\\(|\\)|\\@\"|\\xC2\\xAB|\\xC2\\xBB|\\xE2\\x80[\\x98\\x99\\x9C\\x9D]) )?(?:|[A-Z]|::|&nbt;::))", "$1 $2 . $3")

    if (lang == "eng") {
      res = res
        .replaceAll("(\\S) (am|at|by|do|in|is|it|of|on|so|to|us)\\. ((?:(?:\"|\\(|\\)|\\@\"|\\xC2\\xAB|\\xC2\\xBB|\\xE2\\x80[\\x98\\x99\\x9C\\x9D]) )?(?:|[A-Z]|::|&nbt;::))", "$1 $2 . $3")
    }

    res = res
      .replaceAll("(%|\\xC2[\\xB2\\xBC-\\xBD]|\\xC2\\xB0[CF]|\\xC2\\xBA[CF])\\.", "$1 .")
      .replaceAll(" \\.([A-Z])", " . $1")
      .replaceAll("(?i) ([a-z0-9&':]*[0-9&':][a-z0-9&':]*[a-z])\\. ", " $1 . ")
      .replaceAll("(?i) ([-a-z0-9&':+%\\/=]*[a-z&':+%][-a-z0-9&':+%\\/=]*[0-9])\\. ", " $1 . ")
      .replaceAll(" ([-0-9.,\\/]*\\d[.,\\/]\\d[-0-9.,\\/]*)\\. ", " $1 . ")
      .replaceAll("(?i)([a-z0-9]|\\xC3[\\xA0-\\xBF]|\\xE1\\xBB\\xB3)(\\.{2,}) ", "$1 $2 ")
      .replaceAll("(?i) (\\.{2,})([a-z0-9])", " $1 $2")
      .replaceAll(" ([!?]+|\\++|\\#+|')\\. ", " $1 . ")
      .replaceAll("(?i) ([a-z]+-+)\\. ", " $1 . ")
      .replaceAll(" (KIST)\\. ", " $1 . ") // Period not part of abbreviation as for USC.

    if (lang == "mlg") {
      res = res
        .replaceAll("(?i) (Aho|Ary|Ay|Esao|Indro|Ireo|Syria)\\. ", " $1 . ")
        .replaceAll(" (e|ny|omby|roa|teo)\\. ", " $1 . ")
    }
    if (lang == "eng") {
      res = res
        .replaceAll(" (all|asks?|back|barn|book|bow|days?|dead|die|down|dug|dust|ear|else|envy|food|go|gods?|gold|hair|hand|he|heed|help|her|hill|him|king|land|law|lips?|man|me|mind|near|new|noon|oil|old|ox|poor|rest|roof|said|says?|sea|seen|ship|shut|sins?|sky|sons?|that|them|this|tomb|too|up|wait|wall|war|way|wear|you)\\. ", " $1 . ")
        .replaceAll("(?i) (Amen|David|Egypt|God|John|Judah|Lord|Moses|Peter|Ruth|Saul|Sodom)\\. ", " $1 . ")
    }
    // period (join)
    res = res
      .replaceAll("(?i) (www\\.) ((?:[a-z]{2,}\\.)+(?:com|edu|fr|net|org|rw)) ", " $1$2 ")
      //  URL + other stuff
      .replaceAll("(?i)((?:www\\.|http:).\\S*[a-z])([-.'\\/:]) ", "$1 $2 ")
      .replaceAll("(?i)([a-z])([-\\/:])((?:www\\.|http:).\\S*) ", "$1 \\@$2\\@ $3 ")
      .replaceAll("&nbsp;", " ")
      .replaceAll("&nbt;", "")
    res
  }

  def tokenizeEng(str: String): String = {
    str

  }

  def tokenizeFra(str: String): String = {
    str
  }

  def tokenizeKin(str: String): String = {
    str

  }

  def tokenizeMlg(str: String): String = {
    str

  }
}
