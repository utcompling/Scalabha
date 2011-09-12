package org.scalabha.lang

import org.clapper.argot._
import org.apache.commons.lang3.StringEscapeUtils.unescapeHtml4
import java.util.regex.Pattern
import util.matching.Regex.Match

object Tokenizer {

  import ArgotConverters._

  val lang_opts = "(eng|fra|kin|mlg)"

  val parser = new ArgotParser("org.scalabha.lang.Tokenizer", preUsage = Some("Version 0.0"))
  val lang = parser.option[String](List("l", "lang"), lang_opts, "source text language")
  val help = parser.flag[Boolean](List("h", "help"), "print help")
  val input = parser.option[String](List("i", "input"), "FILE", "input file to tokenize")


  def apply(string: String): String = {
    //TODO: some of these are language-specific. Need to implement some conditionals
    apply(string, List[(String) => String](
      (s) => unescapeHtml4(s),
      (s) => "([knrwyz])\u001A([aeiou])".r.replaceAllIn(s, (m) => "%s'%s".format(m.group(1), m.group(2))), //FIXME: if kin
      (s) => "[\u0000-\u0007\u000E-\u001F\u007F]".r.replaceAllIn(s, ""),
//      (s) => """\u001a""".r.replaceAllIn(s, ""),
      (s) => "\\xC2\\x80".r.replaceAllIn(s, ""),
      (s) => "([knrwyz]?)\u0220([aeiou]?)".r.replaceAllIn(s, (m) => "%s:)%s".format(m.group(1), m.group(2)))
    ))
  }

  def apply(string: String, transformationPipeline: List[(String) => String]): String = {
    var result = string
    for (pipeStage <- transformationPipeline) {
      val a = result
      val b = result.getBytes

      result = pipeStage(result)
    }
    result
  }

  def main(args: Array[String]) {
    try {
      parser.parse(args)

      if (help.value.isDefined) {
        parser.usage()
      }

      val text_lang =
        if (lang.value.isDefined) {
          val res = lang.value.get
          val lang_opts_r = lang_opts.r
          res match {
            case lang_opts_r(lang) => lang
            case _ =>
              parser.usage("Invalid language selection: %s".format(res))
          }
        } else {
          "eng" //default
        }

      val input_file =
        (if (input.value.isDefined) {
          scala.io.Source.fromFile(input.value.get, "ISO-8859-1")
        } else {
          scala.io.Source.stdin
        }).getLines()

      for (line <- input_file) {
//        println(Tokenizer(line.trim))
        print("%s: %s -> %s\n".format(text_lang, line.trim(),
          Tokenizer(line.trim())))
      }
    }
    catch {
      case e: ArgotUsageException =>
        println(e.message)
    }
  }

}