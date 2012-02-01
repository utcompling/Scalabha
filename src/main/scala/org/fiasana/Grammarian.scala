package org.fiasana

import org.clapper.argot._
import opennlp.scalabha.log.SimpleLogger
import java.io.{File, OutputStreamWriter, BufferedWriter}
import opennlp.scalabha.tree.MultiLineTreeParser
import opennlp.scalabha.model.TreeNode

object Grammarian {
  final val ALLOWED_LANGUAGES = List("kin", "mlg", "eng")
  final val IGNORE_DIRS = List("src")

  import ArgotConverters._

  val parser = new ArgotParser(this.getClass().getName, preUsage = Some("Version 0.0"))
  val help = parser.flag[Boolean](List("h", "help"), "print help")
  val srcO = parser.flag(List("sources", "read_sources"), "Set this flag to read the tree/src files. " +
    "Leave it off to read the tree/ files.")
  val muri_dirO = parser.option[File](List("muri_dir"),
    "LDMT_MURI_DIR", "The location of the ldmt-muri repo") {
    (s, opt) =>

      val file = new File(s)
      if (!file.exists)
        parser.usage("Muri directory \"" + s + "\" does not exist.")

      file
  }
  val languageO = parser.option[String]("lang", "LANG", "The language to generate a grammar for.") {
    (s, opt) =>
      if (!ALLOWED_LANGUAGES.contains(s)) {
        parser.usage("Allowed languages are " + ALLOWED_LANGUAGES)
      }
      s
  }
  val collectionsO = parser.multiOption[(String, String)]("coll", "COLLECTION ...", "One or more collections to examine." +
    " If this option is unspecified, all collections in the language will be used.") {
    (s, opt) =>
      try {
        val (lang :: coll :: Nil) = s.split(":").toList
        if (lang == "" || coll == "") {
          throw new MatchError()
        }
        (lang, coll)
      } catch {
        case e: MatchError => parser.usage("You must specify the collection by it's native language and name, as in 'kin:kgmc'. ")
      }
  }

  var log = new SimpleLogger(this.getClass().getName, SimpleLogger.WARN, new BufferedWriter(new OutputStreamWriter(System.err)))

  val getAllFiles: (File, String, String, List[String]) => List[File] =
    (root, lang, extension, searchList) => {
      assert(root.isDirectory)
      var result: List[File] = Nil
      for (child <- root.listFiles().sortBy(f => f.getName)) {
        if (child.isDirectory
          && !IGNORE_DIRS.contains(child.getName)
          && (searchList.contains(child.getName) || searchList.length == 0)) {
          result :::= getAllFiles(child, lang, extension, Nil)
        } else if (child.isFile && child.getName.endsWith(extension) && child.getName.contains(lang)) {
          result = child :: result
        }
      }
      result
    }

  type NonTerminalGrammar = Map[String, Map[List[String], Int]]
  type TerminalGrammar = Map[String, Map[String, Int]]
  type Grammar = Map[String, Map[Any, Int]]

  /**
   * @return (X,Y): X is the non-terminal census and Y is the terminal census
   * <p>The non-terminal census looks like this:<br\>
   *   {"TAG" -> {["TAG1","TAG2"] -> 1} }<\br>
   *     where ["TAG1","TAG2"] is an observation of TAG's child nodes and 1 is the count
   *     <\p>
   *
   * <p>The terminal census looks like this:<br\>
   *   {"TAG" -> {"token" -> 1} }<\br>
   *     where "token" is an observation of TAG's child token and 1 is the count
   *     <\p>
   */
  val getTreeGrammar: (TreeNode) => (NonTerminalGrammar, TerminalGrammar) =
    (tree: TreeNode) => {
      // looks like type sugar doesn't extend to application
      var nonTerminals = Map[String, Map[List[String], Int]]().withDefaultValue(
        Map[List[String], Int]().withDefaultValue(0)
      )
      var terminals = Map[String, Map[String, Int]]().withDefaultValue(
        Map[String, Int]().withDefaultValue(0)
      )

      lazy val getTreeGrammarHelper: (TreeNode) => Unit =
        (tree) => {
          if (!tree.isToken) {
            if (tree.isTerminal) {
              assert(tree.getChildren.length == 1)
              val child = tree.getChildren(0).name
              terminals += ((tree.name, terminals(tree.name) + ((child, terminals(tree.name)(child) + 1))))
            } else {
              val childList = tree.getChildren.map(child => child.name)
              nonTerminals += (
                (tree.name, nonTerminals(tree.name) + ((childList, nonTerminals(tree.name)(childList) + 1)))
                )
            }
            for (child <- tree.getChildren) {
              getTreeGrammarHelper(child)
            }
          }
          ()
        }

      getTreeGrammarHelper(tree)
      (nonTerminals, terminals)
    }

  val combineNonTerminalGrammars: (NonTerminalGrammar, NonTerminalGrammar) => NonTerminalGrammar =
    (gram1, gram2) => {
      var result: NonTerminalGrammar = gram1
      for ((tag, observation) <- gram2) {
        for ((key, count) <- observation) {
          val result_tag = (try {
            result(tag)
          } catch {
            // this makes sure that we allow the caller to define their own defaults
            case e: java.util.NoSuchElementException => Map[List[String], Int]().withDefaultValue(0)
          })
          val result_tag_key = (try {
            result_tag(key)
          } catch {
            // this makes sure that we allow the caller to define their own defaults
            case e: java.util.NoSuchElementException => 0
          })
          result += ((tag, result_tag + ((key, result_tag_key + count))))
        }
      }
      result
    }
  val combineTerminalGrammars: (TerminalGrammar, TerminalGrammar) => TerminalGrammar =
    (gram1, gram2) => {
      var result: TerminalGrammar = gram1
      for ((tag, observation) <- gram2) {
        for ((key, count) <- observation) {
          val result_tag = (try {
            result(tag)
          } catch {
            // this makes sure that we allow the caller to define their own defaults
            case e: java.util.NoSuchElementException => Map[String, Int]().withDefaultValue(0)
          })
          val result_tag_key = (try {
            result_tag(key)
          } catch {
            // this makes sure that we allow the caller to define their own defaults
            case e: java.util.NoSuchElementException => 0
          })
          result += ((tag, result_tag + ((key, result_tag_key + count))))
        }
      }
      result
    }

  val buildGrammar: (List[File] => (NonTerminalGrammar, TerminalGrammar)) =
    (files) => {
      var nonTerminals: NonTerminalGrammar = Map[String, Map[List[String], Int]]().withDefaultValue(
        Map[List[String], Int]().withDefaultValue(0)
      )
      var terminals: TerminalGrammar = Map[String, Map[String, Int]]().withDefaultValue(
        Map[String, Int]().withDefaultValue(0)
      )
      for (file <- files) {
        val trees = MultiLineTreeParser(file.getAbsolutePath)
        for (tree <- trees) {
          println(file.getName + " " + tree.getCanonicalString)
          val (tempNonTerm, tempTerm) = getTreeGrammar(tree)
          nonTerminals = combineNonTerminalGrammars(nonTerminals, tempNonTerm)
          terminals = combineTerminalGrammars(terminals, tempTerm)
        }
      }
      (nonTerminals, terminals)
    }

  val getListString: (List[String]) => String =
    (strings) => "[%s]".format(strings.mkString(","))
  val getNonTerminalGrammarPrettyString: (NonTerminalGrammar) => String =
    (grammar) => {
      var resultString = ""
      for ((tag, observations) <- grammar.toList.sortBy(_._1)) {
        resultString += "%5s = { ".format(tag) + observations.map {
          case (key, count) => "" + getListString(key) + ": " + count
        }.mkString("\n        , ") + "\n        }\n"
      }
      resultString
    }
  val getTerminalGrammarPrettyString: (TerminalGrammar) => String =
    (grammar) => {
      var resultString = ""
      for ((tag, observations) <- grammar.toList.sortBy(_._1)) {
        resultString += "%5s = { ".format(tag) + observations.map {
          case (key, count) => "" + key + ": " + count
        }.mkString("\n        , ") + "\n        }\n"
      }
      resultString
    }

  def main(args: Array[String]) {
    try {
      MultiLineTreeParser.log.logLevel = SimpleLogger.NONE
      parser.parse(args)
      if (help.value.isDefined) {
        parser.usage()
      }
      val language: String = languageO.value match {
        case Some(lang) => lang
        case None => parser.usage("You must specify a language from: " + ALLOWED_LANGUAGES)
      }
      val muri_dir: String = muri_dirO.value match {
        case Some(file: File) => file.getAbsolutePath
        case None => parser.usage("You must specify the location of the muri repo.")
      }

      val langsAndCollections = collectionsO.value.toList.groupBy {
        case (k, v) => k
      }.map {
        case (k, vs) => (k, vs.map {
          case (k, v) => v
        })
      }

      for ((collectionLang, collections) <- langsAndCollections) {
        val root = new File(
          (List(muri_dir, "data", "phase2", collectionLang, "tree") :::
            (if (srcO.value.isDefined) List("src") else List())).mkString(File.separator)
        )

        val treeFileList = getAllFiles(root, language, ".tree", collections).reverse

        val treeList: List[List[TreeNode]] = treeFileList.map(treeFile => MultiLineTreeParser(treeFile.getAbsolutePath))

        val (nonTerminalGrammar, terminalGrammar) = buildGrammar(treeFileList)
        println("Terminals:")
        println(getNonTerminalGrammarPrettyString(nonTerminalGrammar))
        println("Non Terminals:")
        println(getTerminalGrammarPrettyString(terminalGrammar))
      }

    } catch {
      case e: ArgotUsageException =>
        println(e.message)
    }
  }
}