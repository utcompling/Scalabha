package org.fiasana

import org.clapper.argot._
import opennlp.scalabha.log.SimpleLogger
import java.io.{File, OutputStreamWriter, BufferedWriter}
import opennlp.scalabha.tree.MultiLineTreeParser
import opennlp.scalabha.model.TreeNode

object Grammarian {
  final val ALLOWED_LANGUAGES = List("kin", "mlg")
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
  val collectionsO = parser.multiOption[String]("coll", "COLLECTION ...", "One or more collections to examine." +
    " If this option is unspecified, all collections in the language will be used.")
  var log = new SimpleLogger(this.getClass().getName, SimpleLogger.WARN, new BufferedWriter(new OutputStreamWriter(System.err)))

  val getAllFiles: (File, String, List[String]) => List[File] =
    (root, extension, searchList) => {
      assert(root.isDirectory)
      var result: List[File] = Nil
      for (child <- root.listFiles().sortBy(f => f.getName)) {
        val a = child.isDirectory
        val b = !IGNORE_DIRS.contains(child.getName)
        val c = (searchList.contains(child.getName))
        val d = (searchList.length == 0)
        val e = searchList
        val f = child.getName
        if (child.isDirectory
          && !IGNORE_DIRS.contains(child.getName)
          && (searchList.contains(child.getName) || searchList.length == 0)) {
          result :::= getAllFiles(child, extension, Nil)
        } else if (child.isFile && child.getName.endsWith(extension)) {
          result = child :: result
        }
      }
      result
    }

  val getTreeGrammar: (TreeNode) => Map[String, Map[List[String], Int]] =
    (tree) => {
      var nonTerminals = Map[String, Map[List[String], Int]]().withDefaultValue(
        Map[List[String], Int]().withDefaultValue(0)
      )
      val childList = tree.getChildren.map(child => child.name)
      nonTerminals += (
        (tree.name, nonTerminals(tree.name) + ((childList, nonTerminals(tree.name)(childList) + 1)))
        )
      nonTerminals
    }

  val buildGrammar: (List[File] => Map[TreeNode, List[(File, Int)]]) =
    (files) => {
      val result = scala.collection.mutable.Map
      for (file <- files) {
        val trees = MultiLineTreeParser(file.getAbsolutePath)
        for (tree <- trees) {
          println(file.getName + " " + tree.getCanonicalString)
        }
      }
      Map[TreeNode, List[(File, Int)]]()
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
      val collections = collectionsO.value.toList

      val root = new File(
        (List(muri_dir, "data", "phase2", language, "tree") :::
          (if (srcO.value.isDefined) List("src") else List())).mkString(File.separator)
      )

      val treeFileList = getAllFiles(root, ".tree", collections).reverse

      val treeList: List[List[TreeNode]] = treeFileList.map(treeFile => MultiLineTreeParser(treeFile.getAbsolutePath))

      val grammar = buildGrammar(treeFileList)
    } catch {
      case e: ArgotUsageException =>
        println(e.message)
    }
  }
}