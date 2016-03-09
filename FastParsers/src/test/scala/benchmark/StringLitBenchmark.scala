package benchmark

import fastparsers.input.InputWindow
import org.scalameter.api._
import fastparsers.framework.parseresult.{ParseResult, Success, Failure}
import fastparsers.parsers.Parser
import parsers.StringLitParsers._

import InputWindow._

class StringLitBenchmarkHelper extends BenchmarkHelper {
  def files = List(
    "stringlits_small.txt",
    "stringlits_inter.txt",
    "stringlits_vbig.txt"
  )
}

class StringLitAll extends StringLitBenchmarkHelper {
  include[StringLitParse]
  include[StringLitMapAtEnd]
  include[StringLitRecognize]
  include[StringLitRecognizeRec]
  include[StringLitRecognizeWsSkipRec]
  include[LastNameRecognize]
  include[LastNameHoistedRecognize]
  include[JSONStringLit]
}


/** A string parser: parses a stringlit and converts it to string */
class StringLitParse extends StringLitBenchmarkHelper {
  runBenchmark(
    "string literals",
    files,
    List(("strLitParse", StringLitParse.parser.main))
  )
}

/** recognizes a sequence of stringlits, maps them to string at the end */
class StringLitMapAtEnd extends StringLitBenchmarkHelper {
  runBenchmark(
    "string literals",
    files,
    List(("strLitMapAtEnd", StringLitMapAtEnd.parser.main))
  )
}

/** Just recognizes a sequence of string literals, puts them in a list */
class StringLitRecognize extends StringLitBenchmarkHelper {

  override val files = super.files ++ List("justlastname.txt")
  runBenchmark(
    "string literals",
    files,
    List(("strLitRecognize", StringLitRecognize.parser.main))
  )
}

class StringLitRecognizeRec extends StringLitBenchmarkHelper {

  override val files = super.files ++ List("justlastname.txt")
  runBenchmark(
    "string literals",
    files,
    List(("strLitRecognizeRec", StringLitRecognizeRec.parser.main))
  )
}

class StringLitRecognizeWsSkipRec extends StringLitBenchmarkHelper {

  override val files = super.files ++ List("justlastname.txt")
  runBenchmark(
    "string literals",
    files,
    List(("strLitRecognizeRecWsSkipRec", StringLitRecognizeWsSkipRec.parser.main))
  )
}

/**
 * recognizes a sequence of string literals, which are all
 * "lastname", puts them in a list
 */
class LastNameRecognize extends StringLitBenchmarkHelper {

  override val files = List("justlastname.txt")
  runBenchmark(
    "string literals",
    files,
    List(("lastnameRecognize", LastNameRecognize.parser.main))
  )

}

/**
 * recognizes a sequence of string literals, which are all
 * "lastname", puts them in a list. The array is hoisted out
 */
class LastNameHoistedRecognize extends StringLitBenchmarkHelper {
  override val files = List("justlastname.txt")
  runBenchmark(
    "string literals",
    files,
    List(("lastnameHoistedRecognize", LastNameHoistedRecognize.parser.main))
  )
}

/** Run a general JSON parser on a sequence of stringlits */
class JSONStringLit extends StringLitBenchmarkHelper {

  import parsers.JsonParsers._
  lazy val methods: List[(String, Rule)] = List(
    ("StringLit.parse", JSonImplBoxed.jsonparser.value)
  )
  runBenchmark(
    "string literals",
    files,
    List(("jsonStringLiterals", JSonImplBoxed.jsonparser.value))
  )
}
