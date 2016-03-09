package benchmark

import fastparsers.input.InputWindow
import org.scalameter.api._
import org.scalameter._
import fastparsers.framework.parseresult.{ParseResult, Success, Failure}
import fastparsers.parsers.Parser

import InputWindow._
import parsers.KVParsers._

class KeyValueBenchmarkHelper extends BenchmarkHelper {
  val files = List("kvpairs.txt")
}

class KeyValueAll extends KeyValueBenchmarkHelper {
  include[KeyValueMapEarly]
  include[KeyValueMapAtPairTime]
  include[KeyValueMapAtInnerListTime]
  include[KeyValueMapAtVeryEnd]
  include[KeyValueRecognize]
  include[KeyValueSchemaKnownRecognize]
  include[KeyValueRecognizeGetValue]
  include[KeyValueRecognizeAndGetValueAtPairTime]
  include[KeyValueRecognizeTakeWhile2]
  include[KeyValueRecognizeTakeWhile2Hoisted]
  include[KeyValueRecognizeTakeWhile3]
  include[KeyValueRecognizeWSSkip]
  include[KeyValueRecognizeRecWSSkip]
  include[KeyValueRecognizeAndGetValueAtEnd]
  include[KeyValueRecognizeAndGetValueMapAtEnd]
  include[KeyValueRecognizeAndGetValueSchemaKnown]
  include[KeyValueJSON]
}

/**
 * A pair string parser. Parses stuff of the form
 * [{"..." : "...", "...": "..."}]
 * maps stuff into strings early
 */
class KeyValueMapEarly extends KeyValueBenchmarkHelper {
  runBenchmark(
    "key-value pairs",
    files,
    List(("mapEarly", KVMapEarly.parser.main))
  )
}

/**
 * A pair string parser. Parses stuff of the form
 * [{"..." : "...", "...": "..."}]
 * maps at pair creation time
 */
class KeyValueMapAtPairTime extends KeyValueBenchmarkHelper {
  runBenchmark(
    "key-value pairs",
    files,
    List(("mapAtPairTime", KVMapAtPairTime.parser.main))
  )
}

/**
 * A pair string parser. Parses stuff of the form
 * [{"..." : "...", "...": "..."}]
 * maps at end of inner list
 */
class KeyValueMapAtInnerListTime extends KeyValueBenchmarkHelper {
  runBenchmark(
    "key-value pairs",
    files,
    List(("mapAtInnerListTime", KVMapAtInnerListTime.parser.main))
  )
}

/**
 * A pair string parser. Parses stuff of the form
 * [{"..." : "...", "...": "..."}]
 * maps at the very end
 */
class KeyValueMapAtVeryEnd extends KeyValueBenchmarkHelper {
  runBenchmark(
    "key-value pairs",
    files,
    List(("mapAtVeryEnd", KVMapAtVeryEnd.parser.main))
  )
}

/**
 * A pair string parser. Parses stuff of the form
 * [{"..." : "...", "...": "..."}]
 * just recognize
 */
class KeyValueRecognize extends KeyValueBenchmarkHelper {
  runBenchmark(
    "key-value pairs",
    files,
    List(("recognize", KVRecognize.parser.main))
  )
}

/**
 * A pair string parser. Parses stuff of the form
 * [{"name" : "...", "lastname": "..."}]
 * just recognize, arrays hoisted out
 */
class KeyValueSchemaKnownRecognize extends KeyValueBenchmarkHelper {
  runBenchmark(
    "key-value pairs",
    files,
    List(("schemaKnownRecognize", KVSchemaKnownRecognize.parser.main))
  )
}

/**
 * A pair string parser. Parses stuff of the form
 * [{"name" : "...", "lastname": "..."}]
 * recognizes a pair and throws out the key
 */
class KeyValueRecognizeGetValue extends KeyValueBenchmarkHelper {
  runBenchmark(
    "key-value pairs",
    files,
    List(("getValue", KVRecognizeGetValue.parser.main))
  )
}

/**
 * A pair string parser. Parses stuff of the form
 * [{"name" : "...", "lastname": "..."}]
 * recognizes a pair and throws out the key, at pair time
 */
class KeyValueRecognizeAndGetValueAtPairTime extends KeyValueBenchmarkHelper {
  runBenchmark(
    "key-value pairs",
    files,
    List(("getValueAtPairTime", KVRecognizeAndGetValueAtPairTime.parser.main))
  )
}

/**
 * A pair string parser. Parses stuff of the form
 * [{"..." : "...", "...": "..."}]
 * using takeWhile2, which does not inline its function
 * just recognize
 */
class KeyValueRecognizeTakeWhile2 extends KeyValueBenchmarkHelper {
  runBenchmark(
    "key-value pairs",
    files,
    List(("recognizeTakeWhile2", KVRecognizeTakeWhile2.parser.main))
  )
}

/**
 * A pair string parser. Parses stuff of the form
 * [{"..." : "...", "...": "..."}]
 * using takeWhile2, which does not inline its function
 * This time we hoist the closure out. Maybe this triggers
 * inlining on the JVM?
 * just recognize
 */
class KeyValueRecognizeTakeWhile2Hoisted extends KeyValueBenchmarkHelper {
  runBenchmark(
    "key-value pairs",
    files,
    List(("recognizeTakeWhile2Hoisted", KVRecognizeTakeWhile2Hoisted.parser.main))
  )
}

/**
 * A pair string parser. Parses stuff of the form
 * [{"..." : "...", "...": "..."}]
 * using takeWhile3, which inlines its function
 */
class KeyValueRecognizeTakeWhile3 extends KeyValueBenchmarkHelper {
  runBenchmark(
    "key-value pairs",
    files,
    List(("recognizeTakeWhile3", KVRecognizeTakeWhile3.parser.main))
  )
}

class KeyValueRecognizeWSSkip extends KeyValueBenchmarkHelper {
  runBenchmark(
    "key-value pairs",
    files,
    List(("recognizeWSSkip", KVRecognizeWSSkip.parser.main))
  )
}

class KeyValueRecognizeRecWSSkip extends KeyValueBenchmarkHelper {
  runBenchmark(
    "key-value pairs",
    files,
    List(("recognizeRecWSSkip", KVRecognizeRecWSSKip.parser.main))
  )
}

class KeyValueRecognizeAndGetValueAtEnd extends KeyValueBenchmarkHelper {
  runBenchmark(
    "key-value pairs",
    files,
    List(("recognizeAndGetValueAtEnd", KVRecognizeAndGetValueAtEnd.parser.main))
  )
}

class KeyValueRecognizeAndGetValueMapAtEnd extends KeyValueBenchmarkHelper {
  runBenchmark(
    "key-value pairs",
    files,
    List(("recognizeAndGetValueMapAtEnd", KVRecognizeAndGetValueMapAtEnd.parser.main))
  )
}

class KeyValueRecognizeAndGetValueSchemaKnown extends KeyValueBenchmarkHelper {
  runBenchmark(
    "key-value pairs",
    files,
    List(("recognizeAndGetValuesSchemaKnown", KVRecognizeAndGetValueMapAtEnd.parser.main))
  )
}

/**
 * A pair string parser. Parses stuff of the form
 * [{"..." : "...", "...": "..."}]
 * we do this using the general JSON parser
 */
class KeyValueJSON extends KeyValueBenchmarkHelper {
  import parsers.JsonParsers._
  lazy val methods: List[(String, Rule)] = List(
    ("jsonparser", JSonImplBoxed.jsonparser.value)
  )
  runBenchmark("key-value pairs", files, methods)
}
