package benchmark

import fastparsers.input.InputWindow
import org.scalameter.api._
import org.scalameter._
import fastparsers.framework.parseresult.{ParseResult, Success, Failure}
import fastparsers.parsers.Parser

import InputWindow._
import parsers.KVParsers._

class KeyValueBenchmarkHelper extends BenchmarkHelper {

  val files = List("kvpairs.txt") map { (f: String) =>
    val fileName = "FastParsers/src/test/resources/micro/" + f
    val file = scala.io.Source.fromFile(fileName).getLines mkString "\n"
    val fileArray = file.toCharArray
    //val fileSeq = new FastCharSequence(fileArray)
    fileArray
  }

  val description = "key-value pairs"
}

class KeyValueAll extends KeyValueBenchmarkHelper {
  //include[KeyValueMapEarly]
  //include[KeyValueMapAtPairTime]
  //include[KeyValueMapAtInnerListTime]
  //include[KeyValueMapAtVeryEnd]
  //include[KeyValueRecognize]
  include[KeyValueSchemaKnownRecognize]
  include[KeyValueSchemaKnownRecognizeUnit]
  //include[KeyValueRecognizeGetValue]
  //include[KeyValueRecognizeAndGetValueAtPairTime]
  //include[KeyValueRecognizeTakeWhile2]
  //include[KeyValueRecognizeTakeWhile2Hoisted]
  //include[KeyValueRecognizeTakeWhile3]
  //include[KeyValueRecognizeWSSkip]
  //include[KeyValueRecognizeRecWSSkip]
  //include[KeyValueRecognizeAndGetValueAtEnd]
  //include[KeyValueRecognizeAndGetValueMapAtEnd]
  //include[KeyValueRecognizeAndGetValueSchemaKnown]
  include[KeyValueJSON]
}

/**
 * A pair string parser. Parses stuff of the form
 * [{"..." : "...", "...": "..."}]
 * maps stuff into strings early
 */
class KeyValueMapEarly extends KeyValueBenchmarkHelper {
  performanceOfParsers { f =>
    runBM(f, "mapEarly", KVMapEarly.parser.main)
  }
}

/**
 * A pair string parser. Parses stuff of the form
 * [{"..." : "...", "...": "..."}]
 * maps at pair creation time
 */
class KeyValueMapAtPairTime extends KeyValueBenchmarkHelper {
  performanceOfParsers { f =>
    runBM(f, "mapAtPairTime", KVMapAtPairTime.parser.main)
  }
}

/**
 * A pair string parser. Parses stuff of the form
 * [{"..." : "...", "...": "..."}]
 * maps at end of inner list
 */
class KeyValueMapAtInnerListTime extends KeyValueBenchmarkHelper {
  performanceOfParsers { f =>
    runBM(f, "mapAtInnerListTime", KVMapAtInnerListTime.parser.main)
  }
}

/**
 * A pair string parser. Parses stuff of the form
 * [{"..." : "...", "...": "..."}]
 * maps at the very end
 */
class KeyValueMapAtVeryEnd extends KeyValueBenchmarkHelper {
  performanceOfParsers { f =>
    runBM(f, "mapAtVeryEnd", KVMapAtVeryEnd.parser.main)
  }
}

/**
 * A pair string parser. Parses stuff of the form
 * [{"..." : "...", "...": "..."}]
 * just recognize
 */
class KeyValueRecognize extends KeyValueBenchmarkHelper {
  performanceOfParsers { f =>
    runBM(f, "recognize", KVRecognize.parser.main)
  }
}

/**
 * A pair string parser. Parses stuff of the form
 * [{"name" : "...", "lastname": "..."}]
 * just recognize, arrays hoisted out
 */
class KeyValueSchemaKnownRecognize extends KeyValueBenchmarkHelper {
  performanceOfParsers { f =>
    runBM(f, "schemaKnownRecognize", KVSchemaKnownRecognize.parser.main)
  }
}

class KeyValueSchemaKnownRecognizeUnit extends KeyValueBenchmarkHelper {
  performanceOfParsers { f =>
    runBM(f, "schemaKnownRecognizeUnit", KVSchemaKnownRecognizeUnit.parser.main)
  }
}

/**
 * A pair string parser. Parses stuff of the form
 * [{"name" : "...", "lastname": "..."}]
 * recognizes a pair and throws out the key
 */
class KeyValueRecognizeGetValue extends KeyValueBenchmarkHelper {
  performanceOfParsers { f =>
    runBM(f, "getValue", KVRecognizeGetValue.parser.main)
  }
}

/**
 * A pair string parser. Parses stuff of the form
 * [{"name" : "...", "lastname": "..."}]
 * recognizes a pair and throws out the key, at pair time
 */
class KeyValueRecognizeAndGetValueAtPairTime extends KeyValueBenchmarkHelper {
  performanceOfParsers { f =>
    runBM(f, "getValueAtPairTime", KVRecognizeAndGetValueAtPairTime.parser.main)
  }
}

/**
 * A pair string parser. Parses stuff of the form
 * [{"..." : "...", "...": "..."}]
 * using takeWhile2, which does not inline its function
 * just recognize
 */
class KeyValueRecognizeTakeWhile2 extends KeyValueBenchmarkHelper {
  performanceOfParsers { f =>
    runBM(f, "recognizeTakeWhile2", KVRecognizeTakeWhile2.parser.main)
  }
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
  performanceOfParsers { f =>
    runBM(f, "recognizeTakeWhile2Hoisted", KVRecognizeTakeWhile2Hoisted.parser.main)
  }
}

/**
 * A pair string parser. Parses stuff of the form
 * [{"..." : "...", "...": "..."}]
 * using takeWhile3, which inlines its function
 */
class KeyValueRecognizeTakeWhile3 extends KeyValueBenchmarkHelper {
  performanceOfParsers { f =>
    runBM(f, "recognizeTakeWhile3", KVRecognizeTakeWhile3.parser.main)
  }
}

class KeyValueRecognizeWSSkip extends KeyValueBenchmarkHelper {
  performanceOfParsers { f =>
    runBM(f, "recognizeWSSkip", KVRecognizeWSSkip.parser.main)
  }
}

class KeyValueRecognizeRecWSSkip extends KeyValueBenchmarkHelper {
  performanceOfParsers { f =>
    runBM(f, "recognizeRecWSSkip", KVRecognizeRecWSSKip.parser.main)
  }
}

class KeyValueRecognizeAndGetValueAtEnd extends KeyValueBenchmarkHelper {
  performanceOfParsers { f =>
    runBM(f, "recognizeAndGetValueAtEnd", KVRecognizeAndGetValueAtEnd.parser.main)
  }
}

class KeyValueRecognizeAndGetValueMapAtEnd extends KeyValueBenchmarkHelper {
  performanceOfParsers { f =>
    runBM(f, "recognizeAndGetValueMapAtEnd", KVRecognizeAndGetValueMapAtEnd.parser.main)
  }
}

class KeyValueRecognizeAndGetValueSchemaKnown extends KeyValueBenchmarkHelper {
  performanceOfParsers { f =>
    runBM(f, "recognizeAndGetValuesSchemaKnown", KVRecognizeAndGetValueMapAtEnd.parser.main)
  }
}

/**
 * A pair string parser. Parses stuff of the form
 * [{"..." : "...", "...": "..."}]
 * we do this using the general JSON parser
 */
class KeyValueJSON extends KeyValueBenchmarkHelper {
  import parsers.JsonParsers._
  performanceOfParsers { f =>
    runBM(f, "jsonparser", JSonImplBoxed.jsonparser.value)
  }
}

class WeeksBenchmarkHelper extends BenchmarkHelper {
  val files = List("weeks.txt") map { (f: String) =>
    val fileName = "FastParsers/src/test/resources/micro/" + f
    val file = scala.io.Source.fromFile(fileName).getLines mkString "\n"
    val fileArray = file.toCharArray
    //val fileSeq = new FastCharSequence(fileArray)
    fileArray
  }

  val description = "weeks"
}

class KeyValueSchemaKnownRecognizeWeeks extends WeeksBenchmarkHelper {
  performanceOfParsers { f =>
    runBM(f, "schemaKnownRecognizeWeeks", KVSchemaKnownRecognizeWeeks.parser.main)
  }
}

class KeyValueSchemaKnownRecognizeWeeksRec extends WeeksBenchmarkHelper {
  performanceOfParsers { f =>
    runBM(f, "schemaKnownRecognizeWeeksRec", KVSchemaKnownRecognizeWeeksRec.parser.main)
  }
}

class KeyValueJSONWeeks extends WeeksBenchmarkHelper {
  import parsers.JsonParsers._
  performanceOfParsers { f =>
    runBM(f, "jsonparser", JSonImplBoxed.jsonparser.value)
  }
}
