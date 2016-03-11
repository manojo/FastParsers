/*package benchmark

import fastparsers.input.InputWindow
import org.scalameter.api._
import fastparsers.framework.parseresult.{ParseResult, Success, Failure}
import fastparsers.parsers.Parser
import parsers.StringLitParsers._

import InputWindow._

//object StringLitBenchmarkHelper {
//
//}

class StringLitBenchmarkHelper extends BenchmarkHelper {
  val files = List(
    //"stringlits_small.txt",
    //"stringlits_inter.txt",
    "stringlits_vbig.txt"
  ) map { (f: String) =>
    val fileName = "FastParsers/src/test/resources/micro/" + f
    val file = scala.io.Source.fromFile(fileName).getLines mkString "\n"
    val fileArray = file.toCharArray
    //val fileSeq = new FastCharSequence(fileArray)
    fileArray
  }

  val description = "string literals"
}

class StringLitAll extends StringLitBenchmarkHelper {
  include[StringLitParse]
  include[StringLitMapAtEnd]
  //include[StringLitRecognize]
  //include[StringLitRecognizeUnit]
  //include[StringLitRecognizeRec]
  //include[StringLitRecognizeWsSkipRec]
  //include[StringLitRecognizeWsSkipRecUnit]
  //include[JSONStringLit]
}
/*
class LastNameOnly extends StringLitBenchmarkHelper {
  override def files = List("justlastname.txt")

  include[StringLitParse]
  include[StringLitMapAtEnd]
  include[StringLitRecognize]
  include[StringLitRecognizeUnit]
  include[StringLitRecognizeRec]
  include[StringLitRecognizeWsSkipRec]
  include[StringLitRecognizeWsSkipRecUnit]
  include[LastNameRecognize]
  include[LastNameHoistedRecognize]
  include[LastNameHoistedLitRec]
  include[JSONStringLit]
}*/


/** A string parser: parses a stringlit and converts it to string */
class StringLitParse extends StringLitBenchmarkHelper {
  performanceOfParsers { f =>
    runBM(f, "strLitParse", StringLitParse.parser.main)
  }
}

/** recognizes a sequence of stringlits, maps them to string at the end */
class StringLitMapAtEnd extends StringLitBenchmarkHelper {
  performanceOfParsers { f =>
    runBM(f, "strLitMapAtEnd", StringLitMapAtEnd.parser.main)
  }
}

/** Just recognizes a sequence of string literals, puts them in a list */
class StringLitRecognize extends StringLitBenchmarkHelper {
  performanceOfParsers { f =>
    runBM(f, "strLitRecognize", StringLitRecognize.parser.main)
  }
}

class StringLitRecognizeUnit extends StringLitBenchmarkHelper {
  performanceOfParsers { f =>
    runBM(f, "strLitRecognizeUnit", StringLitRecognizeUnit.parser.main)
  }
}

class StringLitRecognizeRec extends StringLitBenchmarkHelper {
  performanceOfParsers { f =>
    runBM(f, "strLitRecognizeRec", StringLitRecognizeRec.parser.main)
  }
}

class StringLitRecognizeWsSkipRec extends StringLitBenchmarkHelper {
  performanceOfParsers { f =>
    runBM(f, "strLitRecognizeRecWsSkipRec", StringLitRecognizeWsSkipRec.parser.main)
  }
}

class StringLitRecognizeWsSkipRecUnit extends StringLitBenchmarkHelper {
  performanceOfParsers { f =>
    runBM(f, "strLitRecognizeRecWsSkipRecUnit", StringLitRecognizeWsSkipRecUnit.parser.main)
  }
}

/**
 * recognizes a sequence of string literals, which are all
 * "lastname", puts them in a list
 */
class LastNameRecognize extends StringLitBenchmarkHelper {
  performanceOfParsers { f =>
    runBM(f, "lastnameRecognize", LastNameRecognize.parser.main)
  }
}

/**
 * recognizes a sequence of string literals, which are all
 * "lastname", puts them in a list. The array is hoisted out
 */
class LastNameHoistedRecognize extends StringLitBenchmarkHelper {
  performanceOfParsers { f =>
    runBM(f, "lastnameHoistedRecognize", LastNameHoistedRecognize.parser.main)
  }
}

class LastNameHoistedLitRec extends StringLitBenchmarkHelper {
  performanceOfParsers { f =>
    runBM(f, "lastnameHoistedLitRec", LastNameHoistedLitRec.parser.main)
  }
}

/** Run a general JSON parser on a sequence of stringlits */
class JSONStringLit extends StringLitBenchmarkHelper {
  import parsers.JsonParsers._
  performanceOfParsers { f =>
    runBM(f, "jsonStringLiterals", JSonImplBoxed.jsonparser.value)
  }
}
*/
