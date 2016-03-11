package benchmark

import fastparsers.input.InputWindow
import org.scalameter.api._
import org.scalameter._
import fastparsers.framework.parseresult.{ParseResult, Success, Failure}
import fastparsers.parsers.Parser

import InputWindow._
import parsers.ContributionsParsers._

class ContributionsBenchmarkHelper extends BenchmarkHelper {

  val files = List("contributions.json") map { (f: String) =>
    val fileName = "FastParsers/src/test/resources/contribs/" + f
    val file = scala.io.Source.fromFile(fileName).getLines mkString "\n"
    val fileArray = file.toCharArray
    //val fileSeq = new FastCharSequence(fileArray)
    fileArray
  }

  val description = "contributions"
}

class ContributionsAll extends ContributionsBenchmarkHelper {
  include[Contributions]
  include[TotalAuthorOnly]
  include[ContributionsJSONNoQuery]
  include[TotalAuthorOnlySkipWeeks]
  include[TotalAuthorOnlySkipWeeks2]
}

class Contributions extends ContributionsBenchmarkHelper {
  performanceOfParsers { f =>
    runBM(f, "contributions", Contributions.parser.main)
  }
}

class TotalAuthorOnly extends ContributionsBenchmarkHelper {
  performanceOfParsers { f =>
    runBM(f, "totalAuthorOnly", TotalAuthorOnly.parser.main)
  }
}

class TotalAuthorOnlySkipWeeks extends ContributionsBenchmarkHelper {
  performanceOfParsers { f =>
    runBM(f, "totalAuthorOnlySkipWeeks", TotalAuthorOnlySkipWeeks.parser.main)
  }
}

class TotalAuthorOnlySkipWeeks2 extends ContributionsBenchmarkHelper {
  performanceOfParsers { f =>
    runBM(f, "totalAuthorOnlySkipWeeks2", TotalAuthorOnlySkipWeeks2.parser.main)
  }
}

class ContributionsJSONNoQuery extends ContributionsBenchmarkHelper {
  import parsers.JsonParsers._
  performanceOfParsers { f =>
    runBM(f, "jsonparser", JSonImplBoxed.jsonparser.main)
  }
}
