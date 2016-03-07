import fastparsers.input.InputWindow
import org.scalameter.api._
import CSVParsers._

import InputWindow._

/**
 * Testing simple properties of parsers vs. recognizers
 */
object MicroBenchmark extends PerformanceTest {

  lazy val executor = LocalExecutor(
    new Executor.Warmer.Default,
    Aggregator.min,
    new Measurer.Default)
  lazy val reporter = new LoggingReporter
  lazy val persistor = Persistor.None

  val range = Gen.enumeration("size")(10)

  //benchBools
  //benchStringLits("stringlits_small.txt")
  //benchStringLits("stringlits_inter.txt")
  //benchStringLits("stringlits_vbig.txt")
  benchKeyValue("kvpairs.txt")

  def benchBools = {
    val bigBoolFileName = "FastParsers/src/test/resources/micro/" + "csvBooleans.txt"
    val bigBoolFile = scala.io.Source.fromFile(bigBoolFileName).getLines mkString "\n"
    val bigBoolFileArray = bigBoolFile.toCharArray
    val bigBoolFileSeq = new FastCharSequence(bigBoolFileArray)

    performance of "CSVBooleanParser:Boolean" in {
      measure method "FastParsers.parse" in {
        using(range) in { j =>
          for (i <- 1 to j)
            CSVParseAll.csvParser.bools(bigBoolFileArray)
        }
      }

      measure method "FastParsers.recognize" in {
        using(range) in { j =>
          for (i <- 1 to j)
            CSVRecognizers.csvParser.bools(bigBoolFileArray)
        }
      }

    }
  }

  def benchDoubles = {
    val bigDoubleFileName = "FastParsers/src/test/resources/micro" + "csvDoubles.txt"
    val bigDoubleFile = scala.io.Source.fromFile(bigDoubleFileName).getLines mkString "\n"
    val bigDoubleFileArray = bigDoubleFile.toCharArray
    val bigDoubleFileSeq = new FastCharSequence(bigDoubleFileArray)

    performance of "CSV Double Parser" in {
      measure method "FastParsers" in {
        using(range) in { j =>
          for (i <- 1 to j)
            CSVParseAll.csvParser.doubles(bigDoubleFileArray)
        }
      }
    }
  }

  /**
   * Benchmarking string literal parsers.
   * A recognizer will merely yield the "struct" corresponding
   * to the string where as the parser will grab the string
   */

  def benchStringLits(fileName: String) = {
    val bigStringLitFileName = "FastParsers/src/test/resources/micro/" + fileName
    val bigStringLitFile = scala.io.Source.fromFile(bigStringLitFileName).getLines mkString "\n"
    val bigStringLitFileArray = bigStringLitFile.toCharArray
    val bigStringLitFileSeq = new FastCharSequence(bigStringLitFileArray)

    performance of s"String Literals on $fileName" in {

      measure method "FastParsers.parse" in {
        using(range) in { j =>
          for (i <- 1 to j)
            CSVParseAll.csvParser.strings(bigStringLitFileArray)
        }
      }

      measure method "FastParsers.recognizeAndMap" in {
        using(range) in { j =>
          for (i <- 1 to j)
            CSVRecognizers.csvParser.stringsParsed(bigStringLitFileArray)
        }
      }

      measure method "FastParsers.recognize" in {
        using(range) in { j =>
          for (i <- 1 to j)
            CSVRecognizers.csvParser.strings(bigStringLitFileArray)
        }
      }
    }
  }

  /**
   * Key value pairs. These are only stringlit to stringlit
   * key-value pairs
   */
  def benchKeyValue(fName: String) = {
    val fileName = "FastParsers/src/test/resources/micro/" + fName
    val file = scala.io.Source.fromFile(fileName).getLines mkString "\n"
    val fileArray = file.toCharArray
    val fileSeq = new FastCharSequence(fileArray)

    performance of s"stringlit key-value on $fileName" in {

      measure method "FastParsers.mapEarlytoString" in {
        using(range) in { j =>
          for (i <- 1 to j)
            CSVParseAll.csvParser.strAll1(fileArray)
        }
      }

      measure method "FastParsers.mapAtPairTime" in {
        using(range) in { j =>
          for (i <- 1 to j)
            CSVParseAll.csvParser.strAll2(fileArray)
        }
      }

      measure method "FastParsers.mapAtObjParseTime" in {
        using(range) in { j =>
          for (i <- 1 to j)
            CSVParseAll.csvParser.strAll3(fileArray)
        }
      }

      measure method "FastParsers.projectAtEnd" in {
        using(range) in { j =>
          for (i <- 1 to j)
            CSVParseAll.csvParser.strAll4(fileArray)
        }
      }

      measure method "FastParsers.recognize" in {
        using(range) in { j =>
          for (i <- 1 to j)
            CSVRecognizers.csvParser.strAll1(fileArray)
        }
      }

      measure method "FastParsers.ignoreRightMap" in {
        using(range) in { j =>
          for (i <- 1 to j)
            CSVRecognizers.csvParser.strAll2(fileArray)
        }
      }

      measure method "FastParsers.ignoreRightMapAtEnd" in {
        using(range) in { j =>
          for (i <- 1 to j)
            CSVRecognizers.csvParser.strAll3(fileArray)
        }
      }
    }
  }
}
