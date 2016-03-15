package benchmark

import java.io.{File, RandomAccessFile}
import java.nio.channels.FileChannel
import java.nio.charset.StandardCharsets

import org.scalameter.Key
import org.scalameter.picklers.Implicits._
import org.scalameter.api._
import parsers.KVParsers._

trait AuthorInfoReader {
  type Files = List[Array[Char]]
  private val RO = FileChannel.MapMode.READ_ONLY

  final def readFile(filename: String): Array[Char] = {
    val fileName = s"FastParsers/src/test/resources/micro/$filename"
    val fileSize = (new File(fileName)).length

    def readFileUpfront = {
      val channel = new RandomAccessFile(fileName, "r").getChannel
      val buffer = channel.map(RO, 0, channel.size).force()
      val contents = StandardCharsets.ISO_8859_1.decode(buffer).array
      channel.close
      println("File contents have been read")
      contents
    }

    def readFileBySteps = {
      val channel = new RandomAccessFile(fileName, "r").getChannel
      val totalSize = channel.size
      /* Convert to `Int`, as the size of an array cannot be a `Long` */
      val firstHalf = (totalSize / 2).toInt
      val secondHalf = (totalSize - firstHalf).toInt
      val buffer1 = channel.map(RO, 0, firstHalf).force
      println(s"Reading file 0 + $firstHalf")
      val contents1 = StandardCharsets.ISO_8859_1.decode(buffer1).array
      val buffer2 = channel.map(RO, firstHalf, secondHalf).force
      println(s"Reading ${firstHalf + secondHalf} from $firstHalf")
      val contents2 = StandardCharsets.ISO_8859_1.decode(buffer2).array
      channel.close

      val contents = Array.ofDim[Char](totalSize.toInt)
      /* Heads up - don't use {contents1 or contents2}.length because
       * the actual size of the contents is the same than `totalSize`. */
      System.arraycopy(contents1, 0, contents, 0, firstHalf)
      System.arraycopy(contents2, 0, contents, firstHalf, secondHalf)
      println("File contents have been put together into one single array")
      contents
    }

    /* NIO `Charset`s are buggy and cannot be used for sizes of arrays
     * around 2GB because it tries to allocate 2*n + 1 which gives an integer
     * overflow and throws an `IllegalArgumentException`.
     *
     * To deal with larger files we need to create separate arrays and then
     * concatenate them. This is only valid for file sizes than don't overcome
     * `Int.MaxValue`. */
    if((2 * fileSize + 1) > Int.MaxValue) {
      println(s"File size is ~2GB, concretely: $fileSize")
      readFileBySteps
    } else {
      readFileUpfront
    }
  }
}

/*
object KeyValueFiles {
  lazy val fileArrays = List("kvpairs.txt") map { (f: String) =>
    val fileName = "FastParsers/src/test/resources/micro/" + f
    val file = scala.io.Source.fromFile(fileName).getLines mkString "\n"
    val fileArray = file.toCharArray
    fileArray
  }
  implicit val range = Gen.enumeration("size")(List.empty[Array[Char]])
}

class KeyValueBenchmarkHelper extends BenchmarkHelper {
  val description = "key-value pairs"
}

class KeyValueAll extends BenchmarkRun {
  include[KeyValueMapEarly]
  include[KeyValueMapAtPairTime]
  include[KeyValueMapAtInnerListTime]
  include[KeyValueMapAtVeryEnd]
  include[KeyValueRecognize]
  include[KeyValueSchemaKnownRecognize]
  include[KeyValueSchemaKnownRecognizeUnit]
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
  import KeyValueFiles._
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
  import KeyValueFiles._
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
  import KeyValueFiles._
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
  import KeyValueFiles._
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
  import KeyValueFiles._
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
  import KeyValueFiles._
  performanceOfParsers { f =>
    runBM(f, "schemaKnownRecognize", KVSchemaKnownRecognize.parser.main)
  }
}

class KeyValueSchemaKnownRecognizeUnit extends KeyValueBenchmarkHelper {
  import KeyValueFiles._
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
  import KeyValueFiles._
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
  import KeyValueFiles._
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
  import KeyValueFiles._
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
  import KeyValueFiles._
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
  import KeyValueFiles._
  performanceOfParsers { f =>
    runBM(f, "recognizeTakeWhile3", KVRecognizeTakeWhile3.parser.main)
  }
}

class KeyValueRecognizeWSSkip extends KeyValueBenchmarkHelper {
  import KeyValueFiles._
  performanceOfParsers { f =>
    runBM(f, "recognizeWSSkip", KVRecognizeWSSkip.parser.main)
  }
}

class KeyValueRecognizeRecWSSkip extends KeyValueBenchmarkHelper {
  import KeyValueFiles._
  performanceOfParsers { f =>
    runBM(f, "recognizeRecWSSkip", KVRecognizeRecWSSKip.parser.main)
  }
}

class KeyValueRecognizeAndGetValueAtEnd extends KeyValueBenchmarkHelper {
  import KeyValueFiles._
  performanceOfParsers { f =>
    runBM(f, "recognizeAndGetValueAtEnd", KVRecognizeAndGetValueAtEnd.parser.main)
  }
}

class KeyValueRecognizeAndGetValueMapAtEnd extends KeyValueBenchmarkHelper {
  import KeyValueFiles._
  performanceOfParsers { f =>
    runBM(f, "recognizeAndGetValueMapAtEnd", KVRecognizeAndGetValueMapAtEnd.parser.main)
  }
}

class KeyValueRecognizeAndGetValueSchemaKnown extends KeyValueBenchmarkHelper {
  import KeyValueFiles._
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
  import KeyValueFiles._
  import parsers.JsonParsers._
  performanceOfParsers { f =>
    runBM(f, "jsonparser", JSonImplBoxed.jsonparser.value)
  }
}

/************ WEEKS ***********/
object WeeksFiles {
  lazy val fileArrays = List("weeks.txt") map { (f: String) =>
    val fileName = "FastParsers/src/test/resources/micro/" + f
    val file = scala.io.Source.fromFile(fileName).getLines mkString "\n"
    val fileArray = file.toCharArray
    fileArray
  }
  implicit val range = Gen.enumeration("size")(List.empty[Array[Char]])
}

class WeeksBenchmarkHelper extends BenchmarkHelper {
  val description = "weeks"
}

class KeyValueSchemaKnownRecognizeWeeks extends WeeksBenchmarkHelper {
  import WeeksFiles._
  performanceOfParsers { f =>
    runBM(f, "schemaKnownRecognizeWeeks", KVSchemaKnownRecognizeWeeks.parser.main)
  }
}

class KeyValueSchemaKnownRecognizeWeeksRec extends WeeksBenchmarkHelper {
  import WeeksFiles._
  performanceOfParsers { f =>
    runBM(f, "schemaKnownRecognizeWeeksRec", KVSchemaKnownRecognizeWeeksRec.parser.main)
  }
}

class KeyValueJSONWeeks extends WeeksBenchmarkHelper {
  import WeeksFiles._
  import parsers.JsonParsers._
  performanceOfParsers { f =>
    runBM(f, "jsonparser", JSonImplBoxed.jsonparser.value)
  }
}

class KeyValueSchemaKnownRecognizeWeeksADT extends WeeksBenchmarkHelper {
  import WeeksFiles._
  performanceOfParsers { f =>
    runBM(f, "schemaKnownRecognizeWeeksADT", KVSchemaKnownRecognizeWeeksADT.parser.main)
  }
}
*/

/********** AUTHORINFOS ********/


object AuthorInfoFiles extends AuthorInfoReader {
  lazy val fileNames = List("authorinfos-480.txt")
  lazy val fileArrays = fileNames map readFile

  implicit lazy val filesGen: Gen[String] = Gen.single("files")(fileNames.head)
}

trait AuthorInfosBenchmark extends BasicBenchmark {
  lazy val data = AuthorInfoFiles.fileArrays.head
  val description = "authorinfos"
}

class KeyValueAuthorAll extends Bench.Group {
  performance of "recogniser" config (
   Key.reports.resultDir -> "benchmarks"
  ) in {
    include(new KeyValueSchemaKnownRecognizeAuthorInfos {})
  }

  performance of "parser" config (
    Key.reports.resultDir -> "benchmarks"
  ) in {
    include(new KeyValueJSONAuthorInfos {})
  }

  performance of "parser-recogniser-8" config (
    Key.reports.resultDir -> "benchmarks"
    ) in {
    include(new KeyValueJSONAuthorInfosParserRecogniser8 {})
  }

  performance of "parser-recogniser-4" config (
    Key.reports.resultDir -> "benchmarks"
    ) in {
    include(new KeyValueJSONAuthorInfosParserRecogniser4 {})
  }

  performance of "parser-recogniser-12" config (
    Key.reports.resultDir -> "benchmarks"
    ) in {
    include(new KeyValueJSONAuthorInfosParserRecogniser12 {})
  }
}

trait KeyValueSchemaKnownRecognizeAuthorInfos extends AuthorInfosBenchmark {
  import AuthorInfoFiles.filesGen
  performanceOfParsers { (gfiles: Gen[String]) =>
    runBM(gfiles, "recogniserAuthorInfos",
      KVSchemaKnownRecognizeAuthorInfos.parser.main)
  }(filesGen)
}

trait KeyValueJSONAuthorInfos extends AuthorInfosBenchmark {
  import AuthorInfoFiles.filesGen
  import parsers.JsonParsers._
  performanceOfParsers { (gfiles: Gen[String]) =>
    runBM(gfiles, "jsonFullParser", JsonParser.parser.main)
  }(filesGen)
}

trait KeyValueJSONAuthorInfosParserRecogniser4 extends AuthorInfosBenchmark {
  import AuthorInfoFiles.filesGen
  performanceOfParsers { (gfiles: Gen[String]) =>
    runBM(gfiles, "jsonRecogniserParser4",
      KVSchemaKnownRecognizeAuthorPartial4.parser.main)
  }(filesGen)
}

trait KeyValueJSONAuthorInfosParserRecogniser8 extends AuthorInfosBenchmark {
  import AuthorInfoFiles.filesGen
  performanceOfParsers { (gfiles: Gen[String]) =>
    runBM(gfiles, "jsonRecogniserParser8",
      KVSchemaKnownRecognizeAuthorPartial8.parser.main)
  }(filesGen)
}

trait KeyValueJSONAuthorInfosParserRecogniser12 extends AuthorInfosBenchmark {
  import AuthorInfoFiles.filesGen
  performanceOfParsers { (gfiles: Gen[String]) =>
    runBM(gfiles, "jsonRecogniserParser12",
      KVSchemaKnownRecognizeAuthorPartial12.parser.main)
  }(filesGen)
}

/*

/********** AUTHORINFOSPartial ********/
object AuthorPartialFiles {
  lazy val fileArrays = List("authorpartial.txt") map { (f: String) =>
    val fileName = "FastParsers/src/test/resources/micro/" + f
    val file = scala.io.Source.fromFile(fileName).getLines mkString "\n"
    val fileArray = file.toCharArray
    fileArray
  }
  implicit val range = Gen.enumeration("size")(List.empty[Array[Char]])
}

class AuthorPartialBenchmarkHelper extends BenchmarkHelper {
  val description = "authorpartial"
}

class KeyValueAuthorPartialAll extends BenchmarkRun {
  include[KeyValueSchemaKnownRecognizeAuthorPartial]
  include[KeyValueJSONAuthorPartial]
}

class KeyValueSchemaKnownRecognizeAuthorPartial extends AuthorPartialBenchmarkHelper {
  import AuthorPartialFiles._
  performanceOfParsers { f =>
    runBM(f, "schemaKnownRecognizeAuthorPartial", KVSchemaKnownRecognizeAuthorPartial.parser.main)
  }
}

class KeyValueJSONAuthorPartial extends AuthorPartialBenchmarkHelper {
  import AuthorPartialFiles._
  import parsers.JsonParsers._
  performanceOfParsers { f =>
    runBM(f, "jsonparser", JSonImplBoxed.jsonparser.value)
  }
}
  */
