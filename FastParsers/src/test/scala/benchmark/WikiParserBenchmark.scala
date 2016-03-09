package benchmark

import fastparsers.input.InputWindow
import org.scalameter.api._
import scala.collection.mutable.ListBuffer

import InputWindow._
import util.FastCharSequence
import parsers.WikiParsers._

object WikiParserBenchmark extends PerformanceTest {

  lazy val executor = LocalExecutor(
    new Executor.Warmer.Default,
    Aggregator.min,
    new Measurer.Default)
  lazy val reporter = new LoggingReporter
  lazy val persistor = Persistor.None

  val range = Gen.enumeration("size")(10)

  val smallFileName = "FastParsers/src/test/resources/wiki/smaller.xml"
  val smallFile = scala.io.Source.fromFile(smallFileName).getLines mkString "\n"
  val smallFileArray = smallFile.toCharArray
  val smallFileSeq = new FastCharSequence(smallFileArray)

  val bigFileName = "FastParsers/src/test/resources/wiki/larger.xml"
  val bigFile = scala.io.Source.fromFile(bigFileName).getLines mkString "\n"
  val bigFileArray = bigFile.toCharArray
  val bigFileSeq = new FastCharSequence(bigFileArray)

  val vbigFileName = "FastParsers/src/test/resources/wiki/vbig.xml"
  val vbigFile = scala.io.Source.fromFile(vbigFileName).getLines mkString "\n"
  val vbigFileArray = vbigFile.toCharArray
  val vbigFileSeq = new FastCharSequence(vbigFileArray)

  import fastparsers.framework.parseresult.{ParseResult, Success, Failure}

  val bla = """<page>
        <title>AfghanistanGeography</title>
        <ns>0</ns>
        <id>14</id>
        <redirect title="Geography of Afghanistan" />
        <revision>
          <id>407008307</id>
          <parentid>74466619</parentid>
          <timestamp>2011-01-10T03:56:19Z</timestamp>
          <contributor>
            <username>Graham87</username>
            <id>194203</id>
          </contributor>
          <minor />
          <comment>1 revision from [[:nost:AfghanistanGeography]]: import old edit,
          see [[User:Graham87/Import]]</comment>
          <model>wikitext</model>
          <format>text/x-wiki</format>
          <text xml:space="preserve">#REDIRECT [[Geography of Afghanistan]] {{R from CamelCase}}</text>
          <sha1>0uwuuhiam59ufbu0uzt9lookwtx9f4r</sha1>
        </revision>
      </page>"""

//  fullWikiParser.relevantInfos(vbigFileArray) match {
//    case Success(result) => println("success kid "  + result.size)
//    case f @ Failure(msg) => println("error : " + f)
//  }

//  val xmlParsed = WikiParsers.parseXMLtoWikipediaPage(bla)
//  println(xmlParsed)

//  relevantWikiParser.relevantInfos2(vbigFileArray) match {
//    case Success(result) => println("success kid")
//    case f @ Failure(msg) => println("error : " + f)
//  }
/*
 performance of "WikiParser on a small-ish input" in {
    measure method "Fullwiki" in {
      using(range) in { j =>
        for (i <- 1 to j)
          fullWikiParser.fullParser(smallFileArray)
      }
    }

    measure method "Fullwiki + projection" in {
      using(range) in { j =>
        for (i <- 1 to j)
          fullWikiParser.relevantInfos(smallFileArray)
      }
    }

    measure method "Relevantinfo" in {
      using(range) in { j =>
        for (i <- 1 to j)
          relevantWikiParser.relevantInfos2(smallFileArray)
      }
    }
  }
*/
/*  performance of "WikiParser on a larger input" in {
    measure method "Fullwiki" in {
      using(range) in { j =>
        for (i <- 1 to j)
          fullWikiParser.fullParser(smallFileArray)
      }
    }

    measure method "Fullwiki + projection" in {
      using(range) in { j =>
        for (i <- 1 to j)
          fullWikiParser.relevantInfos(smallFileArray)
      }
    }

    measure method "Relevantinfo" in {
      using(range) in { j =>
        for (i <- 1 to j)
          relevantWikiParser.relevantInfos2(smallFileArray)
      }
    }
  }
*/
/*  performance of "WikiParser on superlarge input" in {
    measure method "Fullwiki" in {
      using(range) in { j =>
        for (i <- 1 to j)
          fullWikiParser.fullParser(vbigFileArray)
      }
    }

    measure method "Fullwiki + projection" in {
      using(range) in { j =>
        for (i <- 1 to j)
          fullWikiParser.relevantInfos(vbigFileArray)
      }
    }

    measure method "Relevantinfo" in {
      using(range) in { j =>
        for (i <- 1 to j)
          relevantWikiParser.relevantInfos2(vbigFileArray)
      }
    }
  }
*/

  performance of "Different JSonParser implementations" in {
    /*measure method "FastParsers" in {
      using(range) in { j =>
        for (i <- 1 to j)
          JSonImpl2.jsonparser.value(bigFileArray)
      }
    }*/

    /*measure method "FastParsers Boxed" in {
      using(range) in { j =>
        for (i <- 1 to j)
          JSonImplBoxed.jsonparser.value(vbigFileArray)
      }
    }

    measure method "FastParsers no inline" in {
      using(range) in { j =>
        for (i <- 1 to j)
          JSonImpl3.jsonparser.value(vbigFileArray)
      }
    }
    measure method "FastParsers no inline with errors reporting" in {
      using(range) in { j =>
        for (i <- 1 to j)
          JSonImpl4.jsonparser.value(vbigFileArray)
      }
    }

    measure method "FastParsers no inline with errors reporting and ignore results" in {
      using(range) in { j =>
        for (i <- 1 to j)
          JSonImpl5.jsonparser.value(vbigFileArray)
      }
    }*/

    /*measure method "FastParsers InputWindow to String" in {
      using(range) in { j =>
        for (i <- 1 to j)
          JSonImpl6.jsonparser.value(vbigFileArray)
      }
    }*/

    /*measure method "FastParsers on string input" in {
      using(range) in { j =>
        for (i <- 1 to j)
          JSonImpl1.jsonparser.value(bigFile)
      }
    }*/

    /*measure method "FastParsers on bigFileSeq input" in {
      using(range) in { j =>
        //for (i <- 1 to j)
          JSON.parse(JSON.value,bigFileSeq)
      }
    }

    measure method "FastParsers on bigFile input" in {
      using(range) in { j =>
        //for (i <- 1 to j)
          JSON.parse(JSON.value,bigFile)
      }
    }*/

  }
}
