package benchmark

import fastparsers.framework.parseresult.{ParseResult, Success, Failure}
import fastparsers.input.InputWindow
import org.scalameter.api._
import org.scalameter.Key
import InputWindow._
import util.FastCharSequence

abstract class BenchmarkHelper extends PerformanceTest.OfflineReport {

  //lazy val executor = LocalExecutor(
  //  new Executor.Warmer.Default,
  //  Aggregator.average,
  //  new Measurer.Default)

  //lazy val reporter = new LoggingReporter
  //lazy val reporter = Reporter.Composite(
  //  new RegressionReporter(
  //    RegressionReporter.Tester.OverlapIntervals(),
  //    RegressionReporter.Historian.ExponentialBackoff() ),
  //  HtmlReporter(true)
  //)

  //lazy val persistor = Persistor.None


  def independentSamples = 1 //16
  def benchRunsPerSample = 128
  def benchRuns = independentSamples * benchRunsPerSample

  def memoryInHeapSeq = Seq("16g")

  type Rule = (Array[Char], Int) => ParseResult[Any, _]

  /** every benchmark must provide files to run on */
  def files: List[Array[Char]]
  def description: String

  lazy val range = Gen.enumeration("size")(files)

  def runBM(f: Array[Char], mName: String, meth: Rule): Unit = {
    performance of s"$mName" in {
      measure method mName in {
        val Success(res) = meth(f, 0)
        //println(res)
        res
      }
    }
  }

  /**
   * Design inspired by @nicolasstucki
   */
  def performanceOfParsers(measurer: Array[Char] => Unit): Unit = {
    performance of s"$description" config(
      Key.exec.benchRuns -> benchRuns,
      // Key.verbose -> false,
      Key.exec.independentSamples -> independentSamples,
      //Key.reports.resultDir -> "benchmark_results"
      Key.exec.jvmflags -> s"-Xms2g -Xmx4g" // "-XX:+UnlockDiagnosticVMOptions -XX:+PrintInlining" "-XX:+PrintCompilation",
    ) in {
      using(range) in { fs =>
        for (f <- fs) {
          measurer(f)
        }
      }
    }
  }
}
