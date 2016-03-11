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

  def description: String

//  lazy val range = Gen.enumeration("size")(files)

  def runBM(g: Gen[List[Array[Char]]], mName: String, meth: Rule): Unit = {
    measure method mName in {
      using(g) in { fs =>
        for (f <- fs) {
          performance of s"$mName on ${f.size}" in {
            val Success(res) = meth(f, 0)
            //println(res)
            res
          }
        }
      }
    }
  }

  /**
   * Design inspired by @nicolasstucki
   */
  def performanceOfParsers(measurer: Gen[List[Array[Char]]] => Unit)
                          (implicit files: Gen[List[Array[Char]]]): Unit = {
    performance of s"$description" config(
      Key.exec.benchRuns -> benchRuns,
      // Key.verbose -> false,
      Key.exec.independentSamples -> independentSamples,
      //Key.reports.resultDir -> "benchmark_results"
      //-XX:+PrintInlining"
      Key.exec.jvmflags -> s"-Xms2g -Xmx4g" // -XX:+UnlockDiagnosticVMOptions -XX:+PrintInlining -XX:+PrintCompilation"
    ) in {
      measurer(files)
    }
  }
}
