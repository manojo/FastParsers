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


  def independentSamples = 16
  def benchRunsPerSample = 128
  def benchRuns = independentSamples * benchRunsPerSample

  type Rule = (Array[Char], Int) => ParseResult[Any, _]

  def runBenchmark(desc: String, files: List[String],
                   methods: List[(String, Rule)]) = {

    val range = Gen.enumeration("size"){(files map { (f: String) =>
      val fileName = "FastParsers/src/test/resources/micro/" + f
      val file = scala.io.Source.fromFile(fileName).getLines mkString "\n"
      val fileArray = file.toCharArray
      //val fileSeq = new FastCharSequence(fileArray)
      fileArray
    }).toSeq }

    /**
     * Config taken from @nicolasstucki s rrb-vector stuff
     */
    performance of s"$desc" config(
      Key.exec.benchRuns -> benchRuns,
      // Key.verbose -> false,
      Key.exec.independentSamples -> independentSamples
      //Key.reports.resultDir -> "benchmark_results"
      //Key.exec.jvmflags -> s"-Xms$memoryInHeap -Xmx$memoryInHeap" // "-XX:+UnlockDiagnosticVMOptions -XX:+PrintInlining" "-XX:+PrintCompilation",
    ) in {
      for ((m, rule) <- methods) {
        measure method m in {
          using(range) in { fs =>
            for (f <- fs) {
              val Success(res) = rule(f, 0)
            }
          }
        }
      }
    }
  }
}
