package benchmark

import fastparsers.framework.parseresult.{ParseResult, Success}
import org.scalameter.PerformanceTest.{Microbenchmark, OfflineRegressionReport}
import org.scalameter.api._
import org.scalameter.Key


abstract class BenchmarkRun extends OfflineRegressionReport {
  override def reporter = new Reporter.Composite(CSVReporter, super.reporter)
}

abstract class BenchmarkHelper extends OfflineRegressionReport {
  def independentSamples = 1
  def benchRunsPerSample = 128
  def benchRuns = independentSamples * benchRunsPerSample

  def memoryInHeap = "8g"
  def data: Array[Char]

  type Rule = (Array[Char], Int) => ParseResult[Any, _]

  def description: String

  def runBM(g: Gen[List[Int]], mName: String, meth: Rule): Unit = {
    measure method mName in {
      using(g) in { fs =>
        for (f <- fs) {
          performance of s"$mName benchmark" in {
            meth(data, 0)
            ()
          }
        }
      }
    }
  }

  def performanceOfParsers(measurer: Gen[List[Int]] => Unit)
                          (implicit files: Gen[List[Int]]): Unit = {
    performance of s"$description" config(
      Key.exec.benchRuns -> benchRuns,
      // Key.verbose -> false,
      Key.exec.independentSamples -> independentSamples,
      //Key.reports.resultDir -> "benchmark_results"
      Key.exec.jvmflags -> s"-Xms$memoryInHeap -Xmx$memoryInHeap"
      // -XX:+UnlockDiagnosticVMOptions -XX:+PrintInlining -XX:+PrintCompilation"
    ) in {
      measurer(files)
    }
  }
}
