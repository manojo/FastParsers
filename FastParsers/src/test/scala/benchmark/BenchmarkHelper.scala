package benchmark

import fastparsers.framework.parseresult.{ParseResult, Success}
import org.scalameter.PerformanceTest.{Microbenchmark, OfflineRegressionReport}
import org.scalameter.api._
import org.scalameter.Key


abstract class BenchmarkRun extends Microbenchmark

abstract class BenchmarkHelper extends Microbenchmark {

  def independentSamples = 1
  def benchRunsPerSample = 128
  def benchRuns = independentSamples * benchRunsPerSample

  def memoryInHeap = "8g"

  type Rule = (Array[Char], Int) => ParseResult[Any, _]

  def description: String

  def runBM(g: Gen[List[Array[Char]]], mName: String, meth: Rule): Unit = {
    measure method mName in {
      using(g) in { fs =>
        for (f <- fs) {
          performance of s"$mName on ${f.size}" in {
            val Success(res) = meth(f, 0)
            res
          }
        }
      }
    }
  }

  def performanceOfParsers(measurer: Gen[List[Array[Char]]] => Unit)
                          (implicit files: Gen[List[Array[Char]]]): Unit = {
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
