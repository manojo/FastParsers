package benchmark

import fastparsers.framework.parseresult.{ParseResult, Success}
import org.scalameter.Bench.OfflineRegressionReport
import org.scalameter.picklers.Implicits._
import org.scalameter.api._
import org.scalameter.Key
import org.scalameter.picklers.TraversablePickler

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable

trait BenchmarkHelper extends OfflineRegressionReport {

  override def measurer: Measurer[Double] =
    new Measurer.PeriodicReinstantiation[Double]
      with Measurer.OutlierElimination[Double]
      with Measurer.RelativeNoise {
    def numeric: Numeric[Double] = implicitly[Numeric[Double]]
  }

  def tester: RegressionReporter.Tester
  def independentSamples = 1
  def benchRunsPerSample = 128
  def benchRuns = independentSamples * benchRunsPerSample

  def memoryInHeap = "16g"
  def data: Array[Char]

  type Rule = (Array[Char], Int) => ParseResult[Any, _]

  def description: String

  def runBM(g: Gen[String], mName: String, meth: Rule): Unit = {
    measure method mName in {
      using(g) in { fs =>
        for (f <- fs) {
          performance of s"$mName benchmark" in {
            meth(data, 0)
          }
        }
      }
    }
  }

  final val yourkitPath = "/home/jonnalag/yjp-2016.02/bin/linux-x86-64/libyjpagent.so"

  def performanceOfParsers[T](measurer: Gen[T] => Unit)(implicit seed: Gen[T]): Unit = {
    performance of s"$description" config(
      Key.exec.benchRuns -> benchRuns,
      Key.verbose -> true,
      Key.exec.independentSamples -> independentSamples,
      // Key.reports.resultDir -> "benchmark_results"
      Key.exec.jvmflags -> List(
        s"-Xms$memoryInHeap",
        s"-Xmx$memoryInHeap",
        s"-agentpath:$yourkitPath"
        // "-XX:+UnlockDiagnosticVMOptions",
        // "-XX:+PrintInlining",
        // "-XX:+PrintCompilation"
      )
    ) in { measurer(seed) }
  }
}
