package benchmark

import java.text.SimpleDateFormat
import java.util.Calendar

import fastparsers.framework.parseresult.{ParseResult, Success}
import org.scalameter.Bench.OfflineReport
import org.scalameter.Measurer.{RelativeNoise, OutlierElimination, PeriodicReinstantiation, MemoryFootprint}
import org.scalameter.picklers.Implicits._
import org.scalameter.api._
import org.scalameter.Key

trait BasicBenchmark extends OfflineReport {
  override def reporter = Reporter.Composite(new CSVReporter[Double], super.reporter)

  override def measurer: Measurer[Double] = new Measurer.Default
    with Measurer.PeriodicReinstantiation[Double]
    with Measurer.OutlierElimination[Double]
    with Measurer.RelativeNoise {
    def numeric: Numeric[Double] = implicitly[Numeric[Double]]
  }

  def tester: RegressionReporter.Tester
  def independentSamples = 1
  def benchRunsPerSample = 12
  def benchRuns = independentSamples * benchRunsPerSample

  def memoryInHeap = "2g"
  def data: Array[Char]

  type Rule = (Array[Char], Int) => ParseResult[Any, _]

  def description: String

  def runBM(g: Gen[String], mName: String, meth: Rule): Unit = {
    measure method mName in {
      using(g) in { fs =>
        for (f <- fs) {
          performance of s"$mName benchmark" in {
            val Success(res) = meth(data, 0)
          }
        }
      }
    }
  }

  final val home = sys.env.apply("HOME")
  final val yourkitPath = s"$home/yjp-2016.02/bin/linux-x86-64/libyjpagent.so"

  val dateTimeFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm")
  val dateTime = dateTimeFormat.format(Calendar.getInstance().getTime)

  def performanceOfParsers[T](measurer: Gen[T] => Unit)(implicit seed: Gen[T]): Unit = {
    performance of s"$description" config(
      Key.exec.benchRuns -> benchRuns,
      Key.exec.maxWarmupRuns -> 5,
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
      ),
      Key.reports.resultDir -> s"report-$dateTime"
    ) in { measurer(seed) }
  }
}

trait MemoryBenchmark extends BasicBenchmark {
  override def measurer: Measurer[Double] = new Measurer.MemoryFootprint
}

