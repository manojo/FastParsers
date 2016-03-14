package benchmark

import fastparsers.framework.parseresult.{ParseResult, Success}
import org.scalameter.Bench.OfflineRegressionReport
import org.scalameter.picklers.Implicits._
import org.scalameter.api._
import org.scalameter.Key
import org.scalameter.picklers.TraversablePickler

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable

object FileImplicits {
  object CharArrayPickler extends TraversablePickler[mutable.WrappedArray, Char] {
    protected def canBuildFrom =
      implicitly[CanBuildFrom[Nothing, Char, mutable.WrappedArray[Char]]]
  }

  implicit val arrayCharPickler = CharArrayPickler

  object ListCharArrayPickler extends TraversablePickler[List, mutable.WrappedArray[Char]] {
    protected def canBuildFrom =implicitly[CanBuildFrom[Nothing,
      mutable.WrappedArray[Char], List[mutable.WrappedArray[Char]]]]
  }

  implicit val listArrayCharPickler = ListCharArrayPickler
}


trait BenchmarkHelper extends OfflineRegressionReport {
  def independentSamples = 1
  def benchRunsPerSample = 128
  def benchRuns = independentSamples * benchRunsPerSample

  def memoryInHeap = "16g"

  type Rule = (Array[Char], Int) => ParseResult[Any, _]

  def description: String

  def runBM(g: Gen[List[mutable.WrappedArray[Char]]], mName: String, meth: Rule): Unit = {
    measure method mName in {
      using(g) in { fs =>
        for (f <- fs) {
          performance of s"$mName benchmark" in {
            meth(f.array, 0)
          }
        }
      }
    }
  }

  final val yourkitPath = "/home/jonnalag/yjp-2016.02/bin/linux-x86-64/libyjpagent.so"

  def performanceOfParsers[T](measurer: Gen[T] => Unit)(implicit seed: Gen[T]): Unit = {
    performance of s"$description" config(
      Key.exec.benchRuns -> benchRuns,
      // Key.verbose -> false,
      Key.exec.independentSamples -> independentSamples,
      //Key.reports.resultDir -> "benchmark_results"
      Key.exec.jvmflags -> List(
        s"-Xms$memoryInHeap",
        s"-Xmx$memoryInHeap",
        s"-agentpath:$yourkitPath"
      )
      // -XX:+UnlockDiagnosticVMOptions -XX:+PrintInlining -XX:+PrintCompilation"
    ) in {
      measurer(seed)
    }
  }
}
