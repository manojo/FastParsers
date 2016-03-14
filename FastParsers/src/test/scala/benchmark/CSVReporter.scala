package benchmark

import java.text.SimpleDateFormat
import java.util.Calendar

import org.scalameter.CurveData
import org.scalameter._
import org.scalameter.utils.Tree

/** Greatly inspired by nicolasstucki. */
class CSVReporter[T: Numeric] extends Reporter[T] {

  def report(result: CurveData[T], persistor: Persistor): Unit = ()

  def report(results: Tree[CurveData[T]], persistor: Persistor) = {
    val times = scala.collection.mutable.Map.empty[(List[(Parameter[_], Any)], String), String]
    val curves = scala.collection.mutable.Map.empty[String, String]
    val scopes = scala.collection.mutable.SortedSet.empty[String]
    val scopeFiles = scala.collection.mutable.SortedSet.empty[String]
    val params = scala.collection.mutable.Set.empty[List[(Parameter[_], Any)]]

    for (result <- results) {
      for (measurement <- result.measurements) {
        val scopeFile = result.context.scope
          .replace(result.context.curve, "")
          // TODO Change these embedded java options
          .replace("benchmarks (-Xms16g -Xmx16g)", "")
          .replaceAll("Height [0-9]", "") + " "
        val scopeName = scopeFile + result.context.curve

        val _params = measurement.params.axisData.toList
        params += _params
        times((_params, scopeName)) = measurement.value.toString
        scopes += scopeName
        scopeFiles += scopeFile
        curves(scopeName) = result.context.curve
      }
    }

    implicit val ordInt = new Ordering[List[Int]] {
      override def compare(xs: List[Int], ys: List[Int]) = (xs, ys) match {
        case (x :: xs, y :: ys) =>
          val cmp = x compare y
          if (cmp == 0) compare(xs, ys)
          else cmp
        case (x :: xs, Nil) => -1
        case (Nil, y :: ys) => 1
        case (Nil, Nil) => 0
      }
    }

    for (scopeFile <- scopeFiles) {
      val sb = new StringBuilder()
      sb ++= params.head.map(_._1).mkString(", ") ++= ", " ++=
        scopes.iterator.filter(_.startsWith(scopeFile)).map(curves(_)
          .replace("[Int]", " ")).mkString("", ", ", "\n")

      for (param <- params.toList.sortBy(_.map {
        a => Integer.parseInt(a._2.toString)
      })) {
        sb ++= param.map(_._2).mkString(", ")
        sb ++= scopes.iterator.filter(_.startsWith(scopeFile))
          .map(scope => times.getOrElse((param, scope), ""))
          .fold("")((a, b) => s"$a, $b") += '\n'
      }

      import java.io._
      val dateTimeFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm")
      val dateTime = dateTimeFormat.format(Calendar.getInstance().getTime)
      val file = new File(s"./benchmarks/${
        scopeFile.replaceFirst("W*", "").replace(" ", "")}-$dateTime.csv")
      val bw = new BufferedWriter(new FileWriter(file))
      bw.write(sb.result())
      bw.close()

      log(sb.result())
    }
    true
  }
}
