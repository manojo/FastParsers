/**
 * This file contains functional tests between pre and post transformed
 * parsers, i.e. running pre transformed and post transformed
 * parsers on the same input yields the same results
 */

import fastparsers.framework.implementations.{FastParsers, FasterParsers}
import fastparsers.input.InputWindow
import org.scalatest._
import InputWindow._

import fastparsers.parsers.Parser
import TestsHelper._

import scala.language.reflectiveCalls

class TransformedSuite extends FunSuite {

  def isDigit(c: Char) = (c >= '0') && (c <= '9')

  val parserPre = {
    import FastParsers._
    val parser = FastParser {
      def digit2Int: Parser[Int] = acceptIf(isDigit) map { c =>
        (c - '0').toInt
      }

      def myNum: Parser[Int] = rep(digit2Int) map {
        ls => ls.foldLeft[Int](0)((acc, x) => acc * 10 + x)
      }
    }
    parser
  }

  val parserPost = {
    import FasterParsers._
    val parser = FastParser {
      def digit2Int: Parser[Int] = acceptIf(isDigit) map { c =>
        (c - '0').toInt
      }

      def myNum: Parser[Int] = rep(digit2Int) map {
        ls => ls.foldLeft[Int](0)((acc, x) => acc * 10 + x)
      }
    }
    parser
  }

  test("myNum test") {
    shouldSucceed(parserPre.myNum) {
      "12345" gives 12345
    }

    shouldSucceed(parserPost.myNum) {
      "12345" gives 12345
    }
  }
}
