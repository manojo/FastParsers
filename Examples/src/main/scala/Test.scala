/**
 * Created with IntelliJ IDEA.
 * User: Eric
 * Date: 12.02.14
 * Time: 15:57
 * To change this template use File | Settings | File Templates.
 */

//because warnings

import fastparsers.framework.getAST
import fastparsers.framework.implementations.{
  FastParsers,
  FastPrinters,
  FastArrayParsers,
  TransformedPrinters,
  FasterParsers
}
import fastparsers.framework.parseresult._
import fastparsers.input.InputWindow
import fastparsers.parsers.Parser

import scala.collection.mutable.HashMap
import scala.language.reflectiveCalls
import scala.language.implicitConversions
import scala.reflect.ClassTag


object Test {
  def isDigit(c: Char) = (c >= '0') && (c <= '9')

  val parserPre = {
    import FastPrinters._
    val parser = FastParser {
      /*def digit2Int: Parser[Int] = acceptIf(isDigit) map { c =>
        (c - '0').toInt
      }*/
      def test = acceptIf(isDigit) ~ acceptIf(isDigit)
      def test2 = test map {
        case a ~ b => (a - '0', b - '0')
      }
      /*def test2: Parser[Int] = rep(digit2Int) map {
        ls => ls.foldLeft[Int](0)((acc, x) => acc * 10 + x)
      }*/
    }
    parser
  }

  val parserPost = {
    import TransformedPrinters._
    val parser = FastParser {
      //def test2 = acceptIf(isDigit) ~ acceptIf(isDigit) map {
      //  case a ~ b => (a - '0', b - '0')
      //}
      /*def digit2Int: Parser[Int] = acceptIf(isDigit) map { c =>
        (c - '0').toInt
      }*/

      def test2 = acceptIf(isDigit) ~ acceptIf(isDigit) map {
        case a ~ b => (a - '0', b - '0')
      }

      /*def test2: Parser[Int] = rep(digit2Int) map {
        ls => ls.foldLeft[Int](0)((acc, x) => acc * 10 + x)
      }*/

    }
    parser
  }

 def main(args: Array[String]): Unit = {


  println("===============BEFORE============")
  pprint.pprintln(parserPre.ruleMap("test2"))
  println()
  println("===============AFTER=============")
  pprint.pprintln(parserPost.ruleMap("test2"))
 }
}
