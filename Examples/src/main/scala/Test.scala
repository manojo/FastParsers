/**
 * Created with IntelliJ IDEA.
 * User: Eric
 * Date: 12.02.14
 * Time: 15:57
 * To change this template use File | Settings | File Templates.
 */
import scala.reflect.runtime.universe._

import scala.util.parsing.input._
object Test {

  implicit def stringToCharSeqReader(s:String) = new CharSequenceReader(s)
  implicit def stringToStreamMarkedArray(s:String) = new StreamMarkedArray(s.toCharArray)

  def main(args: Array[String]) {
    import FastParsers._
    val parser = FastParser{
      def rule1 = 'b' ~ 'a' ~ rule2// ~ 'b' ~ 'l'
      def rule2 = 'c' ~ 'b'

      def rule3 = ('a' ~ 'b').rep(2,3) ~ 'c'
      def rule4 = ('a' ~ 'b').+ ~ 'c'
      def rule5 = ('a' ~ 'b').* ~ 'c'
      def rule6 = ('a' ~ 'b').? ~ 'c'
      //def rule7 = rule1
      def rule8 = (rep('b',0,-1) ~ ('a' || 'c')) ^^ {case (x:List[_],y) => println(x.size);y}

      def rule9 = rep1(range('0','9'))

      def rule10 = range('0','9') ~> rep(range('a','z')) <~ '0'

      def rule11 = phrase('a' ~ 'b' ~ 'c')

      def rule12 = guard('a' ~ 'b' ~ 'c') ~ rep(range('a','z'))
      def rule13 = not('a' ~ 'b' ~ 'c') ~ rep(range('a','z'))

      def rule14 = rule1 ~ ('c' || 'd')

      def rule15 = 'a' ~ 'b' ~ 'c' ^^^ 1

      def rule16 = 'a' ~ ('b' ^^^ 2) ~ 'c'
      def rule17 = 'a' ~ ('b' ^^ {_ => 2}) ~ 'c'

      def rule18 = rep('a',3,3) | (rep('a',2,2) ~ 'b')
      def rule19 = phrase(rep('a',0,3)) | rep('a',0,4)
      def rule20 = (rep('a',0,3) ~ 'b') | rep('a' || 'b')

      def rule21 = ((rep(range('a','z'))) filter {case x:List[_] => x.mkString == "salut" || x.mkString == "hello"})

      def rule22 = repFold(range('0','9'))(0){(y:Int,x:Any) => x.asInstanceOf[Char].asDigit + 1 +y}
      def rule23 = range('0','9').repFold(1){(y:Int,x:Char) => x.asDigit * y}

      def rule24 = phrase(rep(range('a','z') || '?'))

      def rule25:Parser[Char] = 'a' ~ rule26
      def rule26:Parser[Char] = 'b' || rule25

      def rule27 = rep('a' ~ 'b') ~ rep('a')
      def rule28 = rep('a') ~ 'b'
      /*def rule29 = seq(List('s','a','l','u','t'))
      def rule30 = seq("salut")   */

      def rule31 = 'a' ~ ('b' withFailureMessage("JE VEUX UN 'b' ICI")) ~ 'c'
    }

    parser.rule1("bacb") match {
      case Success(result) => println(result)
      case Failure(msg) => println("error 1: " + msg)
    }

   parser.rule3("ababcc") match {
      case Success(result) => println(result)
      case Failure(msg) => println("error 3: " + msg)
    }

    parser.rule8("bbbbbbc") match {
      case Success(result) => println(result)
      case Failure(msg) => println("error 8: " + msg)
    }

    parser.rule9("065614486") match {
      case Success(result) => println(result)
      case Failure(msg) => println("error 9: " + msg)
    }

    parser.rule10("5sdawddggh0") match {
      case Success(result) => println(result)
      case Failure(msg) => println("error 10: " + msg)
    }
    parser.rule11("abcd") match {
      case Success(result) => println(result)
      case Failure(msg) => println("error 11: " + msg)
    }

    parser.rule12("abcdfgha") match {
      case Success(result) => println(result)
      case Failure(msg) => println("error 12: " + msg)
    }

    parser.rule13("dbcgha") match {
      case Success(result) => println(result)
      case Failure(msg) => println("error 13: " + msg)
    }

    parser.rule14("bacbd") match {
      case Success(result) => println(result)
      case Failure(msg) => println("error 14: " + msg)
    }

    parser.rule15("abc") match {
      case Success(result) => println(result)
      case Failure(msg) => println("error 15: " + msg)
    }

    parser.rule16("abc") match {
      case Success(result) => println(result)
      case Failure(msg) => println("error 16: " + msg)
    }

    parser.rule17("abc") match {
      case Success(result) => println(result)
      case Failure(msg) => println("error 17: " + msg)
    }

    parser.rule18("aab") match {
      case Success(result) => println(result)
      case Failure(msg) => println("error 18: " + msg)
    }

    parser.rule19("aaaa") match {
      case Success(result) => println(result)
      case Failure(msg) => println("error 19: " + msg)
    }

    parser.rule20("aaabb") match {
      case Success(result) => println(result)
      case Failure(msg) => println("error 20: " + msg)
    }

    parser.rule21("salu") match {
      case Success(result) => println(result)
      case Failure(msg) => println("error 21: " + msg)
    }

    parser.rule22("5623") match {
      case Success(result) => println(result)
      case Failure(msg) => println("error 22: " + msg)
    }

    parser.rule23("5226") match {
      case Success(result) => println(result)
      case Failure(msg) => println("error 23: " + msg)
    }

    parser.rule24("") match {
      case Success(result) => println(result)
      case Failure(msg) => println("error 24: " + msg)
    }

    parser.rule25("aaaaab") match {
      case Success(result) => println(result)
      case Failure(msg) => println("error 25: " + msg)
    }
    parser.rule27("ababaaa") match {
      case Success(result) => println(result)
      case Failure(msg) => println("error 27: " + msg)
    }
    parser.rule28("aaaaab") match {
      case Success(result) => println(result)
      case Failure(msg) => println("error 28: " + msg)
    }

    parser.rule31("acc") match {
      case Success(result) => println(result)
      case Failure(msg) => println("error 31: " + msg)
    }
  }
}