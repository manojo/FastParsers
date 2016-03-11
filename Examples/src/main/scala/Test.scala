/**
 * Created with IntelliJ IDEA.
 * User: Eric
 * Date: 12.02.14
 * Time: 15:57
 * To change this template use File | Settings | File Templates.
 */

//because warnings

import fastparsers.framework.getAST
import fastparsers.framework.implementations.FastPrinters
import fastparsers.framework.parseresult._
import fastparsers.input.InputWindow
import fastparsers.parsers.Parser

import scala.collection.mutable.HashMap
import scala.language.reflectiveCalls
import scala.language.implicitConversions
import scala.reflect.ClassTag

object KVSchemaKnownRecognizeAuthorInfos {

   val login = "\"login\"".toCharArray
   val avatar_url = "\"avatar_url\"".toCharArray
   val gravatar_id = "\"gravatar_id\"".toCharArray
   val url = "\"url\"".toCharArray
   val html_url = "\"html_url\"".toCharArray
   val followers_url = "\"followers_url\"".toCharArray
   val following_url = "\"following_url\"".toCharArray
   val gists_url = "\"gists_url\"".toCharArray
   val starred_url = "\"starred_url\"".toCharArray
   val subscriptions_url = "\"subscriptions_url\"".toCharArray
   val organizations_url = "\"organizations_url\"".toCharArray
   val repos_url = "\"repos_url\"".toCharArray
   val events_url = "\"events_url\"".toCharArray
   val received_events_url = "\"received_events_url\"".toCharArray
   val authortype = "\"type\"".toCharArray
   val id = "\"id\"".toCharArray

   val site_admin = "\"site_admin\"".toCharArray


   val `true` = "true".toCharArray
   val `false` = "false".toCharArray

   import FastPrinters._
   lazy val parser = FastParser {

     def ws = skipws
     def braceOpen = ws ~> '{' ~> ws
     def braceClose = ws ~> '}' ~> ws
     def comma = ws ~> ',' ~> ws
     def colon = ws ~> ':' ~> ws

     def num = number
     def strLit = stringLitRec

     def loginParser = ws ~> litRec(login) ~> ws ~> ':' ~> ws ~> strLit ~> ws
     def avatar_urlParser = ws ~> litRec(avatar_url) ~> ws ~> ':' ~> ws ~> strLit ~> ws
     def gravatar_idParser = ws ~> litRec(gravatar_id) ~> ws ~> ':' ~> ws ~> strLit ~> ws
     def urlParser = ws ~> litRec(url) ~> ws ~> ':' ~> ws ~> strLit ~> ws
     def html_urlParser = ws ~> litRec(html_url) ~> ws ~> ':' ~> ws ~> strLit ~> ws
     def followers_urlParser = ws ~> litRec(followers_url) ~> ws ~> ':' ~> ws ~> strLit ~> ws
     def following_urlParser = ws ~> litRec(following_url) ~> ws ~> ':' ~> ws ~> strLit ~> ws
     def gists_urlParser = ws ~> litRec(gists_url) ~> ws ~> ':' ~> ws ~> strLit ~> ws
     def starred_urlParser = ws ~> litRec(starred_url) ~> ws ~> ':' ~> ws ~> strLit ~> ws
     def subscriptions_urlParser = ws ~> litRec(subscriptions_url) ~> ws ~> ':' ~> ws ~> strLit ~> ws
     def organizations_urlParser = ws ~> litRec(organizations_url) ~> ws ~> ':' ~> ws ~> strLit ~> ws
     def repos_urlParser = ws ~> litRec(repos_url) ~> ws ~> ':' ~> ws ~> strLit ~> ws
     def events_urlParser = ws ~> litRec(events_url) ~> ws ~> ':' ~> ws ~> strLit ~> ws
     def received_events_urlParser = ws ~> litRec(received_events_url) ~> ws ~> ':' ~> ws ~> strLit ~> ws
     def authortypeParser = ws ~> litRec(authortype) ~> ws ~> ':' ~> ws ~> strLit ~> ws

     def idParser = //: Parser[String] =
       ws ~> litRec(id) ~> colon ~> num ~> ws

     def siteAdmin: Parser[Unit] =
       ws ~> litRec(site_admin) ~> colon ~> (
         litRec(`true`) | litRec(`false`)
       )

     def authorInfo: Parser[Unit] = (braceOpen ~>
       (loginParser <~ comma) ~>
       (idParser <~ comma) ~>
       (avatar_urlParser <~ comma) ~>
       (gravatar_idParser <~ comma) ~>
       (urlParser <~ comma) ~>
       (html_urlParser <~ comma) ~>
       (followers_urlParser <~ comma) ~>
       (following_urlParser <~ comma) ~>
       (gists_urlParser <~ comma) ~>
       (starred_urlParser <~ comma) ~>
       (subscriptions_urlParser <~ comma) ~>
       (organizations_urlParser <~ comma) ~>
       (repos_urlParser <~ comma) ~>
       (events_urlParser <~ comma) ~>
       (received_events_urlParser <~ comma) ~>
       (authortypeParser <~ comma) ~>
       (siteAdmin ~> braceClose))

     def authorInfos = ((ws ~> '[' ~> ws) ~>
       repSepUnit(authorInfo, ws ~> ',' ~> ws) <~
     (ws ~> ']' ~> ws))

     def main = authorInfos
   }

   def main(args: Array[String])  {

    println("bla")
    pprint.pprintln(parser.ruleMap)
    //println("===============BEFORE============")
    //pprint.pprintln(parserPre.ruleMap("test2"))
    //println()
    //println("===============AFTER=============")
    //pprint.pprintln(parserPost.ruleMap("test2"))

   }
 }

/*
object Test {
  def isDigit(c: Char) = (c >= '0') && (c <= '9')

  val parserPre = {
    import fastparsers.framework.implementations.FastParsersCharArray._
    //import FastParsers._
    val arr = "greetings".toCharArray
    val parser = FastParsersCharArray {
      def digit2Int: Parser[Int] = acceptIf(isDigit) map { c =>
        (c - '0').toInt
      }
      def test = acceptIf(isDigit) ~ acceptIf(isDigit)
      def test2: Parser[Int] = rep(digit2Int) map {
        ls => ls.foldLeft[Int](0)((acc, x) => acc * 10 + x)
      }

      def myws = takeWhile3(x => x == ' ' || x == '\n')
      def nums: Parser[Unit] = repSepUnit(digit2Int, (skipws ~> ',' <~ skipws))
      def sum = digit2Int.foldLeft[Int](0, (acc, elem) => acc + elem)
      def greetings = litRec(arr)
    }
    parser
  }

  val parserPost = {
    import TransformedPrinters._
    val parser = FastParser {
      //def test2 = acceptIf(isDigit) ~ acceptIf(isDigit) map {
      //  case a ~ b => (a - '0', b - '0')
      //}
      def digit2Int: Parser[Int] = acceptIf(isDigit) map { c =>
        (c - '0').toInt
      }

      def test2: Parser[Int] = rep(digit2Int) map {
        ls => ls.foldLeft[Int](0)((acc, x) => acc * 10 + x)
      }

    }
    parser
  }

 def main(args: Array[String])  {

  println("bla")
  pprint.pprintln(parserPre.nums("1, 2, 3,   4".toCharArray))
  //println("===============BEFORE============")
  //pprint.pprintln(parserPre.ruleMap("test2"))
  //println()
  //println("===============AFTER=============")
  //pprint.pprintln(parserPost.ruleMap("test2"))

 }
}
*/
