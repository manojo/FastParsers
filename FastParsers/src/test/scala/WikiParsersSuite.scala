import fastparsers.framework.implementations.FastParsers
import fastparsers.input.InputWindow
import org.scalatest._
import FastParsers._
import InputWindow._

import fastparsers.parsers.Parser
import TestsHelper._

import scala.language.reflectiveCalls

class WikiParsersSuite extends FunSuite {

  case class TimeStamp(
    year: Int, month: Int, day: Int,
    hour: Int, min: Int, sec: Int
  )

  case class Contributor(name: String, id: Int)

  case class Revision(
    id: Int,
    parentId: Int,
    timestamp: TimeStamp,
    contributor: Contributor,
    minor: Boolean,
    comment: String,
    model: String,
    format: String,
    text: String,
    sha1: String
  )

  case class Page(
    title: String,
    ns: Int,
    id: Int,
    redirect: String,
    rev: Revision
  )

  val parser = FastParser {
    def ws = whitespaces
    def wsWrap[T](p: Parser[T]): Parser[T] =  ws ~> p <~ ws

    def digit2Int: Parser[Int] =
      acceptIf(c => c >= '0' && c <= '9') map { c => (c - '0').toInt }

    def numAsInt: Parser[Int] =
      digit2Int.foldLeft[Int](0, (acc, d) => acc * 10 + d)

    def title: Parser[String] =
      wsWrap("<title>") ~> takeWhile(_ != '<') <~ wsWrap("</title>")
    def ns: Parser[Int] = wsWrap("<ns>") ~> numAsInt <~ wsWrap("</ns>")
    def id: Parser[Int] = wsWrap("<id>") ~> numAsInt <~ wsWrap("</id>")

    def redirect:Parser[String] = (
      wsWrap("<redirect") ~> wsWrap("title=") ~> stringLit <~ wsWrap("/>")
    ) map (_.toString)

    /**
     * start components for revision parser
     */
    def parentid: Parser[Int] =
      wsWrap("<parentid>") ~> numAsInt <~ wsWrap("</parentid>")

    /**
     * datetime parser
     * for now we assume all is given in UTC. We should extend
     * with extra stuff if needed.
     * @see http://www.w3schools.com/xml/schema_dtypes_date.asp
     * for details
     */
    def year: Parser[Int] = rep(digit2Int, 4, 4) map {
      ls => ls.foldLeft[Int](0)((acc, d) => acc * 10 + d)
    }

    def nn: Parser[Int] = rep(digit2Int, 2, 2) map {
      ls => ls.foldLeft[Int](0)((acc, d) => acc * 10 + d)
    }

    /**
     * seems to be some bug in the unapply of ~, @TODO investigate
     */
    def timestamp: Parser[TimeStamp] = (wsWrap("<timestamp>") ~>
        year ~ ('-' ~> nn) ~ ('-' ~> nn) ~
        ('T' ~> nn) ~ (':' ~> nn) ~ (':' ~> nn) <~ 'Z'
      <~ wsWrap("</timestamp>")) map { case (((((yr, mnth), day), hh), mm), ss) =>
      TimeStamp(yr, mnth, day, hh, mm, ss)
    }

    def username: Parser[String] =
      wsWrap("<username>") ~> takeWhile(_ != '<') <~ wsWrap("</username>")

    def contributor: Parser[Contributor] = (
      wsWrap("<contributor>") ~> username ~ id <~ wsWrap("</contributor>")
    ) map { (Contributor.apply _).tupled }

    def minor: Parser[Boolean] =  opt(wsWrap("<minor />")) map { _.isDefined }

    def comment: Parser[String] =
      wsWrap("<comment>") ~> takeWhile(_ != '<') <~ wsWrap("</comment>")

    def model: Parser[String] =
      wsWrap("<model>") ~> takeWhile(_ != '<') <~ wsWrap("</model>")

    def format: Parser[String] =
      wsWrap("<format>") ~> takeWhile(_ != '<') <~ wsWrap("</format>")

    /**
     * @TODO we could explore the xml:space tag more if
     * required. Check the specification.
     */
    def text: Parser[String] = (wsWrap("<text xml:space=\"preserve\">") ~>
      takeWhile(_ != '<')
    <~ wsWrap("</text>"))

    /**
     * @TODO seems that the sha1 in this doc is represented in base 36,
     * didn't find enough info atm regarding this. So leaving it as a
     * Parser[String]
     */
    def sha1: Parser[String] =
      wsWrap("<sha1>") ~> takeWhile(_ != '<') <~ wsWrap("</sha1>")

    def revision: Parser[Revision] = (wsWrap("<revision>") ~>
      id ~ parentid ~ timestamp ~
      contributor ~ minor ~ comment ~ model ~ format ~
      text ~ sha1
    <~ wsWrap("</revision>")) map {
      case (((((((((id, pid), ts), cntr), mnr), cmt), mdl), fmt), txt), sha) =>
        Revision(id, pid, ts, cntr, mnr, cmt, mdl, fmt, txt, sha)
    }

    /**
     * And finally, the page
     */
    def page = (wsWrap("<page>") ~>
      title ~ ns ~ id ~ redirect ~ revision
    <~ wsWrap("</page>")) map {
      case ((((t, ns), id), red), rev) => Page(t, ns, id, red, rev)
    }

  }

  test("ns test") {
    shouldSucceed(parser.ns) {
      """
      <ns>
        12345
      </ns>
      """ gives 12345
    }

    shouldFail(parser.ns) {
      """<ns>
        12345as
      </ns>"""
    }
  }

  test("id test") {
    shouldSucceed(parser.id) {
      """<id>631144794</id>""" gives 631144794
    }

    shouldFail(parser.id) {
      """<id>1asd2</id>"""
    }
  }

  test("redirect test") {
    shouldSucceed(parser.redirect) {
      """<redirect title="Computer accessibility" />""" gives "Computer accessibility"
    }

    shouldFail(parser.redirect) {
      """<redirect title=22123 />"""
    }
  }

  test("parentid test") {
    shouldSucceed(parser.parentid) {
      """<parentid>381202555</parentid>""" gives 381202555
    }

    shouldFail(parser.parentid) {
      """<parentid>3812025awe55</parentid>"""
    }
  }

  test("timestamp test") {
    shouldSucceed(parser.timestamp) {
      """<timestamp>2014-10-26T04:50:23Z</timestamp>""" gives (
        TimeStamp(2014, 10, 26, 4, 50, 23)
      )
    }

    shouldFail(parser.timestamp) {
      """<timestamp>2014-10-26U04:50:23Z</timestamp>"""
    }

    shouldFail(parser.timestamp) {
      """<timestamp>20124-10-26U04:50:23Z</timestamp>"""
    }
  }

  test("username test") {
    shouldSucceed(parser.username) {
      """<username>Paine Ellsworth</username>""" gives "Paine Ellsworth"
    }
    shouldFail(parser.username) {
      """<username>Paine < Ellsworth</username>"""
    }
  }

  test("contributor test") {
    shouldSucceed(parser.contributor) {
      """
      <contributor>
        <username>Paine Ellsworth</username>
        <id>9092818</id>
      </contributor>
      """ gives Contributor("Paine Ellsworth", 9092818)
    }

    shouldFail(parser.contributor) {
      """<contributor>asdf</contributor>"""
    }
  }

  test("comment test") {
    shouldSucceed(parser.comment) {
      """
      <comment>add [[WP:RCAT|rcat]]s</comment>
      """ gives "add [[WP:RCAT|rcat]]s"
    }

    shouldFail(parser.comment) {
      """<comment>as < df</comment>"""
    }
  }

  test("model test") {
    shouldSucceed(parser.model) {
      """
      <model>wikitext</model>
      """ gives "wikitext"
    }

    shouldFail(parser.model) {
      """<model>as < df</model>"""
    }
  }

  test("format test") {
    shouldSucceed(parser.format) {
      """
      <format>text/x-wiki</format>
      """ gives "text/x-wiki"
    }

    shouldFail(parser.format) {
      """<format>as < df</format>"""
    }
  }

  test("text test") {
    shouldSucceed(parser.text) {
      """<text xml:space="preserve">#REDIRECT [[Computer accessibility]]

{{Redr|move|from CamelCase|up}}</text>
      """ gives """#REDIRECT [[Computer accessibility]]

{{Redr|move|from CamelCase|up}}"""

    }

    shouldFail(parser.text) {
      """<text>as < df</text>"""
    }
  }

  test("sha1 test") {
    shouldSucceed(parser.sha1) {
      """
      <sha1>d4tdz2eojqzamnuockahzcbrgd1t9oi</sha1>
      """ gives "d4tdz2eojqzamnuockahzcbrgd1t9oi"
    }

    shouldFail(parser.sha1) {
      """<sha1>as < df</sha1>"""
    }
  }

  test("revision test") {
    shouldSucceed(parser.revision) {
      """
      <revision>
        <id>631144794</id>
        <parentid>381202555</parentid>
        <timestamp>2014-10-26T04:50:23Z</timestamp>
        <contributor>
          <username>Paine Ellsworth</username>
          <id>9092818</id>
        </contributor>
        <comment>add [[WP:RCAT|rcat]]s</comment>
        <model>wikitext</model>
        <format>text/x-wiki</format>
        <text xml:space="preserve">#REDIRECT [[Computer accessibility]]

        {{Redr|move|from CamelCase|up}}</text>
        <sha1>4ro7vvppa5kmm0o1egfjztzcwd0vabw</sha1>
      </revision>
      """ gives Revision(
        631144794,
        381202555,
        TimeStamp(2014, 10, 26, 4, 50, 23),
        Contributor("Paine Ellsworth", 9092818),
        false,
        "add [[WP:RCAT|rcat]]s",
        "wikitext",
        "text/x-wiki",
        """#REDIRECT [[Computer accessibility]]

        {{Redr|move|from CamelCase|up}}""",
        "4ro7vvppa5kmm0o1egfjztzcwd0vabw"
      )
    }

    shouldFail(parser.revision) {
      """<revision>as < df</revision>"""
    }
  }

  test("page test1") {
    shouldSucceed(parser.page) {
      """
      <page>
        <title>AfghanistanGeography</title>
        <ns>0</ns>
        <id>14</id>
        <redirect title="Geography of Afghanistan" />
        <revision>
          <id>407008307</id>
          <parentid>74466619</parentid>
          <timestamp>2011-01-10T03:56:19Z</timestamp>
          <contributor>
            <username>Graham87</username>
            <id>194203</id>
          </contributor>
          <minor />
          <comment>1 revision from [[:nost:AfghanistanGeography]]: import old edit,
          see [[User:Graham87/Import]]</comment>
          <model>wikitext</model>
          <format>text/x-wiki</format>
          <text xml:space="preserve">#REDIRECT [[Geography of Afghanistan]] {{R from CamelCase}}</text>
          <sha1>0uwuuhiam59ufbu0uzt9lookwtx9f4r</sha1>
        </revision>
      </page>
      """ gives Page(
        "AfghanistanGeography",
        0,
        14,
        "Geography of Afghanistan",
        Revision(
          407008307,
          74466619,
          TimeStamp(2011, 1, 10, 3, 56, 19),
          Contributor("Graham87", 194203),
          true,
          """1 revision from [[:nost:AfghanistanGeography]]: import old edit,
          see [[User:Graham87/Import]]""",
          "wikitext",
          "text/x-wiki",
          "#REDIRECT [[Geography of Afghanistan]] {{R from CamelCase}}",
          "0uwuuhiam59ufbu0uzt9lookwtx9f4r"
        )
      )
    }

    shouldFail(parser.page) {
      "bbacb"
    }
  }
}
