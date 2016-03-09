import fastparsers.framework.implementations.FastParsers
import fastparsers.input.InputWindow
import org.scalatest._
import FastParsers._
import InputWindow._

import fastparsers.parsers.Parser
import TestsHelper._
import util.FastCharSequence

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
    comment: Option[String],
    model: String,
    format: String,
    text: String,
    sha1: String
  )

  case class Page(
    title: String,
    ns: Int,
    id: Int,
    redirect: Option[String],
    rev: Revision
  )

  case class Namespace(key: String, cs: String, content: String)

  case class SiteInfo(
    siteName: String,
    dbName: String,
    base: String,
    generator: String,
    cs: String,
    namespaces: List[Namespace]
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
      contributor ~ minor ~ opt(comment) ~ model ~ format ~
      text ~ sha1
    <~ wsWrap("</revision>")) map {
      case (((((((((id, pid), ts), cntr), mnr), cmt), mdl), fmt), txt), sha) =>
        Revision(id, pid, ts, cntr, mnr, cmt, mdl, fmt, txt, sha)
    }

    /**
     * And finally, the page
     */
    def page = (wsWrap("<page>") ~>
      title ~ ns ~ id ~ opt(redirect) ~ revision
    <~ wsWrap("</page>")) map {
      case ((((t, ns), id), red), rev) => Page(t, ns, id, red, rev)
    }

    def sitename: Parser[String] =
      wsWrap("<sitename>") ~> takeWhile(_ != '<') <~ wsWrap("</sitename>")

    def dbname: Parser[String] =
      wsWrap("<dbname>") ~> takeWhile(_ != '<') <~ wsWrap("</dbname>")

    def base: Parser[String] =
      wsWrap("<base>") ~> takeWhile(_ != '<') <~ wsWrap("</base>")

    def generator: Parser[String] =
      wsWrap("<generator>") ~> takeWhile(_ != '<') <~ wsWrap("</generator>")

    def cs: Parser[String] =
      wsWrap("<case>") ~> takeWhile(_ != '<') <~ wsWrap("</case>")

    def namespace1: Parser[Namespace] = (wsWrap("<namespace") ~>
      (wsWrap("key=") ~> stringLit) ~ (wsWrap("case=") ~> stringLit)
    ~ (wsWrap('>') ~> takeWhile(_ != '<') <~ wsWrap("</namespace>"))) map {
      case ((key, cs), ns) => Namespace(key.toString, cs.toString, ns)
    }

    def namespace2: Parser[Namespace] = (wsWrap("<namespace") ~>
      (wsWrap("key=") ~> stringLit) ~ (wsWrap("case=") ~> stringLit)
    <~ wsWrap("/>")) map {
      case (key, cs) => Namespace(key.toString, cs.toString, "")
    }

    def namespace = namespace1 | namespace2

    def namespaces: Parser[List[Namespace]] =
      wsWrap("<namespaces>") ~> rep(namespace) <~ wsWrap("</namespaces>")

    def siteinfo: Parser[SiteInfo] = (wsWrap("<siteinfo>") ~>
      sitename ~ dbname ~ base ~ generator ~ cs ~ namespaces
    <~ wsWrap("</siteinfo>")) map { case (((((stn, dbn) ,bs), gen), cs), nss) =>
        SiteInfo(stn, dbn, bs, gen, cs, nss)
    }

    def mediawikiOpen: Parser[Unit] = (wsWrap("<mediawiki") ~>
      wsWrap("xmlns=") ~> wsWrap(stringLit) ~>
      wsWrap("xmlns:xsi=") ~> wsWrap(stringLit) ~>
      wsWrap("xsi:schemaLocation=") ~> wsWrap(stringLit) ~>
      wsWrap("version=") ~> wsWrap(stringLit) ~>
      wsWrap("xml:lang=") ~> wsWrap(stringLit)
    ~> wsWrap('>')) map { _ => () }

    def fullParser: Parser[(SiteInfo, List[Page])] = (mediawikiOpen ~>
      siteinfo ~ rep(page)
    <~ wsWrap("</mediawiki>"))

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
        Some("add [[WP:RCAT|rcat]]s"),
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

  test("page test") {
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
        Some("Geography of Afghanistan"),
        Revision(
          407008307,
          74466619,
          TimeStamp(2011, 1, 10, 3, 56, 19),
          Contributor("Graham87", 194203),
          true,
          Some("""1 revision from [[:nost:AfghanistanGeography]]: import old edit,
          see [[User:Graham87/Import]]"""),
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

  test("siteinfo test") {
    shouldSucceed(parser.siteinfo) {
      """
      <siteinfo>
        <sitename>Wikipedia</sitename>
        <dbname>enwiki</dbname>
        <base>https://en.wikipedia.org/wiki/Main_Page</base>
        <generator>MediaWiki 1.27.0-wmf.10</generator>
        <case>first-letter</case>
        <namespaces>
          <namespace key="-2" case="first-letter">Media</namespace>
          <namespace key="-1" case="first-letter">Special</namespace>
          <namespace key="0" case="first-letter" />
          <namespace key="1" case="first-letter">Talk</namespace>
          <namespace key="2" case="first-letter">User</namespace>
          <namespace key="3" case="first-letter">User talk</namespace>
          <namespace key="4" case="first-letter">Wikipedia</namespace>
          <namespace key="5" case="first-letter">Wikipedia talk</namespace>
          <namespace key="6" case="first-letter">File</namespace>
          <namespace key="7" case="first-letter">File talk</namespace>
          <namespace key="8" case="first-letter">MediaWiki</namespace>
          <namespace key="9" case="first-letter">MediaWiki talk</namespace>
          <namespace key="10" case="first-letter">Template</namespace>
          <namespace key="11" case="first-letter">Template talk</namespace>
          <namespace key="12" case="first-letter">Help</namespace>
          <namespace key="13" case="first-letter">Help talk</namespace>
          <namespace key="14" case="first-letter">Category</namespace>
          <namespace key="15" case="first-letter">Category talk</namespace>
          <namespace key="100" case="first-letter">Portal</namespace>
          <namespace key="101" case="first-letter">Portal talk</namespace>
          <namespace key="108" case="first-letter">Book</namespace>
          <namespace key="109" case="first-letter">Book talk</namespace>
          <namespace key="118" case="first-letter">Draft</namespace>
          <namespace key="119" case="first-letter">Draft talk</namespace>
          <namespace key="446" case="first-letter">Education Program</namespace>
          <namespace key="447" case="first-letter">Education Program talk</namespace>
          <namespace key="710" case="first-letter">TimedText</namespace>
          <namespace key="711" case="first-letter">TimedText talk</namespace>
          <namespace key="828" case="first-letter">Module</namespace>
          <namespace key="829" case="first-letter">Module talk</namespace>
          <namespace key="2300" case="first-letter">Gadget</namespace>
          <namespace key="2301" case="first-letter">Gadget talk</namespace>
          <namespace key="2302" case="case-sensitive">Gadget definition</namespace>
          <namespace key="2303" case="case-sensitive">Gadget definition talk</namespace>
          <namespace key="2600" case="first-letter">Topic</namespace>
        </namespaces>
      </siteinfo>
      """ gives SiteInfo(
        "Wikipedia",
        "enwiki",
        "https://en.wikipedia.org/wiki/Main_Page",
        "MediaWiki 1.27.0-wmf.10",
        "first-letter",
        List(
          Namespace("-2", "first-letter", "Media"),
          Namespace("-1", "first-letter", "Special"),
          Namespace("0","first-letter", ""),
          Namespace("1","first-letter", "Talk"),
          Namespace("2","first-letter", "User"),
          Namespace("3","first-letter", "User talk"),
          Namespace("4","first-letter", "Wikipedia"),
          Namespace("5","first-letter", "Wikipedia talk"),
          Namespace("6","first-letter", "File"),
          Namespace("7","first-letter", "File talk"),
          Namespace("8","first-letter", "MediaWiki"),
          Namespace("9","first-letter", "MediaWiki talk"),
          Namespace("10","first-letter", "Template"),
          Namespace("11","first-letter", "Template talk"),
          Namespace("12","first-letter", "Help"),
          Namespace("13","first-letter", "Help talk"),
          Namespace("14","first-letter", "Category"),
          Namespace("15","first-letter", "Category talk"),
          Namespace("100","first-letter", "Portal"),
          Namespace("101","first-letter", "Portal talk"),
          Namespace("108","first-letter", "Book"),
          Namespace("109","first-letter", "Book talk"),
          Namespace("118","first-letter", "Draft"),
          Namespace("119","first-letter", "Draft talk"),
          Namespace("446","first-letter", "Education Program"),
          Namespace("447","first-letter", "Education Program talk"),
          Namespace("710","first-letter", "TimedText"),
          Namespace("711","first-letter", "TimedText talk"),
          Namespace("828","first-letter", "Module"),
          Namespace("829","first-letter", "Module talk"),
          Namespace("2300","first-letter", "Gadget"),
          Namespace("2301","first-letter", "Gadget talk"),
          Namespace("2302","case-sensitive", "Gadget definition"),
          Namespace("2303","case-sensitive", "Gadget definition talk"),
          Namespace("2600","first-letter", "Topic"))
        )
    }

    shouldFail(parser.siteinfo) {
      "bbacb"
    }
  }

  test("smaller xml file test") {
    import fastparsers.framework.parseresult.{ParseResult, Success, Failure}

    val file = scala.io.Source.fromFile(
      "FastParsers/src/test/resources/wiki/smaller.xml").getLines mkString "\n"

    val fileArray = file.toCharArray
    val charSeq = new FastCharSequence(fileArray)

    parser.fullParser(file, 0) match {
      case Success(result) => println("success kid ")
      case f @ Failure(msg) => fail("error : " + f)
    }
  }
}
