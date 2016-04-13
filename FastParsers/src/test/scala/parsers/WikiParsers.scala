package parsers

import fastparsers.input.InputWindow
import fastparsers.parsers.Parser
import scala.util.parsing.combinator._
import scala.util.parsing.input._

object WikiParsers {

  import fastparsers.framework.implementations.FastParsersCharArray._
  import fastparsers.input.InputWindow.InputWindow

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

  case class Namespace(
    key: String,
    cs: String,
    content: String
  )

  case class SiteInfo(
    siteName: String,
    dbName: String,
    base: String,
    generator: String,
    cs: String,
    namespaces: List[Namespace]
  )

  case class RelevantInfo(
    title: String,
    redirectTitle: Option[String],
    timeStamp: TimeStamp,
    contributor: String,
    text: String
  )

  case class WikipediaPage(
    title: String,
    redirectTitle: String,
    timestamp: String,
    lastContributorUsername: String,
    text: String)

  def parseXMLtoWikipediaPage(xml: String) : WikipediaPage = {
    val elem = scala.xml.XML.loadString(xml)
    return new WikipediaPage(
      (elem \ "title").text,
      (elem \ "redirect" \ "@title").text,
      (elem \ "revision" \ "timestamp").text,
      (elem \ "revision" \ "contributor" \ "username").text,
      (elem \ "revision" \ "text").text.replaceAll("\n", " ")  // Hive doesn't like \n in column strings.
    )
  }

  val titleOpen = "<title>".toCharArray
  val titleClose = "</title>".toCharArray

  val restrictionsOpen = "<restrictions>".toCharArray
  val restrictionsClose = "</restrictions>".toCharArray

  val nsOpen = "<ns>".toCharArray
  val nsClose = "</ns>".toCharArray

  val idOpen = "<id>".toCharArray
  val idClose = "</id>".toCharArray

  val redirectOpen = "<redirect".toCharArray
  val titleEq = "title=".toCharArray
  val singleClose = "/>".toCharArray

  val parentIdOpen = "<parentid>".toCharArray
  val parentIdClose = "</parentid>".toCharArray

  val timestampOpen = "<timestamp>".toCharArray
  val timestampClose =  "</timestamp>".toCharArray

  val usernameOpen = "<username>".toCharArray
  val usernameClose = "</username>".toCharArray

  val contributorOpen = "<contributor>".toCharArray
  val contributorClose = "</contributor>".toCharArray

  val minorArr = "<minor />".toCharArray

  val commentOpen = "<comment>".toCharArray
  val commentClose = "</comment>".toCharArray

  val modelOpen = "<model>".toCharArray
  val modelClose = "</model>".toCharArray

  val formatOpen = "<format>".toCharArray
  val formatClose = "</format>".toCharArray

  val textOpen = "<text xml:space=\"preserve\">".toCharArray
  val textClose = "</text>".toCharArray

  val shaOpen = "<sha1>".toCharArray
  val shaClose = "</sha1>".toCharArray

  val revisionOpen = "<revision>".toCharArray
  val revisionClose = "</revision>".toCharArray

  val pageOpen = "<page>".toCharArray
  val pageClose = "</page>".toCharArray

  val sitenameOpen = "<sitename>".toCharArray
  val sitenameClose = "</sitename>".toCharArray

  val dbnameOpen = "<dbname>".toCharArray
  val dbnameClose = "</dbname>".toCharArray

  val baseOpen = "<base>".toCharArray
  val baseClose = "</base>".toCharArray

  val generatorOpen = "<generator>".toCharArray
  val generatorClose = "</generator>".toCharArray

  val caseOpen = "<case>".toCharArray
  val caseClose = "</case>".toCharArray

  val namespaceOpen = "<namespace".toCharArray
  val namespaceClose = "</namespace>".toCharArray

  val keyEq = "key=".toCharArray
  val caseEq = "case=".toCharArray

  val namespacesOpen = "<namespaces>".toCharArray
  val namespacesClose = "</namespaces>".toCharArray

  val siteinfoOpen = "<siteinfo>".toCharArray
  val siteinfoClose = "</siteinfo>".toCharArray

  val mediawikiOpen = "<mediawiki".toCharArray
  val mediawikiClose = "</mediawiki>".toCharArray

  val xmlns = "xmlns=".toCharArray
  val xmlnsxsi = "xmlns:xsi=".toCharArray
  val schemaLocation = "xsi:schemaLocation=".toCharArray
  val version = "version=".toCharArray
  val lang = "xml:lang=".toCharArray

  object FullWikiParser {
    lazy val parser = FastParsersCharArray {

      def ws = skipws

      def digit2Int: Parser[Int] =
        acceptIf(c => c >= '0' && c <= '9') map { c => (c - '0').toInt }

      def numAsInt: Parser[Int] =
        digit2Int.foldLeft[Int](0, (acc, d) => acc * 10 + d)

      def title: Parser[String] = ((ws ~> titleOpen <~ws) ~>
        takeWhile(_ != '<')
      <~ (ws ~> titleClose <~ ws)) map (_.mkString)

      def ns: Parser[Int] = (ws ~> litRec(nsOpen) ~> ws) ~> numAsInt <~ (ws ~> litRec(nsClose) ~> ws)
      def id: Parser[Int] = (ws ~> litRec(idOpen) ~> ws) ~> numAsInt <~ (ws ~> litRec(idClose) ~> ws)
      def restrictions: Parser[String] = ((ws ~> litRec(restrictionsOpen) ~> ws) ~>
        takeWhile(_ != '<')
      <~ (ws ~> litRec(restrictionsClose) ~> ws)) map (_.toString)

      def redirect: Parser[String] = ((ws ~> litRec(redirectOpen) ~> ws) ~>
        (ws ~> litRec(titleEq) <~ ws) ~> stringLit
      <~ (ws ~> singleClose <~ ws)) map (_.toString)

      def parentid: Parser[Int] =
        (ws ~> parentIdOpen <~ ws) ~> numAsInt <~ (ws ~> parentIdClose <~ ws)

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
      def timestamp: Parser[TimeStamp] = ((ws ~> timestampOpen <~ ws) ~>
          year ~ ('-' ~> nn) ~ ('-' ~> nn) ~
          ('T' ~> nn) ~ (':' ~> nn) ~ (':' ~> nn) <~ 'Z'
        <~ (ws ~> timestampClose <~ ws)) map { case (((((yr, mnth), day), hh), mm), ss) =>
        TimeStamp(yr, mnth, day, hh, mm, ss)
      }

      def username: Parser[String] = ((ws ~> usernameOpen <~ ws) ~>
        takeWhile(_ != '<') <~ (ws ~> usernameClose <~ ws)
      ) map (_.mkString)

      def contributor: Parser[Contributor] = (
        (ws ~> contributorOpen <~ ws) ~> username ~ id <~ (ws ~> contributorClose <~ ws)
      ) map { (Contributor.apply _).tupled }

      def minor: Parser[Boolean] =  opt((ws ~> minorArr <~ ws)) map { _.isDefined }

      def comment: Parser[String] = ((ws ~> commentOpen <~ ws) ~>
        takeWhile(_ != '<')
      <~ (ws ~> commentClose <~ ws)) map (_.mkString)

      def model: Parser[String] = ((ws ~> modelOpen <~ ws) ~>
        takeWhile(_ != '<')
      <~ (ws ~> modelClose <~ ws)) map (_.mkString)


      def format: Parser[String] = ((ws ~> formatOpen <~ ws) ~>
        takeWhile(_ != '<') <~ (ws ~> formatClose <~ ws)
      ) map (_.mkString)

      /**
       * @TODO we could explore the xml:space tag more if
       * required. Check the specification.
       */
      def text: Parser[String] = ((ws ~> textOpen <~ ws) ~>
        takeWhile(_ != '<')
      <~ (ws ~> textClose <~ ws)) map (_.mkString)

      /**
       * @TODO seems that the sha1 in this doc is represented in base 36,
       * didn't find enough info atm regarding this. So leaving it as a
       * Parser[String]
       */
      def sha1: Parser[String] = ((ws ~> shaOpen <~ ws) ~>
        takeWhile(_ != '<') <~ (ws ~> shaClose <~ ws)
      ) map (_.mkString)

      def revision: Parser[Revision] = ((ws ~> revisionOpen <~ ws) ~>
        id ~ parentid ~ timestamp ~
        contributor ~ minor ~ opt(comment) ~ model ~ format ~
        text ~ sha1
      <~ (ws ~> revisionClose <~ ws)) map {
        case (((((((((id, pid), ts), cntr), mnr), cmt), mdl), fmt), txt), sha) =>
          Revision(id, pid, ts, cntr, mnr, cmt, mdl, fmt, txt, sha)
      }

      /**
       * And finally, the page
       */
      def page = ((ws ~> pageOpen <~ ws) ~>
        title ~ ns ~ id ~ opt(restrictions) ~ opt(redirect) ~ revision
      <~ (ws ~> pageClose <~ ws)) map {
        case (((((t, ns), id), _), red), rev) => Page(t, ns, id, red, rev)
      }

      def sitename: Parser[String] = ((ws ~> sitenameOpen <~ ws) ~>
        takeWhile(_ != '<') <~ (ws ~> sitenameClose <~ ws)
      ) map (_.mkString)

      def dbname: Parser[String] = ((ws ~> dbnameOpen <~ ws) ~>
        takeWhile(_ != '<')
      <~ (ws ~> dbnameClose <~ ws)) map (_.mkString)


      def base: Parser[String] = ((ws ~> baseOpen <~ ws) ~>
        takeWhile(_ != '<')
      <~ (ws ~> baseClose <~ ws)) map (_.mkString)

      def generator: Parser[String] = ((ws ~> generatorOpen <~ ws) ~>
        takeWhile(_ != '<')
      <~ (ws ~> generatorClose <~ ws)) map (_.mkString)

      def cs: Parser[String] = ((ws ~> caseOpen <~ ws) ~>
        takeWhile(_ != '<')
      <~ (ws ~> caseClose <~ ws)) map (_.mkString)

      def namespace1: Parser[Namespace] = ((ws ~> namespaceOpen <~ ws) ~>
        ((ws ~> keyEq <~ ws) ~> stringLit) ~ ((ws ~> caseEq <~ ws) ~> stringLit)
      ~ ((ws ~> '>' <~ ws) ~> takeWhile(_ != '<') <~ (ws ~> namespaceClose <~ ws))) map {
        case ((key, cs), ns) => Namespace(key.toString, cs.toString, ns.mkString)
      }

      def namespace2: Parser[Namespace] = ((ws ~> namespaceOpen <~ ws) ~>
        ((ws ~> keyEq <~ ws) ~> stringLit) ~ ((ws ~> caseEq <~ ws) ~> stringLit)
      <~ (ws ~> singleClose <~ ws)) map {
        case (key, cs) => Namespace(key.toString, cs.toString, "")
      }

      def namespace = namespace1 | namespace2

      def namespaces: Parser[List[Namespace]] =
        (ws ~> namespacesOpen <~ ws) ~> rep(namespace) <~ (ws ~> namespacesClose <~ ws)

      def siteinfo: Parser[SiteInfo] = ((ws ~> siteinfoOpen <~ ws) ~>
        sitename ~ dbname ~ base ~ generator ~ cs ~ namespaces
      <~ (ws ~> siteinfoClose <~ ws)) map { case (((((stn, dbn) ,bs), gen), cs), nss) =>
          SiteInfo(stn, dbn, bs, gen, cs, nss)
      }

      def mediawikiOpen: Parser[Unit] = ((ws ~> mediawikiOpen <~ ws) ~>
        (ws ~> xmlns <~ ws) ~> (ws ~> stringLit <~ ws) ~>
        (ws ~> xmlnsxsi <~ ws) ~> (ws ~> stringLit <~ ws) ~>
        (ws ~> schemaLocation <~ ws) ~> (ws ~> stringLit <~ ws) ~>
        (ws ~> version <~ ws) ~> (ws ~> stringLit <~ ws) ~>
        (ws ~> lang <~ ws) ~> (ws ~> stringLit <~ ws)
      ~> (ws ~> '>' <~ ws)) map { _ => () }

      def fullParser: Parser[(SiteInfo, List[Page])] = (mediawikiOpen ~>
        siteinfo ~ rep(page)
      <~ (ws ~> mediawikiClose <~ ws))

      /**
       * Only keep relevant info as per the wiki example on databricks
       *
       *  return new WikipediaPage(
       *    (elem \ "title").text,
       *    (elem \ "redirect" \ "@title").text,
       *    (elem \ "revision" \ "timestamp").text,
       *    (elem \ "revision" \ "contributor" \ "username").text,
       *    (elem \ "revision" \ "text").text.replaceAll("\n", " ")  // Hive doesn't like \n in column strings.
       *  )
       */
      def relevantInfos: Parser[List[RelevantInfo]] = fullParser map {
        case (_, pages) => pages map { page =>
          RelevantInfo(
            page.title,
            page.redirect,
            page.rev.timestamp,
            page.rev.contributor.name,
            page.rev.text
          )
        }
      }
    }
  }
/*
  object RelevantWikiParser {
    lazy val parser = FastParsersCharArray {

      def digit2Int: Parser[Int] =
        acceptIf(c => c >= '0' && c <= '9') map { c => (c - '0').toInt }

      def ws = whitespaces

      def title: Parser[String] = ((ws ~> titleOpen <~ ws) ~>
        takeWhile(_ != '<')
      <~ (ws ~> titleClose <~ ws)) map (_.mkString)

      def redirect: Parser[String] = ((ws ~> redirectOpen <~ ws) ~>
        (ws ~> titleEq <~ ws) ~> stringLit
      <~ (ws ~> singleClose <~ ws))

      def minor: Parser[Boolean] =  opt((ws ~> minorArr <~ ws)) map { _.isDefined }

      def sitename2 = ((ws ~> sitenameOpen <~ ws) ~>
        takeWhile2(_ != '<') <~ (ws ~> sitenameClose <~ ws)
      )

      def dbname2 =
        (ws ~> dbnameOpen <~ ws) ~> takeWhile2(_ != '<') ~> (ws ~> dbnameClose <~ ws)

      def base2 =
        (ws ~> baseOpen <~ ws) ~> takeWhile2(_ != '<') ~> (ws ~> baseClose <~ ws)

      def generator2 =
        (ws ~> generatorOpen <~ ws) ~> takeWhile2(_ != '<') ~> (ws ~> generatorClose <~ ws)

      def cs2 =
        (ws ~> caseOpen <~ ws) ~> takeWhile2(_ != '<') ~> (ws ~> caseClose <~ ws)

      def namespace1B: Parser[Unit] = ((ws ~> namespaceOpen <~ ws) ~>
        ((ws ~> keyEq <~ ws) ~> stringLit) ~> ((ws ~> caseEq <~ ws) ~> stringLit)
      ~ ((ws ~> '>' <~ ws) ~> takeWhile2(_ != '<') ~> (ws ~> namespaceClose <~ ws))) map {
        _ => ()
      }

      def namespace2B: Parser[Unit] = ((ws ~> namespaceOpen <~ ws) ~>
        ((ws ~> keyEq <~ ws) ~> stringLit) ~> ((ws ~> caseEq <~ ws) ~> stringLit)
      <~ (ws ~> singleClose <~ ws)) map { _ => () }

      def namespaceB = namespace1B | namespace2B

      def namespaces2 = ((ws ~> namespacesOpen <~ ws) ~>
        namespaceB.foldLeft[Unit]((), (_, elem) => ())
      <~ (ws ~> namespacesClose <~ ws))

      def siteinfo2: Parser[Unit] = ((ws ~> siteinfoOpen <~ ws) ~>
        sitename2 ~> dbname2 ~> base2 ~> generator2 ~> cs2 ~> namespaces2
      <~ (ws ~> siteinfoClose <~ ws))

      /**
       * Only keep relevant info as per the wiki example on databricks
       *
       *  return new WikipediaPage(
       *    (elem \ "title").text,
       *    (elem \ "redirect" \ "@title").text,
       *    (elem \ "revision" \ "timestamp").text,
       *    (elem \ "revision" \ "contributor" \ "username").text,
       *    (elem \ "revision" \ "text").text.replaceAll("\n", " ")  // Hive doesn't like \n in column strings.
       *  )
       */

      def numAsInt2: Parser[Unit] = digit2Int.foldLeft[Unit]((), (_, d) => ())

      def ns2: Parser[Unit] = (ws ~> nsOpen <~ ws) ~> numAsInt2 <~ (ws ~> nsClose <~ ws)
      def id2: Parser[Unit] = (ws ~> idOpen <~ ws) ~> numAsInt2 <~ (ws ~> idClose <~ ws)
      def restrictions2 =
        (ws ~> restrictionsOpen <~ ws) ~> takeWhile2(_ != '<') ~> (ws ~> restrictionsClose <~ ws)

      def parentid2: Parser[Unit] =
        (ws ~> parentIdOpen <~ ws) ~> numAsInt2 <~ (ws ~> parentIdClose <~ ws)

      def contributor2: Parser[String] = (
        (ws ~> contributorOpen <~ ws) ~> username <~ id2 <~ (ws ~> contributorClose <~ ws)
      ) map (_.mkString)

      def comment2 =
        (ws ~> commentOpen <~ ws) ~> takeWhile2(_ != '<') ~> (ws ~> commentClose <~ ws)

      def model2 =
        (ws ~> modelOpen <~ ws) ~> takeWhile2(_ != '<') ~> (ws ~> modelClose <~ ws)

      def format2 =
        (ws ~> formatOpen <~ ws) ~> takeWhile2(_ != '<') ~> (ws ~> formatClose <~ ws)

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
      def timestamp: Parser[TimeStamp] = ((ws ~> timestampOpen <~ ws) ~>
          year ~ ('-' ~> nn) ~ ('-' ~> nn) ~
          ('T' ~> nn) ~ (':' ~> nn) ~ (':' ~> nn) <~ 'Z'
      <~ (ws ~> timestampClose <~ ws)) map { case (((((yr, mnth), day), hh), mm), ss) =>
        TimeStamp(yr, mnth, day, hh, mm, ss)
      }

      /**
       * @TODO we could explore the xml:space tag more if
       * required. Check the specification.
       */
      def text: Parser[String] = ((ws ~> textOpen <~ ws) ~>
        takeWhile(_ != '<')
      <~ (ws ~> textClose <~ ws)) map (_.mkString)

      def username: Parser[String] = (
        (ws ~> usernameOpen <~ ws) ~> takeWhile(_ != '<') <~ (ws ~> usernameClose <~ ws)
      ) map (_.mkString)

      /**
       * @TODO seems that the sha1 in this doc is represented in base 36,
       * didn't find enough info atm regarding this. So leaving it as a
       * Parser[String]
       */
      def sha12 =
        (ws ~> shaOpen <~ ws) ~> takeWhile2(_ != '<') <~ (ws ~> shaClose <~ ws)

      def revision2: Parser[((TimeStamp, String), String)] = ((ws ~> revisionOpen <~ ws) ~>
        (id2 ~> parentid2 ~> timestamp) ~
        (contributor2 <~ minor <~ opt(comment2) <~ model2 <~ format2) ~
        (text <~ sha12)
      <~ (ws ~> revisionClose <~ ws))

      def relevantInfo: Parser[RelevantInfo] = ((ws ~> pageOpen <~ ws) ~>
        (title <~ ns2 <~ id2 <~ opt(restrictions2)) ~ opt(redirect) ~ revision2
      <~ (ws ~> pageClose <~ ws)) map {
        case ((t, red), ((ts, usr), txt)) => RelevantInfo(t, red, ts, usr, txt)
      }

      def mediawikiOpen: Parser[Unit] = ((ws ~> mediawikiOpen <~ ws) ~>
        (ws ~> xmlns <~ ws) ~> (ws ~> stringLit <~ ws) ~>
        (ws ~> xmlnsxsi <~ ws) ~> (ws ~> stringLit <~ ws) ~>
        (ws ~> schemaLocation <~ ws) ~> (ws ~> stringLit <~ ws) ~>
        (ws ~> version <~ ws) ~> (ws ~> stringLit <~ ws) ~>
        (ws ~> lang <~ ws) ~> (ws ~> stringLit <~ ws)
      ~> (ws ~> '>' <~ ws)) map { _ => () }

      def relevantInfos2: Parser[List[RelevantInfo]] = (mediawikiOpen ~>
        siteinfo2 ~> rep(relevantInfo)
      <~ (ws ~> mediawikiClose <~ ws))

    }
  }*/
}
