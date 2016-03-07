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
    redirect: Option[InputWindow[Array[Char]]],
    rev: Revision
  )

  case class Namespace(
    key: InputWindow[Array[Char]],
    cs: InputWindow[Array[Char]],
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
    redirectTitle: Option[InputWindow[Array[Char]]],
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

  val fullWikiParser = FastParsersCharArray {
    def ws = whitespaces
    def wsWrap(p: Parser[Array[Char]]): Parser[Array[Char]] =  ws ~> p <~ ws

    def digit2Int: Parser[Int] =
      acceptIf(c => c >= '0' && c <= '9') map { c => (c - '0').toInt }

    def numAsInt: Parser[Int] =
      digit2Int.foldLeft[Int](0, (acc, d) => acc * 10 + d)

    def title: Parser[String] = ((ws ~> ("<title>".toCharArray) <~ws) ~>
      takeWhile(_ != '<')
    <~ (ws ~> "</title>".toCharArray <~ ws)) map (_.mkString)

    def ns: Parser[Int] = (ws ~> "<ns>".toCharArray <~ ws) ~> numAsInt <~ (ws ~> "</ns>".toCharArray <~ ws)
    def id: Parser[Int] = (ws ~> "<id>".toCharArray <~ ws) ~> numAsInt <~ (ws ~> "</id>".toCharArray <~ ws)
    def restrictions: Parser[String] = ((ws ~> "<restrictions>".toCharArray <~ ws) ~>
      takeWhile(_ != '<')
    <~ (ws ~> "</restrictions>".toCharArray <~ ws)) map (_.toString)

    def redirect: Parser[InputWindow[Array[Char]]] = ((ws ~> "<redirect".toCharArray <~ ws) ~>
      (ws ~> "title=".toCharArray <~ ws) ~> stringLit
    <~ (ws ~> "/>".toCharArray <~ ws))

    def parentid: Parser[Int] =
      (ws ~> "<parentid>".toCharArray <~ ws) ~> numAsInt <~ (ws ~> "</parentid>".toCharArray <~ ws)

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
    def timestamp: Parser[TimeStamp] = ((ws ~> "<timestamp>".toCharArray <~ ws) ~>
        year ~ ('-' ~> nn) ~ ('-' ~> nn) ~
        ('T' ~> nn) ~ (':' ~> nn) ~ (':' ~> nn) <~ 'Z'
      <~ (ws ~> "</timestamp>".toCharArray <~ ws)) map { case (((((yr, mnth), day), hh), mm), ss) =>
      TimeStamp(yr, mnth, day, hh, mm, ss)
    }

    def username: Parser[String] = ((ws ~> "<username>".toCharArray <~ ws) ~>
      takeWhile(_ != '<') <~ (ws ~> "</username>".toCharArray <~ ws)
    ) map (_.mkString)

    def contributor: Parser[Contributor] = (
      (ws ~> "<contributor>".toCharArray <~ ws) ~> username ~ id <~ (ws ~> "</contributor>".toCharArray <~ ws)
    ) map { (Contributor.apply _).tupled }

    def minor: Parser[Boolean] =  opt((ws ~> "<minor />".toCharArray <~ ws)) map { _.isDefined }

    def comment: Parser[String] = ((ws ~> "<comment>".toCharArray <~ ws) ~>
      takeWhile(_ != '<')
    <~ (ws ~> "</comment>".toCharArray <~ ws)) map (_.mkString)

    def model: Parser[String] = ((ws ~> "<model>".toCharArray <~ ws) ~>
      takeWhile(_ != '<')
    <~ (ws ~> "</model>".toCharArray <~ ws)) map (_.mkString)


    def format: Parser[String] = ((ws ~> "<format>".toCharArray <~ ws) ~>
      takeWhile(_ != '<') <~ (ws ~> "</format>".toCharArray <~ ws)
    ) map (_.mkString)

    /**
     * @TODO we could explore the xml:space tag more if
     * required. Check the specification.
     */
    def text: Parser[String] = (wsWrap("<text xml:space=\"preserve\">".toCharArray) ~>
      takeWhile(_ != '<')
    <~ (ws ~> "</text>".toCharArray <~ ws)) map (_.mkString)

    /**
     * @TODO seems that the sha1 in this doc is represented in base 36,
     * didn't find enough info atm regarding this. So leaving it as a
     * Parser[String]
     */
    def sha1: Parser[String] = ((ws ~> "<sha1>".toCharArray <~ ws) ~>
      takeWhile(_ != '<') <~ (ws ~> "</sha1>".toCharArray <~ ws)
    ) map (_.mkString)

    def revision: Parser[Revision] = ((ws ~> "<revision>".toCharArray <~ ws) ~>
      id ~ parentid ~ timestamp ~
      contributor ~ minor ~ opt(comment) ~ model ~ format ~
      text ~ sha1
    <~ (ws ~> "</revision>".toCharArray <~ ws)) map {
      case (((((((((id, pid), ts), cntr), mnr), cmt), mdl), fmt), txt), sha) =>
        Revision(id, pid, ts, cntr, mnr, cmt, mdl, fmt, txt, sha)
    }

    /**
     * And finally, the page
     */
    def page = ((ws ~> "<page>".toCharArray <~ ws) ~>
      title ~ ns ~ id ~ opt(restrictions) ~ opt(redirect) ~ revision
    <~ (ws ~> "</page>".toCharArray <~ ws)) map {
      case (((((t, ns), id), _), red), rev) => Page(t, ns, id, red, rev)
    }

    def sitename: Parser[String] = ((ws ~> "<sitename>".toCharArray <~ ws) ~>
      takeWhile(_ != '<') <~ (ws ~> "</sitename>".toCharArray <~ ws)
    ) map (_.mkString)

    def dbname: Parser[String] = ((ws ~> "<dbname>".toCharArray <~ ws) ~>
      takeWhile(_ != '<')
    <~ (ws ~> "</dbname>".toCharArray <~ ws)) map (_.mkString)


    def base: Parser[String] = ((ws ~> "<base>".toCharArray <~ ws) ~>
      takeWhile(_ != '<')
    <~ (ws ~> "</base>".toCharArray <~ ws)) map (_.mkString)

    def generator: Parser[String] = ((ws ~> "<generator>".toCharArray <~ ws) ~>
      takeWhile(_ != '<')
    <~ (ws ~> "</generator>".toCharArray <~ ws)) map (_.mkString)

    def cs: Parser[String] = ((ws ~> "<case>".toCharArray <~ ws) ~>
      takeWhile(_ != '<')
    <~ (ws ~> "</case>".toCharArray <~ ws)) map (_.mkString)

    def namespace1: Parser[Namespace] = ((ws ~> "<namespace".toCharArray <~ ws) ~>
      ((ws ~> "key=".toCharArray <~ ws) ~> stringLit) ~ ((ws ~> "case=".toCharArray <~ ws) ~> stringLit)
    ~ ((ws ~> ">".toCharArray <~ ws) ~> takeWhile(_ != '<') <~ (ws ~> "</namespace>".toCharArray <~ ws))) map {
      case ((key, cs), ns) => Namespace(key, cs, ns.mkString)
    }

    def namespace2: Parser[Namespace] = ((ws ~> "<namespace".toCharArray <~ ws) ~>
      ((ws ~> "key=".toCharArray <~ ws) ~> stringLit) ~ ((ws ~> "case=".toCharArray <~ ws) ~> stringLit)
    <~ (ws ~> "/>".toCharArray <~ ws)) map {
      case (key, cs) => Namespace(key, cs, "")
    }

    def namespace = namespace1 | namespace2

    def namespaces: Parser[List[Namespace]] =
      (ws ~> "<namespaces>".toCharArray <~ ws) ~> rep(namespace) <~ (ws ~> "</namespaces>".toCharArray <~ ws)

    def siteinfo: Parser[SiteInfo] = ((ws ~> "<siteinfo>".toCharArray <~ ws) ~>
      sitename ~ dbname ~ base ~ generator ~ cs ~ namespaces
    <~ (ws ~> "</siteinfo>".toCharArray <~ ws)) map { case (((((stn, dbn) ,bs), gen), cs), nss) =>
        SiteInfo(stn, dbn, bs, gen, cs, nss)
    }

    def mediawikiOpen: Parser[Unit] = ((ws ~> "<mediawiki".toCharArray <~ ws) ~>
      (ws ~> "xmlns=".toCharArray <~ ws) ~> (ws ~> stringLit <~ ws) ~>
      (ws ~> "xmlns:xsi=".toCharArray <~ ws) ~> (ws ~> stringLit <~ ws) ~>
      (ws ~> "xsi:schemaLocation=".toCharArray <~ ws) ~> (ws ~> stringLit <~ ws) ~>
      (ws ~> "version=".toCharArray <~ ws) ~> (ws ~> stringLit <~ ws) ~>
      (ws ~> "xml:lang=".toCharArray <~ ws) ~> (ws ~> stringLit <~ ws)
    ~> (ws ~> ">".toCharArray <~ ws)) map { _ => () }

    def fullParser: Parser[(SiteInfo, List[Page])] = (mediawikiOpen ~>
      siteinfo ~ rep(page)
    <~ (ws ~> "</mediawiki>".toCharArray <~ ws))

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

  val relevantWikiParser = FastParsersCharArray {

    def digit2Int: Parser[Int] =
      acceptIf(c => c >= '0' && c <= '9') map { c => (c - '0').toInt }

    def ws = whitespaces
    def wsWrap(p: Parser[Array[Char]]): Parser[Array[Char]] =  ws ~> p <~ ws

    def title: Parser[String] = ((ws ~> "<title>".toCharArray <~ ws) ~>
      takeWhile(_ != '<')
    <~ (ws ~> "</title>".toCharArray <~ ws)) map (_.mkString)

    def redirect: Parser[InputWindow[Array[Char]]] = ((ws ~> "<redirect".toCharArray <~ ws) ~>
      (ws ~> "title=".toCharArray <~ ws) ~> stringLit
    <~ (ws ~> "/>".toCharArray <~ ws))

    def minor: Parser[Boolean] =  opt((ws ~> "<minor />".toCharArray <~ ws)) map { _.isDefined }

    def sitename2 = ((ws ~> "<sitename>".toCharArray <~ ws) ~>
      takeWhile2(_ != '<') <~ (ws ~> "</sitename>".toCharArray <~ ws)
    )

    def dbname2 =
      (ws ~> "<dbname>".toCharArray <~ ws) ~> takeWhile2(_ != '<') ~> (ws ~> "</dbname>".toCharArray <~ ws)

    def base2 =
      (ws ~> "<base>".toCharArray <~ ws) ~> takeWhile2(_ != '<') ~> (ws ~> "</base>".toCharArray <~ ws)

    def generator2 =
      (ws ~> "<generator>".toCharArray <~ ws) ~> takeWhile2(_ != '<') ~> (ws ~> "</generator>".toCharArray <~ ws)

    def cs2 =
      (ws ~> "<case>".toCharArray <~ ws) ~> takeWhile2(_ != '<') ~> (ws ~> "</case>".toCharArray <~ ws)

    def namespace1B: Parser[Unit] = ((ws ~> "<namespace".toCharArray <~ ws) ~>
      ((ws ~> "key=".toCharArray <~ ws) ~> stringLit) ~> ((ws ~> "case=".toCharArray <~ ws) ~> stringLit)
    ~ ((ws ~> ">".toCharArray <~ ws) ~> takeWhile2(_ != '<') ~> (ws ~> "</namespace>".toCharArray <~ ws))) map {
      _ => ()
    }

    def namespace2B: Parser[Unit] = ((ws ~> "<namespace".toCharArray <~ ws) ~>
      ((ws ~> "key=".toCharArray <~ ws) ~> stringLit) ~> ((ws ~> "case=".toCharArray <~ ws) ~> stringLit)
    <~ (ws ~> "/>".toCharArray <~ ws)) map { _ => () }

    def namespaceB = namespace1B | namespace2B

    def namespaces2 = ((ws ~> "<namespaces>".toCharArray <~ ws) ~>
      namespaceB.foldLeft[Unit]((), (_, elem) => ())
    <~ (ws ~> "</namespaces>".toCharArray <~ ws))

    def siteinfo2: Parser[Unit] = ((ws ~> "<siteinfo>".toCharArray <~ ws) ~>
      sitename2 ~> dbname2 ~> base2 ~> generator2 ~> cs2 ~> namespaces2
    <~ (ws ~> "</siteinfo>".toCharArray <~ ws))

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

    def ns2: Parser[Unit] = (ws ~> "<ns>".toCharArray <~ ws) ~> numAsInt2 <~ (ws ~> "</ns>".toCharArray <~ ws)
    def id2: Parser[Unit] = (ws ~> "<id>".toCharArray <~ ws) ~> numAsInt2 <~ (ws ~> "</id>".toCharArray <~ ws)
    def restrictions2 =
      (ws ~> "<restrictions>".toCharArray <~ ws) ~> takeWhile2(_ != '<') ~> (ws ~> "</restrictions>".toCharArray <~ ws)

    def parentid2: Parser[Unit] =
      (ws ~> "<parentid>".toCharArray <~ ws) ~> numAsInt2 <~ (ws ~> "</parentid>".toCharArray <~ ws)

    def contributor2: Parser[String] = (
      (ws ~> "<contributor>".toCharArray <~ ws) ~> username <~ id2 <~ (ws ~> "</contributor>".toCharArray <~ ws)
    ) map (_.mkString)

    def comment2 =
      (ws ~> "<comment>".toCharArray <~ ws) ~> takeWhile2(_ != '<') ~> (ws ~> "</comment>".toCharArray <~ ws)

    def model2 =
      (ws ~> "<model>".toCharArray <~ ws) ~> takeWhile2(_ != '<') ~> (ws ~> "</model>".toCharArray <~ ws)

    def format2 =
      (ws ~> "<format>".toCharArray <~ ws) ~> takeWhile2(_ != '<') ~> (ws ~> "</format>".toCharArray <~ ws)

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
    def timestamp: Parser[TimeStamp] = ((ws ~> "<timestamp>".toCharArray <~ ws) ~>
        year ~ ('-' ~> nn) ~ ('-' ~> nn) ~
        ('T' ~> nn) ~ (':' ~> nn) ~ (':' ~> nn) <~ 'Z'
    <~ (ws ~> "</timestamp>".toCharArray <~ ws)) map { case (((((yr, mnth), day), hh), mm), ss) =>
      TimeStamp(yr, mnth, day, hh, mm, ss)
    }

    /**
     * @TODO we could explore the xml:space tag more if
     * required. Check the specification.
     */
    def text: Parser[String] = (wsWrap("<text xml:space=\"preserve\">".toCharArray) ~>
      takeWhile(_ != '<')
    <~ (ws ~> "</text>".toCharArray <~ ws)) map (_.mkString)

    def username: Parser[String] = (
      (ws ~> "<username>".toCharArray <~ ws) ~> takeWhile(_ != '<') <~ (ws ~> "</username>".toCharArray <~ ws)
    ) map (_.mkString)

    /**
     * @TODO seems that the sha1 in this doc is represented in base 36,
     * didn't find enough info atm regarding this. So leaving it as a
     * Parser[String]
     */
    def sha12 =
      (ws ~> "<sha1>".toCharArray <~ ws) ~> takeWhile2(_ != '<') <~ (ws ~> "</sha1>".toCharArray <~ ws)

    def revision2: Parser[((TimeStamp, String), String)] = ((ws ~> "<revision>".toCharArray <~ ws) ~>
      (id2 ~> parentid2 ~> timestamp) ~
      (contributor2 <~ minor <~ opt(comment2) <~ model2 <~ format2) ~
      (text <~ sha12)
    <~ (ws ~> "</revision>".toCharArray <~ ws))

    def relevantInfo: Parser[RelevantInfo] = ((ws ~> "<page>".toCharArray <~ ws) ~>
      (title <~ ns2 <~ id2 <~ opt(restrictions2)) ~ opt(redirect) ~ revision2
    <~ (ws ~> "</page>".toCharArray <~ ws)) map {
      case ((t, red), ((ts, usr), txt)) => RelevantInfo(t, red, ts, usr, txt)
    }

    def mediawikiOpen: Parser[Unit] = ((ws ~> "<mediawiki".toCharArray <~ ws) ~>
      (ws ~> "xmlns=".toCharArray <~ ws) ~> (ws ~> stringLit <~ ws) ~>
      (ws ~> "xmlns:xsi=".toCharArray <~ ws) ~> (ws ~> stringLit <~ ws) ~>
      (ws ~> "xsi:schemaLocation=".toCharArray <~ ws) ~> (ws ~> stringLit <~ ws) ~>
      (ws ~> "version=".toCharArray <~ ws) ~> (ws ~> stringLit <~ ws) ~>
      (ws ~> "xml:lang=".toCharArray <~ ws) ~> (ws ~> stringLit <~ ws)
    ~> (ws ~> ">".toCharArray <~ ws)) map { _ => () }

    def relevantInfos2: Parser[List[RelevantInfo]] = (mediawikiOpen ~>
      siteinfo2 ~> rep(relevantInfo)
    <~ (ws ~> "</mediawiki>".toCharArray <~ ws))
  }
}
