package parsers

import fastparsers.input.InputWindow
import org.scalameter.api._
import fastparsers.parsers.Parser
import fastparsers.framework.implementations.FastParsersCharArray._

import InputWindow._

/**
 * parsing the contributions json file
 */
object ContributionsParsers {

  /** Hoisting out all literals to char arrays */
  val total = "\"total\"".toCharArray
  val wArr = "\"w\"".toCharArray
  val aArr = "\"a\"".toCharArray
  val dArr = "\"d\"".toCharArray
  val cArr = "\"c\"".toCharArray

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
  val weeks = "\"weeks\"".toCharArray

  val site_admin = "\"site_admin\"".toCharArray
  val author = "\"author\"".toCharArray


  val `true` = "true".toCharArray
  val `false` = "false".toCharArray

  def isDigit(c: Char) = (c >= '0') && (c <= '9')

  object Contributions {

    case class WeekInfo(w: String, a: String, d: String, c: String)

    case class AuthorInfo(
      login: String,
      id: String,
      avatar_url: String,
      gravatar_id: String,
      url: String,
      html_url: String,
      followers_url: String,
      following_url: String,
      gists_url: String,
      starred_url: String,
      subscriptions_url: String,
      organizations_url: String,
      repos_url: String,
      events_url: String,
      received_events_url: String,
      authortype: String,
      site_admin: Boolean
    )

    case class Contribution(
      total: String,
      weeks: List[WeekInfo],
      authInfo: AuthorInfo)


    lazy val parser = FastParsersCharArray {
      def ws = skipws
      def strLit = stringLit
      //def digit2Int: Parser[Int] = acceptIf(isDigit) map { c =>
      //  (c - '0').toInt
      //}
      def num: Parser[String] = number map (_.toString)
        //digit2Int.foldLeft2[Int](0, (acc, elem) => acc * 10 + elem)

      def squareOpen = ws ~> '[' <~ ws
      def squareClose = ws ~> ']' <~ ws

      def braceOpen = ws ~> '{' <~ ws
      def braceClose = ws ~> '}' <~ ws

      def comma = ws ~> ',' <~ ws
      def colon = ws ~> ':' <~ ws

      /**
       * taking parameters which are not of type
       * `Parser` not supported yet
       */
      def litW(str: Parser[Unit]) =
        ws ~> str ~>colon ~> strLit <~ ws

      def litN(str: Parser[Unit]): Parser[String] =
        ws ~> str ~> colon ~> num <~ ws

      def totalParser: Parser[String] = ws ~> litRec(total) ~> colon ~> num

      def w = (litRec(wArr) <~ (ws ~> ':' <~ ws)) ~> number
      def a = (litRec(aArr) <~ (ws ~> ':' <~ ws)) ~> number
      def d = (litRec(dArr) <~ (ws ~> ':' <~ ws)) ~> number
      def c = (litRec(cArr) <~ (ws ~> ':' <~ ws)) ~> number

      def weekInfo: Parser[WeekInfo] = ((ws ~> '{' <~ ws) ~>
         (w ~ (ws ~> ',' <~ ws) ~
          a ~ (ws ~> ',' <~ ws) ~
          d ~ (ws ~> ',' <~ ws) ~
          c) <~
       (ws ~> '}' <~ ws)) map { case (((w, a), d), c) =>
          WeekInfo(w.toString, a.toString, d.toString, c.toString)
        }

      def weekInfos: Parser[List[WeekInfo]] = (litRec(weeks) ~> colon ~>
        squareOpen ~> repsep(weekInfo, comma) <~ squareClose
      )

      def idParser: Parser[String] = ws ~> litRec(id) ~> colon ~> num <~ ws

      def siteAdmin: Parser[Boolean] =
        ws ~> litRec(site_admin) ~> colon ~> (
          (litRec(`true`) ^^^ true) | (litRec(`false`) ^^^ false)
        )

      def authorInfo: Parser[AuthorInfo] = (ws ~> litRec(author) ~> colon ~> braceOpen ~>
        (litW(litRec(login)) <~ comma) ~
        (idParser <~ comma) ~
        (litW(litRec(avatar_url)) <~ comma) ~
        (litW(litRec(gravatar_id)) <~ comma) ~
        (litW(litRec(url)) <~ comma) ~
        (litW(litRec(html_url)) <~ comma) ~
        (litW(litRec(followers_url)) <~ comma) ~
        (litW(litRec(following_url)) <~ comma) ~
        (litW(litRec(gists_url)) <~ comma) ~
        (litW(litRec(starred_url)) <~ comma) ~
        (litW(litRec(subscriptions_url)) <~ comma) ~
        (litW(litRec(organizations_url)) <~ comma) ~
        (litW(litRec(repos_url)) <~ comma) ~
        (litW(litRec(events_url)) <~ comma) ~
        (litW(litRec(received_events_url)) <~ comma) ~
        (litW(litRec(authortype)) <~ comma) ~
        (siteAdmin <~ braceClose)) map {
        case ((((((((((((((((l, i), av), gr), u), h), fr), fg), g), st), s), o), r), ev), re), at), sa) =>
          AuthorInfo(
            l.toString,
            i,
            a.toString,
            g.toString,
            u.toString,
            h.toString,
            fr.toString,
            fg.toString,
            g.toString,
            st.toString,
            s.toString,
            o.toString,
            r.toString,
            ev.toString,
            re.toString,
            at.toString,
            sa
          )
      }

      def singleContribution: Parser[Contribution] = (braceOpen ~>
        totalParser ~ (comma ~> weekInfos <~ comma) ~ authorInfo
      <~ braceClose) map { case ((t, wi), a) => Contribution(t, wi, a) }

      def contributions: Parser[List[Contribution]] =
        squareOpen ~> repsep(singleContribution, comma) <~ squareClose

      def main = contributions map { ls =>
        for (Contribution(t, _, auth) <- ls) yield (t, auth.id)
      }

    }
  }

  object TotalAuthorOnly {

    lazy val parser = FastParsersCharArray {
      def ws = skipws
      def strLit = stringLit ^^^ (()) //stringLitRec
      def digit: Parser[Char] = acceptIf(isDigit)
      //def digit2Int: Parser[Int] = acceptIf(isDigit) map { c =>
      //  (c - '0').toInt
      //}

      def num = number map (_.toString)//digit2Int.foldLeft2[Int](0, (acc, elem) => acc * 10 + elem)
      def numRec: Parser[Unit] = number ^^^ (())// digit.foldLeft2[Unit]((), (_, elem) => ())

      def squareOpen = ws ~> '[' <~ ws
      def squareClose = ws ~> ']' <~ ws

      def braceOpen = ws ~> '{' <~ ws
      def braceClose = ws ~> '}' <~ ws

      def comma = ws ~> ',' <~ ws
      def colon = ws ~> ':' <~ ws

      /**
       * taking parameters which are not of type
       * `Parser` not supported yet
       */
      def litW(str: Parser[Unit]): Parser[Unit] =
        ws ~> str ~> ws ~> ':' ~> ws ~> strLit ~> ws

      def litN(str2: Parser[Unit]): Parser[Unit] =
        ws ~> str2 ~> ws ~> ':' ~> ws ~> numRec ~> ws

      def totalParser: Parser[String] = ws ~> litRec(total) ~> colon ~> num

      def w = (litRec(wArr) <~ (ws ~> ':' <~ ws)) <~ number
      def a = (litRec(aArr) <~ (ws ~> ':' <~ ws)) <~ number
      def d = (litRec(dArr) <~ (ws ~> ':' <~ ws)) <~ number
      def c = (litRec(cArr) <~ (ws ~> ':' <~ ws)) <~ number

      def weekInfo: Parser[Unit] = ((ws ~> '{' <~ ws) ~>
         (w ~> (ws ~> ',' <~ ws) ~>
          a ~> (ws ~> ',' <~ ws) ~>
          d ~> (ws ~> ',' <~ ws) ~>
          c) <~
       (ws ~> '}' <~ ws))

      def weekInfos: Parser[Unit] = (litRec(weeks) ~> colon ~>
        squareOpen ~> repSepUnit(weekInfo, comma) <~ squareClose
      )

      def idParser: Parser[String] =
        ws ~> litRec(id) ~> colon ~> num <~ ws

      def siteAdmin: Parser[Unit] =
        ws ~> litRec(site_admin) ~> colon ~> (
          litRec(`true`) | litRec(`false`)
        )

      def authorInfo: Parser[String] = (litRec(author) ~> colon ~> braceOpen ~>
        (litW(litRec(login)) <~ comma) ~>
        (idParser <~ comma) <~ (
        (litW(litRec(avatar_url)) <~ comma) ~>
        (litW(litRec(gravatar_id)) <~ comma) ~>
        (litW(litRec(url)) <~ comma) ~>
        (litW(litRec(html_url)) <~ comma) ~>
        (litW(litRec(followers_url)) <~ comma) ~>
        (litW(litRec(following_url)) <~ comma) ~>
        (litW(litRec(gists_url)) <~ comma) ~>
        (litW(litRec(starred_url)) <~ comma) ~>
        (litW(litRec(subscriptions_url)) <~ comma) ~>
        (litW(litRec(organizations_url)) <~ comma) ~>
        (litW(litRec(repos_url)) <~ comma) ~>
        (litW(litRec(events_url)) <~ comma) ~>
        (litW(litRec(received_events_url)) <~ comma) ~>
        (litW(litRec(authortype)) <~ comma) ~>
        (siteAdmin ~> braceClose)))

      def singleContribution: Parser[(String, String)] = (braceOpen ~>
        (totalParser <~ (comma ~> weekInfos <~ comma)) ~ authorInfo
      <~ braceClose)

      def contributions: Parser[List[(String, String)]] =
        squareOpen ~> repsep(singleContribution, comma) <~ squareClose

      def main = contributions
    }
  }

  object TotalAuthorOnlySkipWeeks {


    lazy val parser = FastParsersCharArray {
      def ws = skipws
      def strLit = stringLitRec
      def digit: Parser[Char] = acceptIf(isDigit)
      //def digit2Int: Parser[Int] = acceptIf(isDigit) map { c =>
      //  (c - '0').toInt
      //}

      def num = number map (_.toString)//digit2Int.foldLeft2[Int](0, (acc, elem) => acc * 10 + elem)
      def numRec: Parser[Unit] = number ^^^ (())// digit.foldLeft2[Unit]((), (_, elem) => ())

      def squareOpen = ws ~> '[' <~ ws
      def squareClose = ws ~> ']' <~ ws

      def braceOpen = ws ~> '{' <~ ws
      def braceClose = ws ~> '}' <~ ws

      def comma = ws ~> ',' <~ ws
      def colon = ws ~> ':' <~ ws

      /**
       * taking parameters which are not of type
       * `Parser` not supported yet
       */
      def litW(str: Parser[Unit]): Parser[Unit] =
        ws ~> str ~> ws ~> ':' ~> ws ~> strLit ~> ws

      def litN(str2: Parser[Unit]): Parser[Unit] =
        ws ~> str2 ~> ws ~> ':' ~> ws ~> numRec ~> ws

      def totalParser: Parser[String] = ws ~> litRec(total) ~> colon ~> num


      /**
       * simple repsep instead of a proper schema match
       */
      def weekInfo: Parser[Unit] = (braceOpen ~>
        repSepUnit(strLit ~> colon ~> numRec, comma) <~ braceClose)

      def weekInfos: Parser[Unit] = (litRec(weeks) ~> colon ~>
        squareOpen ~> repSepUnit(weekInfo, comma) <~ squareClose
      )

      def idParser: Parser[String] =
        ws ~> litRec(id) ~> colon ~> num <~ ws

      def siteAdmin: Parser[Unit] =
        ws ~> litRec(site_admin) ~> colon ~> (
          litRec(`true`) | litRec(`false`)
        )

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

      def authorInfo: Parser[String] = (litRec(author) ~> colon ~> braceOpen ~>
        (loginParser <~ comma) ~>
        (idParser <~ comma) <~ (
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
        (siteAdmin ~> braceClose)))

      def singleContribution: Parser[(String, String)] = (braceOpen ~>
        (totalParser <~ (comma ~> weekInfos <~ comma)) ~ authorInfo
      <~ braceClose)

      def contributions: Parser[List[(String, String)]] =
        squareOpen ~> repsep(singleContribution, comma) <~ squareClose

      def main = contributions
    }
  }

  /**
   * seems to be a break point in number of tuples at which point
   * things crash in terms of performance
   */
  object TotalAuthorOnlySkipWeeks2 {
    lazy val parser = FastParsersCharArray {
      def ws = skipws
      def strLit = stringLitRec
      def digit: Parser[Char] = acceptIf(isDigit)
      //def digit2Int: Parser[Int] = acceptIf(isDigit) map { c =>
      //  (c - '0').toInt
      //}
      def num = number map (_.toString)//digit2Int.foldLeft2[Int](0, (acc, elem) => acc * 10 + elem)
      def numRec: Parser[Unit] = number ^^^ (())// digit.foldLeft2[Unit]((), (_, elem) => ())
      def squareOpen = ws ~> '[' <~ ws
      def squareClose = ws ~> ']' <~ ws
      def braceOpen = ws ~> '{' <~ ws
      def braceClose = ws ~> '}' <~ ws
      def comma = ws ~> ',' <~ ws
      def colon = ws ~> ':' <~ ws
      /**
       * taking parameters which are not of type
       * `Parser` not supported yet
       */
      def litW(str: Parser[Unit]): Parser[Unit] =
        ws ~> str ~> ws ~> ':' ~> ws ~> strLit ~> ws
      def litN(str2: Parser[Unit]): Parser[Unit] =
        ws ~> str2 ~> ws ~> ':' ~> ws ~> numRec ~> ws
      def totalParser: Parser[String] = ws ~> litRec(total) ~> colon ~> num

      /**
       * simple repsep
       */
      def weekInfo: Parser[Unit] = (braceOpen ~>
        repSepUnit(strLit ~> colon ~> numRec, comma) <~ braceClose)
      def weekInfos: Parser[Unit] = (litRec(weeks) ~> colon ~>
        squareOpen ~> repSepUnit(weekInfo, comma) <~ squareClose
      )
      def idParser: Parser[String] =
        ws ~> litRec(id) ~> colon ~> num <~ ws

      /**
       * we only take what we need with the author too. The combo
       * of this and the above rep give us a lot of perf!!
       */
      def authorInfo: Parser[String] = (litRec(author) ~> colon ~> braceOpen ~>
        ((ws ~> litRec(login) ~> ws ~> ':' ~> ws ~> strLit ~> ws) <~ comma) ~>
        (idParser <~ comma) <~ (
          repSepUnit(strLit ~> colon ~> (strLit | litRec(`true`) | litRec(`false`)),
            comma)
        ) ~> braceClose)

      def singleContribution: Parser[(String, String)] = (braceOpen ~>
        (totalParser <~ (comma ~> weekInfos <~ comma)) ~ authorInfo
      <~ braceClose)

      def contributions: Parser[List[(String, String)]] =
        squareOpen ~> repsep(singleContribution, comma) <~ squareClose

      def main = contributions
    }
  }
}
