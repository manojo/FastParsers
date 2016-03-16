package parsers

import fastparsers.input.InputWindow
import org.scalameter.api._
import fastparsers.parsers.Parser
import fastparsers.framework.implementations.FastParsersCharArray._

import InputWindow._

object KVParsers {

  /**
    * A pair string parser. Parses stuff of the form
    * [{"..." : "...", "...": "..."}]
    * maps stuff into strings early
    */
  object KVMapEarly {
    lazy val parser = FastParsersCharArray {
      def ws = whitespaces
      def stringPair =
        ((stringLit map (_.toString)) <~ (ws ~> ':' <~ ws)) ~ (stringLit map (_.toString))
      def stringPairs =
        (ws ~> '{' <~ ws) ~> repsep(stringPair, ws ~> ',' <~ ws) <~ (ws ~> '}' <~ ws)
      def main = '[' ~> repsep(stringPairs, ws ~> ',' <~ ws) <~ ']'
    }
  }

  /**
    * A pair string parser. Parses stuff of the form
    * [{"..." : "...", "...": "..."}]
    * maps at pair creation time
    */
  object KVMapAtPairTime {
    lazy val parser = FastParsersCharArray {
      def ws = whitespaces
      def stringPair = ((stringLit <~ (ws ~> ':' <~ ws)) ~ stringLit) map { x =>
        (x._1.toString, x._2.toString)
      }

      def stringPairs =
        (ws ~> '{' <~ ws) ~> repsep(stringPair, ws ~> ',' <~ ws) <~ (ws ~> '}' <~ ws)
      def main = '[' ~> repsep(stringPairs, ws ~> ',' <~ ws) <~ ']'
    }
  }

  /**
    * A pair string parser. Parses stuff of the form
    * [{"..." : "...", "...": "..."}]
    * maps at end of inner list
    */
  object KVMapAtInnerListTime {
    lazy val parser = FastParsersCharArray {
      def ws = whitespaces
      def stringPair = (stringLit <~ (ws ~> ':' <~ ws)) ~ stringLit
      def stringPairs = ((ws ~> '{' <~ ws) ~>
        repsep(stringPair, ws ~> ',' <~ ws) <~
        (ws ~> '}' <~ ws)) map { ls => ls map { x =>
        (x._1.toString, x._2.toString)
      }
      }
      def main = '[' ~> repsep(stringPairs, ws ~> ',' <~ ws) <~ ']'
    }
  }

  /**
    * A pair string parser. Parses stuff of the form
    * [{"..." : "...", "...": "..."}]
    * maps at the very end
    */
  object KVMapAtVeryEnd {
    lazy val parser = FastParsersCharArray {
      def ws = whitespaces
      def stringPair = (stringLit <~ (ws ~> ':' <~ ws)) ~ stringLit
      def stringPairs = ((ws ~> '{' <~ ws) ~>
        repsep(stringPair, ws ~> ',' <~ ws) <~
        (ws ~> '}' <~ ws))
      def main = ('[' ~> repsep(stringPairs, ws ~> ',' <~ ws) <~ ']') map {
        ls =>
          ls map { xs => xs map { x => (x._1.toString, x._2.toString) } }
      }
    }
  }

  /**
    * A pair string parser. Parses stuff of the form
    * [{"..." : "...", "...": "..."}]
    * just recognize
    */
  object KVRecognize {
    lazy val parser = FastParsersCharArray {
      def ws = whitespaces
      def stringPair = (stringLit <~ (ws ~> ':' <~ ws)) ~ stringLit

      def stringPairs = ((ws ~> '{' <~ ws) ~>
        repsep(stringPair, ws ~> ',' <~ ws) <~
        (ws ~> '}' <~ ws))

      def main = ((ws ~> '[' <~ ws) ~> repsep(stringPairs, ws ~> ',' <~ ws)
        <~ (ws ~> ']' <~ ws))
    }
  }

  /**
    * A pair string parser. Parses stuff of the form
    * [{"name" : "...", "lastname": "..."}]
    * just recognize, arrays hoisted out
    */
  object KVSchemaKnownRecognize {
    /**
      * very important to hoist out the literals
      * gains perfs like anything!
      */
    val nameArr = "\"name\"".toCharArray
    val lastnameArr = "\"lastname\"".toCharArray

    lazy val parser = FastParsersCharArray {
      def ws = whitespaces
      def name = (lit(nameArr) <~ (ws ~> ':' <~ ws)) ~ stringLit
      def lastname = (lit(lastnameArr) <~ (ws ~> ':' <~ ws)) ~ stringLit
      def stringPairs = ((ws ~> '{' <~ ws) ~>
        (name ~ (ws ~> ',' <~ ws) ~ lastname) <~
        (ws ~> '}' <~ ws))
      def main = ('[' ~> repsep(stringPairs, ws ~> ',' <~ ws) <~ ']')
    }
  }

  /**
    * A pair string parser. Parses stuff of the form
    * [{"name" : "...", "lastname": "..."}]
    * just recognize, arrays hoisted out
    * use `litRec`, `repSepUnit`, `skipws`
    */
  object KVSchemaKnownRecognizeUnit {
    /**
      * very important to hoist out the literals
      * gains perfs like anything!
      */
    val nameArr = "\"name\"".toCharArray
    val lastnameArr = "\"lastname\"".toCharArray

    lazy val parser = FastParsersCharArray {
      def ws = skipws
      def colon = ws ~> ':' <~ ws
      def name: Parser[Unit] = (litRec(nameArr) <~ colon) <~ stringLitRec
      def lastname: Parser[Unit] = (litRec(lastnameArr) <~ colon) <~ stringLitRec
      def stringPairs: Parser[Unit] = ((ws ~> '{' <~ ws) ~>
        (name ~> (ws ~> ',' <~ ws) ~> lastname) <~
        (ws ~> '}' <~ ws))
      def main: Parser[Unit] = ('[' ~> repSepUnit(stringPairs, ws ~> ',' <~ ws) <~ ']')
    }
  }

  /**
    * A pair string parser. Parses stuff of the form
    * [{"..." : "...", "...": "..."}]
    * recognizes a pair and throws out the key
    */
  object KVRecognizeGetValue {
    lazy val parser = FastParsersCharArray {
      def ws = whitespaces
      def stringPair = (stringLit <~ (ws ~> ':' <~ ws)) ~> stringLit

      def stringPairs = ((ws ~> '{' <~ ws) ~>
        repsep(stringPair, ws ~> ',' <~ ws) <~
        (ws ~> '}' <~ ws))

      def main = ('[' ~> repsep(stringPairs, ws ~> ',' <~ ws) <~ ']')
    }
  }

  object KVRecognizeAndGetValueAtPairTime {
    lazy val parser = FastParsersCharArray {
      def ws = whitespaces
      def stringPair = ((stringLit ~ (ws ~> ':' <~ ws)) ~ stringLit) map (
        x => x._1)

      def stringPairs = ((ws ~> '{' <~ ws) ~>
        repsep(stringPair, ws ~> ',' <~ ws) <~
        (ws ~> '}' <~ ws))

      def main = ('[' ~> repsep(stringPairs, ws ~> ',' <~ ws) <~ ']')
    }
  }

  /**
    * A pair string parser. Parses stuff of the form
    * [{"..." : "...", "...": "..."}]
    * using takeWhile2, which does not inline its function
    * just recognize
    */
  object KVRecognizeTakeWhile2 {
    lazy val parser = FastParsersCharArray {
      def ws = takeWhile2(x => x == ' ' || x == '\n')
      def stringPair = (stringLit <~ (ws ~> ':' <~ ws)) ~ stringLit

      def stringPairs = ((ws ~> '{' <~ ws) ~>
        repsep(stringPair, ws ~> ',' <~ ws) <~
        (ws ~> '}' <~ ws))

      def main = ((ws ~> '[' <~ ws) ~> repsep(stringPairs, ws ~> ',' <~ ws)
        <~ (ws ~> ']' <~ ws))
    }
  }

  /**
    * A pair string parser. Parses stuff of the form
    * [{"..." : "...", "...": "..."}]
    * using takeWhile2, which does not inline its function
    * This time we hoist the closure out. Maybe this triggers
    * inlining on the JVM?
    * just recognize
    */
  object KVRecognizeTakeWhile2Hoisted {
    def isWS(c: Char) = (c == ' ' || c == '\n')

    lazy val parser = FastParsersCharArray {
      def ws = takeWhile2(isWS _)
      def stringPair = (stringLit <~ (ws ~> ':' <~ ws)) ~ stringLit

      def stringPairs = ((ws ~> '{' <~ ws) ~>
        repsep(stringPair, ws ~> ',' <~ ws) <~
        (ws ~> '}' <~ ws))

      def main = ((ws ~> '[' <~ ws) ~> repsep(stringPairs, ws ~> ',' <~ ws)
        <~ (ws ~> ']' <~ ws))
    }
  }

  /**
    * A pair string parser. Parses stuff of the form
    * [{"..." : "...", "...": "..."}]
    * using takeWhile3, which inlines its function
    */
  object KVRecognizeTakeWhile3 {
    lazy val parser = FastParsersCharArray {
      def ws = takeWhile3(x => x == ' ' || x == '\n')
      def stringPair = (stringLit <~ (ws ~> ':' <~ ws)) ~ stringLit

      def stringPairs = ((ws ~> '{' <~ ws) ~>
        repsep(stringPair, ws ~> ',' <~ ws) <~
        (ws ~> '}' <~ ws))

      def main = ((ws ~> '[' <~ ws) ~> repsep(stringPairs, ws ~> ',' <~ ws)
        <~ (ws ~> ']' <~ ws))
    }
  }

  /**
    * A pair string parser. Parses stuff of the form
    * [{"..." : "...", "...": "..."}]
    * just recognize, use WSSKip instead of creating
    * intermediate structs
    */
  object KVRecognizeWSSkip {
    lazy val parser = FastParsersCharArray {
      def ws: Parser[Unit] = skipws
      def stringPair = (stringLit <~ (ws ~> ':' <~ ws)) ~ stringLit

      def stringPairs = ((ws ~> '{' <~ ws) ~>
        repsep(stringPair, ws ~> ',' <~ ws) <~
        (ws ~> '}' <~ ws))

      def main = ((ws ~> '[' <~ ws) ~> repsep(stringPairs, ws ~> ',' <~ ws)
        <~ (ws ~> ']' <~ ws))
    }
  }

  /**
    * A pair string parser. Parses stuff of the form
    * [{"..." : "...", "...": "..."}]
    * just recognize, use WSSKip and stringLitRec
    * instead of creating intermediate structs
    */
  object KVRecognizeRecWSSKip {
    lazy val parser = FastParsersCharArray {
      def ws = skipws
      def strLit = stringLitRec
      def stringPair = (strLit <~ (ws ~> ':' <~ ws)) ~ strLit

      def stringPairs = ((ws ~> '{' <~ ws) ~>
        repsep(stringPair, ws ~> ',' <~ ws) <~
        (ws ~> '}' <~ ws))

      def main = ((ws ~> '[' <~ ws) ~> repsep(stringPairs, ws ~> ',' <~ ws)
        <~ (ws ~> ']' <~ ws))
    }
  }

  /**
    * A pair string parser. Parses stuff of the form
    * [{"..." : "...", "...": "..."}]
    * just recognize, project at end
    */
  object KVRecognizeAndGetValueAtEnd {
    lazy val parser = FastParsersCharArray {
      def ws = whitespaces
      def stringPair = (stringLit ~ (ws ~> ':' <~ ws)) ~ stringLit

      def stringPairs = ((ws ~> '{' <~ ws) ~>
        repsep(stringPair, ws ~> ',' <~ ws) <~
        (ws ~> '}' <~ ws))

      def main = ('[' ~> repsep(stringPairs, ws ~> ',' <~ ws) <~ ']') map {
        ls =>
          ls map (xs => xs map (x => x._2))
      }
    }
  }

  /**
    * A pair string parser. Parses stuff of the form
    * [{"..." : "...", "...": "..."}]
    * just recognize, project and map to string at end
    */
  object KVRecognizeAndGetValueMapAtEnd {
    lazy val parser = FastParsersCharArray {
      def ws = whitespaces
      def stringPair = (stringLit ~ (ws ~> ':' <~ ws)) ~ stringLit

      def stringPairs = ((ws ~> '{' <~ ws) ~>
        repsep(stringPair, ws ~> ',' <~ ws) <~
        (ws ~> '}' <~ ws))

      def main = ('[' ~> repsep(stringPairs, ws ~> ',' <~ ws) <~ ']') map {
        ls =>
          ls map (xs => xs map (x => x._2.toString))
      }
    }
  }

  /**
    * A pair string parser. Parses stuff of the form
    * [{"..." : "...", "...": "..."}]
    * just recognize, and get value, schema known
    */
  object KVRecognizeAndGetValueSchemaKnown {
    /**
      * very important to hoist out the literals
      * gains perfs like anything!
      */
    val nameArr = "\"name\"".toCharArray
    val lastnameArr = "\"lastname\"".toCharArray

    lazy val parser = FastParsersCharArray {
      def ws = whitespaces
      def name = (lit(nameArr) <~ (ws ~> ':' <~ ws)) ~> stringLit
      def lastname = (lit(lastnameArr) <~ (ws ~> ':' <~ ws)) ~> stringLit
      def stringPairs = ((ws ~> '{' <~ ws) ~>
        (name ~ (ws ~> ',' <~ ws) ~ lastname) <~
        (ws ~> '}' <~ ws))
      def main = ('[' ~> repsep(stringPairs, ws ~> ',' <~ ws) <~ ']')
    }
  }

  /**
    * kv recognize, but for info on weeks
    */
  object KVSchemaKnownRecognizeWeeks {
    /**
      * very important to hoist out the literals
      * gains perfs like anything!
      */
    val wArr = "\"w\"".toCharArray
    val aArr = "\"a\"".toCharArray
    val dArr = "\"d\"".toCharArray
    val cArr = "\"c\"".toCharArray

    val weeks = "\"weeks\"".toCharArray

    lazy val parser = FastParsersCharArray {
      def ws = whitespaces
      def w = (lit(wArr) <~ (ws ~> ':' <~ ws)) ~ number
      def a = (lit(aArr) <~ (ws ~> ':' <~ ws)) ~ number
      def d = (lit(dArr) <~ (ws ~> ':' <~ ws)) ~ number
      def c = (lit(cArr) <~ (ws ~> ':' <~ ws)) ~ number

      def weekInfo = ((ws ~> '{' <~ ws) ~>
        (w ~> (ws ~> ',' <~ ws) ~>
          a ~> (ws ~> ',' <~ ws) ~>
          d ~> (ws ~> ',' <~ ws) ~>
          c) <~
        (ws ~> '}' <~ ws))

      def weekInfos =
        (ws ~> '[' <~ ws) ~> repsep(weekInfo, ws ~> ',' <~ ws) <~ (ws ~> ']' <~ ws)

      def manyWeekInfos =
        (ws ~> '[' <~ ws) ~> repsep(weekInfos, ws ~> ',' <~ ws) <~ (ws ~> ']' <~ ws)

      def main = manyWeekInfos
    }
  }

  /**
    * kv recognize, but for info on weeks, make everything a recognizer
    */
  object KVSchemaKnownRecognizeWeeksRec {
    /**
      * very important to hoist out the literals
      * gains perfs like anything!
      */
    val wArr = "\"w\"".toCharArray
    val aArr = "\"a\"".toCharArray
    val dArr = "\"d\"".toCharArray
    val cArr = "\"c\"".toCharArray

    val weeks = "\"weeks\"".toCharArray

    lazy val parser = FastParsersCharArray {
      def ws = skipws
      def w = (lit(wArr) <~ (ws ~> ':' <~ ws)) ~ number
      def a = (lit(aArr) <~ (ws ~> ':' <~ ws)) ~ number
      def d = (lit(dArr) <~ (ws ~> ':' <~ ws)) ~ number
      def c = (lit(cArr) <~ (ws ~> ':' <~ ws)) ~ number

      def weekInfo = ((ws ~> '{' <~ ws) ~>
        (w ~> (ws ~> ',' <~ ws) ~>
          a ~> (ws ~> ',' <~ ws) ~>
          d ~> (ws ~> ',' <~ ws) ~>
          c) <~
        (ws ~> '}' <~ ws))

      def weekInfos =
        (ws ~> '[' <~ ws) ~> repSepUnit(weekInfo, ws ~> ',' <~ ws) <~ (ws ~> ']' <~ ws)

      def manyWeekInfos =
        (ws ~> '[' <~ ws) ~> repSepUnit(weekInfos, ws ~> ',' <~ ws) <~ (ws ~> ']' <~ ws)

      def main = manyWeekInfos
    }
  }

  /**
    * kv recognize, but for info on weeks
    */
  object KVSchemaKnownRecognizeWeeksADT {
    /**
      * very important to hoist out the literals
      * gains perfs like anything!
      */
    val wArr = "\"w\"".toCharArray
    val aArr = "\"a\"".toCharArray
    val dArr = "\"d\"".toCharArray
    val cArr = "\"c\"".toCharArray

    val weeks = "\"weeks\"".toCharArray

    case class WeekInfo(w: String, a: String, d: String, c: String)

    lazy val parser = FastParsersCharArray {
      def ws = whitespaces
      def w = (lit(wArr) <~ (ws ~> ':' <~ ws)) ~> number
      def a = (lit(aArr) <~ (ws ~> ':' <~ ws)) ~> number
      def d = (lit(dArr) <~ (ws ~> ':' <~ ws)) ~> number
      def c = (lit(cArr) <~ (ws ~> ':' <~ ws)) ~> number

      def weekInfo: Parser[WeekInfo] = ((ws ~> '{' <~ ws) ~>
        ((w ~> (ws ~> ',' <~ ws)) ~
          (a ~> (ws ~> ',' <~ ws)) ~
          (d ~> (ws ~> ',' <~ ws)) ~
          c) <~
        (ws ~> '}' <~ ws)) map {
        case (((w, a), d), c) => WeekInfo(w.toString, a.toString, d.toString, c.toString)
      }

      def weekInfos =
        (ws ~> '[' <~ ws) ~> repsep(weekInfo, ws ~> ',' <~ ws) <~ (ws ~> ']' <~ ws)

      def manyWeekInfos =
        (ws ~> '[' <~ ws) ~> repsep(weekInfos, ws ~> ',' <~ ws) <~ (ws ~> ']' <~ ws)

      def main = manyWeekInfos
    }
  }

  trait AuthorInfosParserHelper {

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

  }

  /** Full recognition of data. */
  object KVSchemaKnownRecognizeAuthorInfos extends AuthorInfosParserHelper {

    lazy val parser = FastParsersCharArray {

      def ws = skipws
      def braceOpen = ws ~> '{' ~> ws
      def braceClose = ws ~> '}' ~> ws
      def comma = ws ~> ',' ~> ws
      def colon = ws ~> ':' ~> ws

      def num = number
      def strLit = stringLitRec

      def loginParser = ws ~> litRec(login) ~> ws ~> ':' ~> ws ~> strLit ~> ws
      def avatarUrlP = ws ~> litRec(avatar_url) ~> ws ~> ':' ~> ws ~> strLit ~> ws
      def gravatarIdP = ws ~> litRec(gravatar_id) ~> ws ~> ':' ~> ws ~> strLit ~> ws
      def urlParser = ws ~> litRec(url) ~> ws ~> ':' ~> ws ~> strLit ~> ws
      def htmlUrlP = ws ~> litRec(html_url) ~> ws ~> ':' ~> ws ~> strLit ~> ws
      def followersUrlP = ws ~> litRec(followers_url) ~> ws ~> ':' ~> ws ~> strLit ~> ws
      def followingUrlP = ws ~> litRec(following_url) ~> ws ~> ':' ~> ws ~> strLit ~> ws
      def gistsUrlP = ws ~> litRec(gists_url) ~> ws ~> ':' ~> ws ~> strLit ~> ws
      def starredUrlP = ws ~> litRec(starred_url) ~> ws ~> ':' ~> ws ~> strLit ~> ws
      def subscriptionsUrlP = ws ~> litRec(subscriptions_url) ~> ws ~> ':' ~> ws ~> strLit ~> ws
      def organizationsUrlP = ws ~> litRec(organizations_url) ~> ws ~> ':' ~> ws ~> strLit ~> ws
      def reposUrlP = ws ~> litRec(repos_url) ~> ws ~> ':' ~> ws ~> strLit ~> ws
      def eventsUrlP = ws ~> litRec(events_url) ~> ws ~> ':' ~> ws ~> strLit ~> ws
      def receivedEventsUrlP = ws ~> litRec(received_events_url) ~> ws ~> ':' ~> ws ~> strLit ~> ws
      def authorTypeP = ws ~> litRec(authortype) ~> ws ~> ':' ~> ws ~> strLit ~> ws

      def idParser = //: Parser[String] =
        ws ~> litRec(id) ~> colon ~> num ~> ws

      def siteAdmin: Parser[Unit] =
        ws ~> litRec(site_admin) ~> colon ~> (
          litRec(`true`) | litRec(`false`)
          )

      def authorInfo: Parser[Unit] = (braceOpen ~>
        (loginParser <~ comma) ~>
        (idParser <~ comma) ~>
        (avatarUrlP <~ comma) ~>
        (gravatarIdP <~ comma) ~>
        (urlParser <~ comma) ~>
        (htmlUrlP <~ comma) ~>
        (followersUrlP <~ comma) ~>
        (followingUrlP <~ comma) ~>
        (gistsUrlP <~ comma) ~>
        (starredUrlP <~ comma) ~>
        (subscriptionsUrlP <~ comma) ~>
        (organizationsUrlP <~ comma) ~>
        (reposUrlP <~ comma) ~>
        (eventsUrlP <~ comma) ~>
        (receivedEventsUrlP <~ comma) ~>
        (authorTypeP <~ comma) ~>
        (siteAdmin ~> braceClose))

      def authorInfos = ((ws ~> '[' ~> ws) ~>
        repSepUnit(authorInfo, ws ~> ',' ~> ws) <~
        (ws ~> ']' ~> ws))

      def main = authorInfos
    }
  }

  /** Recognize half of the dataset and parse the other half. */
  object KVSchemaKnownRecognizeAuthorPartial8 extends AuthorInfosParserHelper {

    case class AuthorInfo(login: String, id: String, avatar: String,
                          gravatar: String, url: String, htmlUrl: String,
                          followersUrl: String, followingUrl: String)

    lazy val parser = FastParsersCharArray {

      def ws = skipws
      def braceOpen = ws ~> '{' ~> ws
      def braceClose = ws ~> '}' ~> ws
      def comma = ws ~> ',' ~> ws
      def colon = ws ~> ':' ~> ws

      def num = number
      def strLitRec = stringLitRec

      /* These are parsers and therefore Parser[String] */
      def loginParser = ws ~> litRec(login) ~> ws ~> ':' ~> ws ~> stringLit.map(_.toString) <~ ws
      def avatarUrlP = ws ~> litRec(avatar_url) ~> ws ~> ':' ~> ws ~> stringLit.map(_.toString) <~ ws
      def gravatarUrlP = ws ~> litRec(gravatar_id) ~> ws ~> ':' ~> ws ~> stringLit.map(_.toString) <~ ws
      def urlP = ws ~> litRec(url) ~> ws ~> ':' ~> ws ~> stringLit.map(_.toString) <~ ws
      def htmlUrlP = ws ~> litRec(html_url) ~> ws ~> ':' ~> ws ~> stringLit.map(_.toString) <~ ws
      def followerUrlP = ws ~> litRec(followers_url) ~> ws ~> ':' ~> ws ~> stringLit.map(_.toString) <~ ws
      def followingUrlP = ws ~> litRec(following_url) ~> ws ~> ':' ~> ws ~> stringLit.map(_.toString) <~ ws

      /* These are recognisers and therefore only Parser[Unit] since we use `litRec` */
      def gistUrlP = ws ~> litRec(gists_url) ~> ws ~> ':' ~> ws ~> strLitRec ~> ws
      def starredUrlP = ws ~> litRec(starred_url) ~> ws ~> ':' ~> ws ~> strLitRec ~> ws
      def subscriptionsUrlP = ws ~> litRec(subscriptions_url) ~> ws ~> ':' ~> ws ~> strLitRec ~> ws
      def organizationsUrlP = ws ~> litRec(organizations_url) ~> ws ~> ':' ~> ws ~> strLitRec ~> ws
      def reposUrlP = ws ~> litRec(repos_url) ~> ws ~> ':' ~> ws ~> strLitRec ~> ws
      def eventsUrlP = ws ~> litRec(events_url) ~> ws ~> ':' ~> ws ~> strLitRec ~> ws
      def receivedEventsUrlP = ws ~> litRec(received_events_url) ~> ws ~> ':' ~> ws ~> strLitRec ~> ws
      def authorTypeP = ws ~> litRec(authortype) ~> ws ~> ':' ~> ws ~> strLitRec ~> ws

      def idParser: Parser[String] =
        ws ~> litRec(id) ~> colon ~> num.map(_.toString) <~ ws

      def siteAdmin: Parser[Unit] =
        ws ~> litRec(site_admin) ~> colon ~> (
          litRec(`true`) | litRec(`false`)
          )

      def authorInfo: Parser[AuthorInfo] = (braceOpen ~>
        (loginParser <~ comma) ~
        (idParser <~ comma) ~
        (avatarUrlP <~ comma) ~
        (gravatarUrlP <~ comma) ~
        (urlP <~ comma) ~
        (htmlUrlP <~ comma) ~
        (followerUrlP <~ comma) ~
        (followingUrlP <~ comma) <~
        (gistUrlP <~ comma) <~
        (starredUrlP <~ comma) <~
        (subscriptionsUrlP <~ comma) <~
        (organizationsUrlP <~ comma) <~
        (reposUrlP <~ comma) <~
        (eventsUrlP <~ comma) <~
        (receivedEventsUrlP <~ comma) <~
        (authorTypeP <~ comma) <~
        siteAdmin <~ braceClose
        ).map {
        case (((((((a, b), c), d), e), f), g), h) =>
          AuthorInfo(a, b, c, d, e, f, g, h)
      }

      def authorInfos = ((ws ~> '[' ~> ws) ~>
        repSepUnit(authorInfo, ws ~> ',' ~> ws) <~
        (ws ~> ']' ~> ws))

      def main = authorInfos
    }
  }

  /** Parse 4 out of 17 tuples of the dataset and recognise the rest. */
  object KVSchemaKnownRecognizeAuthorPartial4 extends AuthorInfosParserHelper {

    case class AuthorInfo(login: String, id: String, avatar: String,
                          gravatar: String)

    lazy val parser = FastParsersCharArray {

      def ws = skipws
      def braceOpen = ws ~> '{' ~> ws
      def braceClose = ws ~> '}' ~> ws
      def comma = ws ~> ',' ~> ws
      def colon = ws ~> ':' ~> ws

      def num = number
      def strLitRec = stringLitRec

      /* These are parsers and therefore Parser[String] */
      def idParser: Parser[String] = ws ~> litRec(id) ~> colon ~> num.map(_.toString) <~ ws
      def loginParser = ws ~> litRec(login) ~> ws ~> ':' ~> ws ~> stringLit.map(_.toString) <~ ws
      def avatarUrlP = ws ~> litRec(avatar_url) ~> ws ~> ':' ~> ws ~> stringLit.map(_.toString) <~ ws
      def gravatarUrlP = ws ~> litRec(gravatar_id) ~> ws ~> ':' ~> ws ~> stringLit.map(_.toString) <~ ws

      /* These are recognisers and therefore only Parser[Unit] since we use `litRec` */
      def urlP = ws ~> litRec(url) ~> ws ~> ':' ~> ws ~> strLitRec <~ ws
      def htmlUrlP = ws ~> litRec(html_url) ~> ws ~> ':' ~> ws ~> strLitRec <~ ws
      def followerUrlP = ws ~> litRec(followers_url) ~> ws ~> ':' ~> ws ~> strLitRec <~ ws
      def followingUrlP = ws ~> litRec(following_url) ~> ws ~> ':' ~> ws ~> strLitRec <~ ws
      def gistUrlP = ws ~> litRec(gists_url) ~> ws ~> ':' ~> ws ~> strLitRec ~> ws
      def starredUrlP = ws ~> litRec(starred_url) ~> ws ~> ':' ~> ws ~> strLitRec ~> ws
      def subscriptionsUrlP = ws ~> litRec(subscriptions_url) ~> ws ~> ':' ~> ws ~> strLitRec ~> ws
      def organizationsUrlP = ws ~> litRec(organizations_url) ~> ws ~> ':' ~> ws ~> strLitRec ~> ws
      def reposUrlP = ws ~> litRec(repos_url) ~> ws ~> ':' ~> ws ~> strLitRec ~> ws
      def eventsUrlP = ws ~> litRec(events_url) ~> ws ~> ':' ~> ws ~> strLitRec ~> ws
      def receivedEventsUrlP = ws ~> litRec(received_events_url) ~> ws ~> ':' ~> ws ~> strLitRec ~> ws
      def authorTypeP = ws ~> litRec(authortype) ~> ws ~> ':' ~> ws ~> strLitRec ~> ws

      def siteAdmin: Parser[Unit] =
        ws ~> litRec(site_admin) ~> colon ~> (litRec(`true`) | litRec(`false`))

      def authorInfo: Parser[AuthorInfo] = (braceOpen ~>
        (loginParser <~ comma) ~
        (idParser <~ comma) ~
        (avatarUrlP <~ comma) ~
        (gravatarUrlP <~ comma) <~
        (urlP <~ comma) <~
        (htmlUrlP <~ comma) <~
        (followerUrlP <~ comma) <~
        (followingUrlP <~ comma) <~
        (gistUrlP <~ comma) <~
        (starredUrlP <~ comma) <~
        (subscriptionsUrlP <~ comma) <~
        (organizationsUrlP <~ comma) <~
        (reposUrlP <~ comma) <~
        (eventsUrlP <~ comma) <~
        (receivedEventsUrlP <~ comma) <~
        (authorTypeP <~ comma) <~
        siteAdmin <~ braceClose
        ).map {
        case (((a, b), c), d) => AuthorInfo(a, b, c, d)
      }

      def authorInfos = ((ws ~> '[' ~> ws) ~>
        repSepUnit(authorInfo, ws ~> ',' ~> ws) <~
        (ws ~> ']' ~> ws))

      def main = authorInfos
    }
  }

  /** Parse 12 out of 17 tuples of the dataset and recognise the rest. */
  object KVSchemaKnownRecognizeAuthorPartial12 extends AuthorInfosParserHelper {

    case class AuthorInfo(login: String, id: String, avatar: String,
                          gravatar: String, url: String, htmlUrl: String,
                          followersUrl: String, followingUrl: String,
                          gistUrl: String, starredUrl: String, subsUrl: String,
                          orgsUrl: String)

    lazy val parser = FastParsersCharArray {

      def ws = skipws
      def braceOpen = ws ~> '{' ~> ws
      def braceClose = ws ~> '}' ~> ws
      def comma = ws ~> ',' ~> ws
      def colon = ws ~> ':' ~> ws

      def num = number
      def strLitRec = stringLitRec

      /* These are parsers and therefore Parser[String] */
      def loginParser = ws ~> litRec(login) ~> ws ~> ':' ~> ws ~> stringLit.map(_.toString) <~ ws
      def avatarUrlP = ws ~> litRec(avatar_url) ~> ws ~> ':' ~> ws ~> stringLit.map(_.toString) <~ ws
      def gravatarUrlP = ws ~> litRec(gravatar_id) ~> ws ~> ':' ~> ws ~> stringLit.map(_.toString) <~ ws
      def urlP = ws ~> litRec(url) ~> ws ~> ':' ~> ws ~> stringLit.map(_.toString) <~ ws
      def htmlUrlP = ws ~> litRec(html_url) ~> ws ~> ':' ~> ws ~> stringLit.map(_.toString) <~ ws
      def followerUrlP = ws ~> litRec(followers_url) ~> ws ~> ':' ~> ws ~> stringLit.map(_.toString) <~ ws
      def followingUrlP = ws ~> litRec(following_url) ~> ws ~> ':' ~> ws ~> stringLit.map(_.toString) <~ ws
      def gistUrlP = ws ~> litRec(gists_url) ~> ws ~> ':' ~> ws ~> stringLit.map(_.toString) <~ ws
      def starredUrlP = ws ~> litRec(starred_url) ~> ws ~> ':' ~> ws ~> stringLit.map(_.toString) <~ ws
      def subscriptionsUrlP = ws ~> litRec(subscriptions_url) ~> ws ~> ':' ~> ws ~> stringLit.map(_.toString) <~ ws
      def organizationsUrlP = ws ~> litRec(organizations_url) ~> ws ~> ':' ~> ws ~> stringLit.map(_.toString) <~ ws

      /* These are recognisers and therefore only Parser[Unit] since we use `litRec` */
      def reposUrlP = ws ~> litRec(repos_url) ~> ws ~> ':' ~> ws ~> strLitRec ~> ws
      def eventsUrlP = ws ~> litRec(events_url) ~> ws ~> ':' ~> ws ~> strLitRec ~> ws
      def receivedEventsUrlP = ws ~> litRec(received_events_url) ~> ws ~> ':' ~> ws ~> strLitRec ~> ws
      def authorTypeP = ws ~> litRec(authortype) ~> ws ~> ':' ~> ws ~> strLitRec ~> ws

      def idParser: Parser[String] =
        ws ~> litRec(id) ~> colon ~> num.map(_.toString) <~ ws

      def siteAdmin: Parser[Unit] =
        ws ~> litRec(site_admin) ~> colon ~> (litRec(`true`) | litRec(`false`))

      def authorInfo: Parser[AuthorInfo] = (braceOpen ~>
        (loginParser <~ comma) ~
        (idParser <~ comma) ~
        (avatarUrlP <~ comma) ~
        (gravatarUrlP <~ comma) ~
        (urlP <~ comma) ~
        (htmlUrlP <~ comma) ~
        (followerUrlP <~ comma) ~
        (followingUrlP <~ comma) ~
        (gistUrlP <~ comma) ~
        (starredUrlP <~ comma) ~
        (subscriptionsUrlP <~ comma) ~
        (organizationsUrlP <~ comma) <~
        (reposUrlP <~ comma) <~
        (eventsUrlP <~ comma) <~
        (receivedEventsUrlP <~ comma) <~
        (authorTypeP <~ comma) <~
        siteAdmin <~ braceClose
      ).map {
        case (((((((((((a, b), c), d), e), f), g), h), i), j), k), l) =>
          AuthorInfo(a, b, c, d, e, f, g, h, i, j, k, l)
      }

      def authorInfos = ((ws ~> '[' ~> ws) ~>
        repSepUnit(authorInfo, ws ~> ',' ~> ws) <~
        (ws ~> ']' ~> ws))

      def main = authorInfos
    }
  }

}
