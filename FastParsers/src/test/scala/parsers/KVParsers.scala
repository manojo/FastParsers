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
      }}
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
      def main = ('[' ~> repsep(stringPairs, ws ~> ',' <~ ws) <~ ']')  map { ls =>
        ls map { xs => xs map { x => (x._1.toString, x._2.toString)} }
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
      def stringPair = ((stringLit ~ (ws ~> ':' <~ ws)) ~ stringLit) map (x => x._1)

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

      def main = ('[' ~> repsep(stringPairs, ws ~> ',' <~ ws) <~ ']') map { ls =>
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

      def main = ('[' ~> repsep(stringPairs, ws ~> ',' <~ ws) <~ ']') map { ls =>
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
}
