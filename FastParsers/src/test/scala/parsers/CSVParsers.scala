package parsers

import fastparsers.input.InputWindow._
import scala.util.parsing.combinator._
import scala.util.parsing.input._
import scala.annotation.compileTimeOnly

/**
 * Created by Eric on 05.04.14.
 */
object CSVParsers {

  sealed abstract class JSValue
  case class JSArray(arr: List[JSValue]) extends JSValue
  case class JSDouble(d: Double) extends JSValue
  case class JSDouble2(d: InputWindow[Array[Char]]) extends JSValue
  case class JSString(d: InputWindow[Array[Char]]) extends JSValue
  case class JSString2(d: String) extends JSValue
  case class JSBool(b: Boolean) extends JSValue
  object JTrue extends JSValue
  object JFalse extends JSValue


  val trueValue = "true".toCharArray
  val falseValue = "false".toCharArray
  val comma = ",".toCharArray
  val close = "]".toCharArray

  /*val x = new {
  	@compileTimeOnly("dsad")
  	def y = 2
  }*/

  /**
   * The `FastParsersCharArray` trait already handles keeping
   * string literals and other parsed values in structs, so
   * a recognizer for such a thing is really not going to add any performance
   */
  object CSVParseAll {
   	import fastparsers.framework.implementations.FastParsersCharArray._
   	import fastparsers.parsers.Parser
		val csvParser = FastParsersCharArray {
      def ws = whitespaces
			def csv(p: Parser[JSValue]) = '[' ~> repsep(p, ws ~> ',' <~ ws) <~ close ^^ JSArray
			def doubles = csv(decimalNumber ^^ (x => JSDouble(x.toString.toDouble)))
			def bools = csv((lit(trueValue) ~> success(JTrue)) | (lit(falseValue) ~> success(JTrue)))

      /**
       * stringLit creates a struct with start and end, we're pulling out
       * string here, since that corresponds to parsing
       */
      def strings = csv(stringLit ^^ { xs => JSString2(xs.toString) })

      /**
       * key-value pairs
       */

      /* Mapping as early as possible */
      def stringPair1 =
        ((stringLit map (_.toString)) <~ (ws ~> ':' <~ ws)) ~ (stringLit map (_.toString))
      def stringPairs1 =
        (ws ~> '{' <~ ws) ~> repsep(stringPair1, ws ~> ',' <~ ws) <~ (ws ~> '}' <~ ws)
      def strAll1 = '[' ~> repsep(stringPairs1, ws ~> ',' <~ ws) <~ close

      /* Mapping at pair creation time */
      def stringPair2 = (stringLit ~ ((ws ~> ':' <~ ws) ~> stringLit)) map { case (a, b) =>
        (a.toString, b.toString)
      }
      def stringPairs2 =
        (ws ~> '{' <~ ws) ~> repsep(stringPair2, ws ~> ',' <~ ws) <~ (ws ~> '}' <~ ws)
      def strAll2 = '[' ~> repsep(stringPairs2, ws ~> ',' <~ ws) <~ close

      /* Map to string at very end */
      def stringPair3 = stringLit ~ ((ws ~> ':' <~ ws) ~> stringLit)
      def stringPairs3 =
        ((ws ~> '{' <~ ws) ~> repsep(stringPair3, ws ~> ',' <~ ws) <~ (ws ~> '}' <~ ws)) map { ls =>
          ls.map { case (a, b) => (a.toString, b.toString) }
        }
      def strAll3 = '[' ~> repsep(stringPairs3, ws ~> ',' <~ ws) <~ close

      /* Project and map at very end */
      def stringPair4 = stringLit ~ ((ws ~> ':' <~ ws) ~> stringLit)
      def stringPairs4 =
        ((ws ~> '{' <~ ws) ~> repsep(stringPair4, ws ~> ',' <~ ws) <~ (ws ~> '}' <~ ws))

      def strAll4 = ('[' ~> repsep(stringPairs4, ws ~> ',' <~ ws) <~ close) map { ls =>
        ls map { ks => ks map { x => (x._1.toString, x._2.toString) } }
      }

      /* Project and map at very end */
      def stringPair5 = stringLit ~ ((ws ~> ':' <~ ws) ~> stringLit)
      def stringPairs5 =
        ((ws ~> '{' <~ ws) ~> repsep(stringPair5, ws ~> ',' <~ ws) <~ (ws ~> '}' <~ ws))

      def strAll5 = ('[' ~> repsep(stringPairs5, ws ~> ',' <~ ws) <~ close) map { ls =>
        ls map { ks => ks map (_._1.toString) }
      }
		}
	}

  object CSVRecognizers {
  	import fastparsers.framework.implementations.FastParsersCharArray._
    import fastparsers.parsers.Parser
		/*fastparsers.framework.implementations.FastPrinters.FastParsers*/
    val csvParser = FastParsersCharArray {

      def ws = whitespaces

      /**
       * takeWhile and fold, both do not inline their functions,
       * hence seem to suffer a performance hit.
       */
      def recws = takeWhile2(x => x == ' ' || x == '\n')
        //acceptIf(x => x == ' ' || x == '\n').foldLeft[Unit]((), (acc, elem) => ())

      def recstrLit = ('"' ~>
        (acceptIf(x => x != '"').foldLeft[Unit]((), (acc, elem) => ()))
      <~ '"')

			def primBools = ('t' ~ 'r' ~ 'u' ~ 'e') | ('f' ~ 'a' ~ 'l' ~'s' ~ 'e')
			def bools = '[' ~> repsep(primBools, ws ~> ',' <~ ws) <~ close
      def strings = '[' ~> repsep(stringLit, ws ~> ',' <~ ws) <~ close

      /** seems like `recstrLit` is much worse than the primitive stringLit */
      //def strings = '[' ~> repsep(recstrLit, recws ~> ',' <~ recws) <~ close

      /**
       * parsing structs, and then mapping at the end
       */
      def stringsParsed =
        ('[' ~> repsep(stringLit, ws ~> ',' <~ ws) <~ close) map {
          ls => ls map (_.toString)
        }

      /**
       * key-value pairs, ignore both
       */
      def stringPair1 = stringLit ~> (ws ~> ':' <~ ws) ~> stringLit
      def stringPairs1 = (ws ~> '{' <~ ws) ~> repsep(stringPair1, ws ~> ',' <~ ws) <~ (ws ~> '}' <~ ws)
      def strAll1 = '[' ~> repsep(stringPairs1, ws ~> ',' <~ ws) <~ close

      /**
       * key-value pairs, ignore right, map now
       */
      def stringPair2 =
        (stringLit map (_.toString)) <~ ((ws ~> ':' <~ ws) <~ stringLit)
      def stringPairs2 = (ws ~> '{' <~ ws) ~> repsep(stringPair2, ws ~> ',' <~ ws) <~ (ws ~> '}' <~ ws)
      def strAll2 = '[' ~> repsep(stringPairs2, ws ~> ',' <~ ws) <~ close

      /**
       * key-value pairs, ignore right, map at end
       */
      def stringPair3 =
        stringLit <~ (ws ~> ':' <~ ws) <~ stringLit
      def stringPairs3 =
        ((ws ~> '{' <~ ws) ~> repsep(stringPair3, ws ~> ',' <~ ws) <~ (ws ~> '}' <~ ws)) map { ls =>
          ls map (_.toString)
        }
      def strAll3 = '[' ~> repsep(stringPairs3, ws ~> ',' <~ ws) <~ close

		}
	}

	object CSV extends JavaTokenParsers {
		def csv(p: Parser[JSValue]) = "[" ~> repsep(p, ",") <~ "]" ^^ (x => JSArray(x))
		def doubles = csv(floatingPointNumber ^^ (y => JSDouble(y.toDouble)))
		def bools = csv(("true" ~> success(JTrue)) | ("false" ~> success(JFalse)))
		def strings = csv(stringLiteral ^^ (y => JSString2(y)))
	}

}
