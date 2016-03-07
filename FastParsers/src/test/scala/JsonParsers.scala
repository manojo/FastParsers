import fastparsers.input.InputWindow
import fastparsers.parsers.Parser
import scala.util.parsing.combinator._
import scala.util.parsing.input._

/**
 * Created by Eric on 05.04.14.
 */
object JsonParsers {

  object JSonImpl1 {
    import fastparsers.framework.implementations.FastParsers._
    val jsonparser = FastParser {
      def value:Parser[Any] = whitespaces ~> (obj | arr | stringLit | decimalNumber | "null" | "true" | "false")
      def obj:Parser[Any] = '{' ~> repsep(member,",") <~ "}"
      def arr:Parser[Any] = '[' ~> repsep(value,",") <~ "]"
      def member:Parser[Any] = stringLit ~> ":" ~> value
    }
  }

  val nullValue = "null".toCharArray
  val trueValue = "true".toCharArray
  val falseValue = "false".toCharArray
  val closeBracket = "}".toCharArray
  val closeSBracket = "]".toCharArray
  val comma = ",".toCharArray
  val points = ":".toCharArray

  object JSonImpl2 {
    import fastparsers.framework.implementations.FastParsersCharArray._
    val jsonparser = FastParsersCharArray {
      def value:Parser[Any] = whitespaces ~> (obj | arr | stringLit | decimalNumber | nullValue | trueValue | falseValue)
      def obj:Parser[Any] = '{' ~> repsep(member,comma) <~ closeBracket
      def arr:Parser[Any] = '[' ~> repsep(value,comma) <~ closeSBracket
      def member:Parser[Any] = stringLit ~ (lit(points) ~> value)
    }
  }


  object JSonImpl3 {
    import fastparsers.framework.implementations.FastParsersCharArrayNoInline._
    val jsonparser = FastParsersCharArray {
      def value:Parser[Any] = whitespaces ~> (obj | arr | stringLit | decimalNumber | nullValue | trueValue | falseValue)
      def obj:Parser[Any] = '{' ~> repsep(member,comma) <~ closeBracket
      def arr:Parser[Any] = '[' ~> repsep(value,comma) <~ closeSBracket
      def member:Parser[Any] = stringLit ~ (lit(points) ~> value)
    }
  }

  object JSonImpl4 {
    import fastparsers.framework.implementations.FastParsersCharArrayDefaultErrors._
    val jsonparser = FastParsersCharArray {
      def value:Parser[Any] = whitespaces ~> (obj | arr | stringLit | decimalNumber | nullValue | trueValue | falseValue)
      def obj:Parser[Any] = '{' ~> repsep(member,comma) <~ closeBracket
      def arr:Parser[Any] = '[' ~> repsep(value,comma) <~ closeSBracket
      def member:Parser[Any] = stringLit ~ (lit(points) ~> value)
    }
  }

  object JSonImpl5 {
    import fastparsers.framework.implementations.FastParsersCharArrayIgnoreResults._
    val jsonparser = FastParsersCharArray {
      def value:Parser[Any] = whitespaces ~> (obj | arr | stringLit | decimalNumber | nullValue | trueValue | falseValue)
      def obj:Parser[Any] = '{' ~> repsep(member,comma) <~ closeBracket
      def arr:Parser[Any] = '[' ~> repsep(value,comma) <~ closeSBracket
      def member:Parser[Any] = stringLit ~ (lit(points) ~> value)
    }
  }

  object JSonImpl6 {
    import fastparsers.framework.implementations.FastParsersCharArray._
    import fastparsers.input.InputWindow.InputWindow
    val jsonparser = FastParsersCharArray {
      def value:Parser[Any] = whitespaces ~> (obj | arr | stringLit ^^ (_.toString) | decimalNumber | nullValue | trueValue | falseValue)
      def obj:Parser[Any] = '{' ~> repsep(member,comma) <~ closeBracket
      def arr:Parser[Any] = '[' ~> repsep(value,comma) <~ closeSBracket
      def member:Parser[Any] = stringLit ~ (lit(points) ~> value) ^^ {case (a, b) => (a.toString, b)}
    }
  }

  object JSonImplBoxed {
    import fastparsers.framework.implementations.FastParsersCharArray._
    //GROS HACK
    import fastparsers.input.InputWindow.InputWindow

    sealed abstract class JSValue
    case class JSObject(map: List[(InputWindow[Array[Char]], JSValue)]) extends JSValue
    case class JSArray(arr: List[JSValue]) extends JSValue
    case class JSDouble(d: InputWindow[Array[Char]]) extends JSValue
    case class JSDouble2(d: Double) extends JSValue
    case class JSString(s: InputWindow[Array[Char]]) extends JSValue
    case class JSBool(b: Boolean) extends JSValue
    case object JSNull extends JSValue

    val nullValue = "null".toCharArray
    val trueValue = "true".toCharArray
    val falseValue = "false".toCharArray
    val closeBracket = "}".toCharArray
    val closeSBracket = "]".toCharArray
    val comma = ",".toCharArray
    val points = ":".toCharArray

    val jsonparser = FastParsersCharArray  {
      def value:Parser[JSValue] = whitespaces ~>
       (
         obj |
         arr |
         stringLit ^^ {x => JSString(x)} |
         decimalNumber ^^ {x => JSDouble2(x.toString.toDouble)} |
         //decimalNumber ^^ {x => JSDouble(x)} |
         lit(nullValue) ^^^ JSNull |
         lit(trueValue) ^^^ JSBool(true) |
         lit(falseValue) ^^^ JSBool(false)
       )
      def obj:Parser[JSValue] = ('{' ~> repsep(member,comma) <~ closeBracket) ^^ {x => JSObject(x)}
      def arr:Parser[JSValue] = ('[' ~> repsep(value,comma) <~ closeSBracket) ^^ {x => JSArray(x)}
      def member:Parser[(InputWindow[Array[Char]], JSValue)] = stringLit ~ (lit(points) ~> value)
    }

  }

  object JSON extends JavaTokenParsers {
    def value: Parser[Any] = obj | arr | stringLiteral |
      floatingPointNumber |
      "null" | "true" | "false"
    def obj: Parser[Any] = "{" ~> repsep(member, ",") <~ "}"
    def arr: Parser[Any] = "[" ~> repsep(value, ",") <~ "]"
    def member: Parser[Any] = stringLiteral ~> ":" ~> value
  }

  //if needed
  def JsonEqual(a:Any,b:Any):Boolean = (a,b) match {
    case (x::xs,y::ys) => JsonEqual(x,y) && JsonEqual(xs,ys)
    case (Tuple2(x1,y1),JSON.~(x2,y2)) => JsonEqual(x1,x2) && JsonEqual(y1,y2)
    case (JSON.~(x1,y1),(x2,y2)) => JsonEqual(x1,x2) && JsonEqual(y1,y2)
    case _ => a == b

  }

  /**
   * grabbing the JSON Parser from fastparse
   * @see https://github.com/lihaoyi/fastparse/blob/master/fastparse/shared/src/test/scala/fastparse/JsonTests.scala
   */
  object FastParseJSON {
    import fastparse.all._

    object Js {
      sealed trait Val extends Any {
        def value: Any
        def apply(i: Int): Val = this.asInstanceOf[Arr].value(i)
        def apply(s: java.lang.String): Val =
          this.asInstanceOf[Obj].value.find(_._1 == s).get._2
      }

      case class Str(value: java.lang.String) extends AnyVal with Val
      case class Obj(value: Map[java.lang.String, Val]) extends AnyVal with Val
      case class Arr(value: Array[Val]) extends AnyVal with Val
      case class Num(value: Double) extends AnyVal with Val

      case object False extends Val {
        def value: Boolean = false
      }
      case object True extends Val {
        def value: Boolean = true
      }
      case object Null extends Val {
        def value: Val = null
      }
    }

    case class NamedFunction[T, V](f: T => V, name: String) extends (T => V) {
      def apply(t: T): V = f(t)
      override def toString(): String = name
    }

    // Here is the parser
    val Whitespace = NamedFunction(" \r\n".contains(_: Char), "Whitespace")
    val Digits = NamedFunction('0' to '9' contains (_: Char), "Digits")
    val StringChars = NamedFunction(!"\"\\".contains(_: Char), "StringChars")

    val space = P(CharsWhile(Whitespace).?)
    val digits = P(CharsWhile(Digits))
    val exponent = P(CharIn("eE") ~ CharIn("+-").? ~ digits)
    val fractional = P("." ~ digits)
    val integral = P("0" | CharIn('1' to '9') ~ digits.?)

    val number = P(CharIn("+-").? ~ integral ~ fractional.? ~ exponent.?).!.map(
      x => Js.Num(x.toDouble)
    )

    val `null` = P("null").map(_ => Js.Null)
    val `false` = P("false").map(_ => Js.False)
    val `true` = P("true").map(_ => Js.True)

    val hexDigit = P(CharIn('0' to '9', 'a' to 'f', 'A' to 'F'))
    val unicodeEscape = P("u" ~ hexDigit ~ hexDigit ~ hexDigit ~ hexDigit)
    val escape = P("\\" ~ (CharIn("\"/\\bfnrt") | unicodeEscape))

    val strChars = P(CharsWhile(StringChars))
    val string =
      P(space ~ "\"" ~/ (strChars | escape).rep.! ~ "\"").map(Js.Str)

    val array =
      P("[" ~/ jsonExpr.rep(sep = ",".~/) ~ space ~ "]").map(xs => Js.Arr(xs.toArray))

    val pair = P(string.map(_.value) ~/ ":" ~/ jsonExpr)

    val obj =
      P("{" ~/ pair.rep(sep = ",".~/) ~ space ~ "}").map(xs => Js.Obj(xs.toMap))

    val jsonExpr: P[Js.Val] = P(
      space ~ (obj | array | string | `true` | `false` | `null` | number) ~ space
    )
  }
}
