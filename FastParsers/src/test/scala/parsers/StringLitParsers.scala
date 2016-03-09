package parsers

import fastparsers.input.InputWindow
import fastparsers.parsers.Parser
import fastparsers.framework.implementations.FastParsersCharArray._

import InputWindow._

object StringLitParsers {

  /** A string parser: parses a stringlit and converts it to string */
  object StringLitParse {
    lazy val parser = FastParsersCharArray {
      def ws = whitespaces
      def main = '[' ~> repsep(stringLit map (_.toString), ws ~> ',' <~ ws) <~ ']'
    }
  }

  /** recognizes a sequence of stringlits, maps them to string at the end */
  object StringLitMapAtEnd {
    lazy val parser = FastParsersCharArray {
      def ws = whitespaces
      def main = (
        '[' ~> repsep(stringLit, ws ~> ',' <~ ws) <~ ']'
      ) map { ls => ls map (_.toString) }
    }
  }

  /** Just recognizes a sequence of string literals, puts them in a list */
  object StringLitRecognize {
    lazy val parser = FastParsersCharArray {
      def ws = whitespaces
      def main = '[' ~> repsep(stringLit, ws ~> ',' <~ ws) <~ ']'
    }
  }

  /**
   * Just recognizes a sequence of string literals, puts them in a list
   * Uses `strLitRec`
   */
  object StringLitRecognizeRec {
    lazy val parser = FastParsersCharArray {
      def ws = whitespaces
      def main = '[' ~> repsep(stringLitRec, ws ~> ',' <~ ws) <~ ']'
    }
  }

  /**
   * Just recognizes a sequence of string literals, puts them in a list
   * Uses `strLitRec` and `skipws`
   */
  object StringLitRecognizeWsSkipRec {
    lazy val parser = FastParsersCharArray {
      def ws = skipws
      def main = '[' ~> repsep(stringLitRec, ws ~> ',' <~ ws) <~ ']'
    }
  }

  /**
   * recognizes a sequence of string literals, which are all
   * "lastname", puts them in a list
   */
  object LastNameRecognize {
    lazy val parser = FastParsersCharArray {
      def ws = whitespaces
      def lastname = lit("\"lastname\"".toCharArray)
      def main = '[' ~> repsep(lastname, ws ~> ',' <~ ws) <~ ']'
    }
  }

  /**
   * recognizes a sequence of string literals, which are all
   * "lastname", puts them in a list. The array is hoisted out
   */
  object LastNameHoistedRecognize {
    val arr = "\"lastname\"".toCharArray
    lazy val parser = FastParsersCharArray {
      def ws = whitespaces
      def lastname = lit(arr)
      def main = '[' ~> repsep(lastname, ws ~> ',' <~ ws) <~ ']'
    }
  }
}
