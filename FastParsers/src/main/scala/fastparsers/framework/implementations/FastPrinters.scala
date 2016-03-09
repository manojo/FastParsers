package fastparsers.framework.implementations

import scala.language.experimental.macros
import fastparsers.parsers._
import scala.reflect.macros.whitebox.Context
import fastparsers.framework.ruleprocessing.{
  RulesInliner, RulesTransformer, RulePrinter, ParseQuery, ParseRules
}
import fastparsers.input.StringInput
import fastparsers.error.IgnoreParseError

/**
 * This trait is useful only for the pretty printing a parser
 */
object FastPrinters
    extends BaseParsers[Char, String]
    with RepParsers
    with TokenParsers[String]
    with FlatMapParsers {

  def FastParser(rules: => Unit): FinalFastParserImpl =
    macro FastPrintersImpl.FastParser
}

/**
 * our pretty printer must work before the `ParseRules` pass
 * since we want to prettyprint before the expansion phase
 * Also, instead of the final `RuleCombiner` phase, we mix in
 * the `RulePrinter` phase, which creates an object with a
 * `Map[String, String]`, mapping a production to its pretty-printed
 * form
 */
class FastPrintersImpl(val c: Context)
    extends BaseImpl
    with RulesTransformer
    with RulesInliner
    with ParseRules
    with BaseParsersImpl
    with RepParsersImpl
    with TokenParsersImpl
    with FlatMapImpl
    with RulePrinter
    with StringInput
    with IgnoreParseError
    with DontIgnoreResults {

  override def FastParser(rules: c.Tree) = super.FastParser(rules)
}


object TransformedPrinters
    extends BaseParsers[Char, String]
    with RepParsers
    with TokenParsers[String]
    with FlatMapParsers {

  def FastParser(rules: => Unit): FinalFastParserImpl =
    macro TransformedPrintersImpl.FastParser
}

/**
 * mixing in the ParseQuery transformation phase
 */
class TransformedPrintersImpl(val c: Context)
    extends BaseImpl
    with RulesTransformer
    with ParseQuery
    with ParseRules
    with FullParseQueryImpl
    with BaseParsersImpl
    with RepParsersImpl
    with TokenParsersImpl
    with FlatMapImpl
    with RulePrinter
    with StringInput
    with IgnoreParseError
    with DontIgnoreResults {

  override def FastParser(rules: c.Tree) = super.FastParser(rules)
}

