package fastparsers.framework.implementations

import scala.language.experimental.macros
import fastparsers.parsers._
import scala.reflect.macros.whitebox.Context
import fastparsers.framework.ruleprocessing._
import fastparsers.input.StringInput
import fastparsers.input.CharArrayInput
import fastparsers.error.IgnoreParseError

trait FastParserSuperTrait
    extends BaseParsers[Char, Array[Char]]
    with RepParsers
    with TokenParsers[Array[Char]]
    with FlatMapParsers

object Tester extends FastParserSuperTrait {
  /**
   * The return type here because of the whitebox nature
   * of the macro. We need to be able to call any method
   * that we declare inside a parser scope, for any parser
   * These methods cannot a priori be members of the
   * `FinalFastParserImpl` trait
   */
  def runIt(rules: => Unit)(fps: FastParserSuperTrait*): Any =
    macro impl

  def impl(c: Context)(rules: c.Tree)(fps: c.Tree*) = {
    import c.universe._
    val ls = fps.toList.map { config =>
      q"$config.FastParser($rules)"
    }
    q"$ls"
  }
}

/**
 * This trait is useful only for the pretty printing a parser
 */
object FastPrinters extends FastParserSuperTrait {

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

object TransformedPrinters extends FastParserSuperTrait {

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
