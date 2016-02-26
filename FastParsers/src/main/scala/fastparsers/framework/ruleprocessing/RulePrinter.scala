package fastparsers.framework.ruleprocessing

import scala.reflect.macros.whitebox.Context
import scala.collection.mutable.HashMap
import fastparsers.parsers.ParserImplBase

/**
 * Creates the final parser object, which contains
 * a `Map[String, String]`, mapping productions to their pretty-printed
 * rules
 */
trait RulePrinter extends ReduceRules { self: ParserImplBase =>
  val c: Context
  import c.universe._

  override def combine(rules: HashMap[String, RuleInfo]) = {
    val anon = TypeName(c.freshName)

    val methods: Map[String, String] = (rules map { case (k, v) =>
      (k, prettyPrint(v.code))
    }).toMap

    val tree = q"""
      class $anon extends fastparsers.framework.implementations.FinalFastParserImpl {
        val ruleMap = $methods
      }
      new $anon
    """
    tree
  }
}
