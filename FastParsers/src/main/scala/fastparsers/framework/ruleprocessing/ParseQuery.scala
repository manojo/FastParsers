package fastparsers.framework.ruleprocessing

import fastparsers.tools.TreeTools
import fastparsers.input.ParseInput
import scala.collection.mutable.HashMap
import fastparsers.parsers.ParseQueryImplBase

/**
 * This phase does the parser to query specialization phase
 * It receives a `Map[String, RuleInfo]` that represents a mapping from
 * productions to "inlined" rules. Each ruleInfo is either recursive, or
 * contains all basic productions it needed. See `RulesInliner.scala` for
 * more details.
 *
 * The job of parsequery is to simply run the parsequery pass over each rule
 */
trait ParseQuery extends RulesInliner {
    self: ParseQueryImplBase with TreeTools with ParseInput =>

  override def process(rules: HashMap[String, RuleInfo]) = {
    val rulesMap = super.process(rules)
    rulesMap map { case (k, v) => (k, v.copy(code = transform(v.code))) }
  }
}
