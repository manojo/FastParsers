package fastparsers.parsers

import fastparsers.input._
import fastparsers.framework._
import fastparsers.error.ParseError

/**
 * Since it is all a big cake pattern we need to have a `ParseQueryImplBase`
 * trait that we mix in to whatever parsers we want to transform
 */
trait ParseQueryImplBase { self: ParserImplBase  =>

  import c.universe._

  /**
   * The function that transforms the trees according to the
   * transformation rule.
   * In this super trait the transformation is the identity
   */
  def transform(tree: c.Tree): c.Tree = tree
}

/**
 * responsible for transforming base parsers
 */
trait BaseParseQueryImpl extends ParseQueryImplBase { self: BaseParsersImpl =>
  import c.universe._

  /**
   *  The following rules apply
   *
   *   - Concat_1: Pushing of maps closer to their creation
   *     T[[ (a ~ b) map { case (a, b) => f3(f1(a), f2(b)) } ]] =
   *     (T[[ a ]] map f1 ~ T[[ b ]] map f2) map { case (a, b) => f3(a, b) }
   *
   *   - Concat_2: Turning parsers into recognizers (make them id recognizers for now)
   *     T[[ (a ~ b) map { case (a, b) => f(a) } ]] = T[[ a ]] map f <~ recognize(T[[ b ]])
   *     T[[ (a ~ b) map { case (a, b) => f(b) } ]] = recognize(T[[ a ]]) ~> T[[ b ]] map f
   *
   *   - Map_Map over Parser
   *     T[[ T[[ p map f map g ]]  =  T[[ p ]] map (f andThen g) ]]
   */
  override def transform(tree: c.Tree): c.Tree = tree match {
    //@TODO write transformations here!
    case _ => super.transform(tree)
  }
}


/**
 * responsible for transforming rep parsers
 */
trait RepParseQueryImpl extends ParseQueryImplBase { self: RepParsersImpl =>
  import c.universe._

  /**
   * The following rules apply:
   *
   *   - Rep_1: Pushing map onto parser
   *     T[[ rep(p) map { ls => f2(ls map f) } ]] =
   *     T[[ rep(T[[ p ]] map f) map { ls => f2(ls) } ]]
   *
   *   - Rep_2: Composing over Foldable
   *     T[[ rep(p) map { ls => f(ls).fold(z, combine) } ]] =
   *     T_2[[ f ]](repFG(T[[ p ]])).fold(z, combine),
   *
   *     where
   *     f: Foldable[T] => Foldable[U] and
   *     T_2[[ ]]: (Foldable[T] => Foldable[U]) => (FoldGrammar[T] => FoldGrammar[U])
   *
   * This second transformation T_2[[ ]] basically maps a function over Foldable[T]
   * into a function over FoldGrammar[T]. The only difference between a Foldable[T]
   * and a FoldGrammar[U] is the signature of their fold function
   *  T_2[[ ]]: (Foldable[T] => Foldable[U]) => (FoldGrammar[T] => FoldGrammar[U])
   *
   *    - Map propagates
   *    T_2[[ fs: Foldable[T] => f2(fs) map f ]] = fg: FoldGrammar[T] => T_2[[ f2 ]](fg) map f
   *
   *    - Filter propagates
   *    T_2[[ fs: Foldable[T] => f2(fs) filter f ]] =
   *     fg: FoldGrammar[T] => T_2[[ f2 ]](fg) filter f
   *
   *    - Id
   *    T_2[[ fs: Foldable[T] => fs ]] = fg: FoldGrammar[T] => fg
   */
  override def transform(tree: c.Tree): c.Tree = tree match {
    //@TODO write transformations here!
    case _ => super.transform(tree)
  }
}

/**
 * responsible for transforming token parsers
 */
trait TokenParseQueryImpl extends ParseQueryImplBase { self: TokenParsersImpl =>
  import c.universe._

  override def transform(tree: c.Tree): c.Tree = tree match {
    //@TODO write transformations here!
    case _ => super.transform(tree)
  }
}

/**
 * mixes in the whole thing, to be used for full transformation
 */
trait FullParseQueryImpl
    extends BaseParseQueryImpl
    with RepParseQueryImpl
    with TokenParseQueryImpl
    with BaseParsersImpl
    with RepParsersImpl
    with TokenParsersImpl {

  self: ParseInput
        with ParseError
        with IgnoreResultsPolicy
        with StringLikeInput =>
}
