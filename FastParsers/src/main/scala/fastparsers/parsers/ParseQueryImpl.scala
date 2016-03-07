package fastparsers.parsers

import fastparsers.error.ParseError
import fastparsers.input._

import scala.collection.mutable.ListBuffer

/**
 * Since it is all a big cake pattern we need to have a `ParseQueryImplBase`
 * trait that we mix in to whatever parsers we want to transform
 */
trait ParseQueryImplBase { self: ParserImplBase  =>
  import c.universe._

  def unwrap(parser: c.Tree): c.Tree = {
    parser match {
      case q"$_.baseParsers[$t]($inner)" =>
        /*println("unwrapping..." + show(inner));*/ inner
      case q"compound[$t]($inner)"  =>
        /*println("unwrapping2..." + show(inner));*/ inner
      case q"$_.compound[$t]($inner)"  =>
        /*println("unwrapping3..." + show(inner));*/ inner
      case _ => parser
    }
  }

  /**
   * The function that transforms the trees according to the
   * transformation rule.
   * In this super trait the transformation is the identity
   */
  def transform(tree: c.Tree): c.Tree = unwrap(tree) match {
    case q"$foo map[$t] $f" =>
      transformMap(foo, f, t)

    case _ =>
      println("Any match in `transform`")
      println(show(tree))
      println()
      tree
  }

  def transformMap(parser: c.Tree, f: c.Tree, typ: c.Tree): c.Tree =
    q"${transform(parser)} map[$typ] $f"
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
   *     For now, we expect f3 to be either a case class constructor (this case
   *     also covers tuples) or method (which can also be an anonymous functions).
   *
   *   - Concat_2: Turning parsers into recognizers (make them id recognizers for now)
   *     T[[ (a ~ b) map { case (a, b) => f(a) } ]] = T[[ a ]] map f <~ recognize(T[[ b ]])
   *     T[[ (a ~ b) map { case (a, b) => f(b) } ]] = recognize(T[[ a ]]) ~> T[[ b ]] map f
   *
   *   - Map_Map over Parser
   *     T[[ T[[ p map f map g ]]  =  T[[ p ]] map (f andThen g) ]]
   */

  override def transformMap(parser: c.Tree, f: c.Tree, typ: c.Tree): c.Tree = {
    unwrap(parser) match {
      case q"$_.baseParsers[$_]($a) ~[$_] $b" =>
        println("It DID match on `transformMap`")
        println(s"Show: ${show(f)}")
        println()

        /*a match {
          case q"$f($s)" => matchCase(s)
          case _ =>
            println("FAILED before matchcase")
            a
        }*/
        matchCase(f)

      case unwrapped =>
        println("It didn't match on `transformMap`")
        println(show(unwrapped))
        println()
        super.transformMap(parser, f, typ)
    }
  }

  private def matchCase(m: c.Tree): c.Tree = {
    m match {
        /*
      case q"$c match { case ..$cases }" =>
        println("YEAH")
        println(show(cases))
        m
      case q"{ case ..$cases }" =>
        println("YEAH2")
        println(show(cases))
        m
      case q"$_ match { case ..$cases }" =>
        println("YEAH3")
        println(show(cases))
        m
      case q"$b match { case ..$cases }" =>
        println("YEAH4")
        println(show(cases))
        println(show(b))
        b
      case q"($x: $t) => $body" =>
        println("YEAH4")
        println(show(x))
        println(show(body))
        body
        */
      case q"($x => $x2 match { case ..$cases })" =>
        println("YEAH5")
        println(show(x))
        println(show(cases))
        processCases(cases)
      case _ => println("FAIL"); println(show(m)); m
    }
  }

  private def processCases(cs: List[c.Tree]): c.Tree = cs.head match {
    case cq"$pat => $body" =>
      println("SHOOT3")
      println(show(pat))
      println(show(body))
      println(showRaw(body))
      body
    case _ =>
      println("SHOOT-fail")
      cs.head
  }

  /** Inspect a function and tell us information about its inner
    * details, like which variables are used and which are not.
    *
    * Use the whitebox context because `FastParsers` use it by default.
    */
  class Inspector {
    import c.universe._

    def getUsedVars(f: Tree): List[Symbol] = {
      val pc = new ParamCollector
      pc.traverse(f)
      val upc = new UsedParamsCollector(pc.params)
      upc.traverse(f)
      upc.usedVariables.toList
    }

    class UsedParamsCollector(params: ListBuffer[Symbol]) extends Traverser {

      private[Inspector] val usedVariables = ListBuffer[Symbol]()

      def isUsed(s: Symbol): Boolean =
        s.isParameter && params.contains(s)

      def isNotStored(s: Symbol): Boolean =
        !usedVariables.contains(s)

      override def traverse(tree: Tree) = tree match {
        case i @ Ident(_) =>
          val sym = i.symbol
          if(isUsed(sym) && isNotStored(sym))
            usedVariables += sym

        // Ignore var references in match clause
        case m @ Match(_, caseDefs) =>
          caseDefs.map(traverse)

        // Ignore var references in if expr
        case i @ If(_, ifTrue, ifFalse) =>
          traverse(ifTrue)
          traverse(ifFalse)

        case _ =>
          super.traverse(tree)
      }
    }

    class BindVariableCollector extends Traverser {

      private[Inspector] val termNames = ListBuffer[TermName]()

      override def traverse(tree: Tree) = tree match {
        case d @ Bind(n, _) =>
          val tn = n.toTermName
          if(!termNames.contains(tn)) {
            termNames += tn
          }
        case _ =>
          println("invalid expression, couldn't find bind")
      }

    }


    class ParamCollector extends Traverser {

      private[Inspector] val params = ListBuffer[Symbol]()

      override def traverse(tree: Tree) = tree match {
        case vd @ ValDef(mods, name, tpt, rhs) =>
          params += vd.symbol
          traverse(rhs)
        case _ =>
          super.traverse(tree)
      }

    }
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
   */
  override def transform(tree: c.Tree): c.Tree = tree match {
    //@TODO write transformations here!
    case _ => super.transform(tree)
  }

  override def transformMap(parser: c.Tree, f: c.Tree, typ: c.Tree): c.Tree = {
    unwrap(parser) match {
      case q"$_.repBis[$d]($a)" =>

        f match {
          case q"($arg => $body)" =>
            println("matching function")
            println()

            body match {
              case q"$prefix.fold[$d2]($z)($comb)" =>

                val t2ed = transform_2(prefix, arg, a)
                //println("matching function application syntax")
                //println(show(d2))
                //println(show(z))
                //println()
                val myTree = c.typecheck(q"$t2ed.fold[$d2]($z, $comb)")

                println("the transform is")
                println(show(myTree))
                println()
                myTree

              case _ =>
                println("not matching sel syntax")
                println(show(body))
                println()
                super.transformMap(parser, f, typ)
            }
        }

      case unwrapped =>
        println("It didn't match on `transformMap`")
        println(show(unwrapped))
        println()
        super.transformMap(parser, f, typ)
    }
  }

  /**
   * handles the T_2[[ ]] transform:
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
   *
   *
   */
  private def transform_2(prefix: c.Tree, arg: c.Tree, parser: c.Tree): c.Tree = {
    prefix match {

      case q"$pre.map[$_]($f)" =>
        val rec = transform_2(pre, arg, parser)
        q"$rec.map($f)"

      case q"$pre.filter($pred)" =>
        val rec = transform_2(pre, arg, parser)
        q"$rec.filter($pred)"

      case q"$arg" =>
        println("Bailing out!!!!")
        println(show(arg))
        println()

        q"repF($parser)"
    }
  }
}

/**
 * responsible for transforming token parsers
 */
trait TokenParseQueryImpl extends ParseQueryImplBase { self: TokenParsersImpl =>

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
