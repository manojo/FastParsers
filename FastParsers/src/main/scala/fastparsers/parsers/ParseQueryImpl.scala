package fastparsers.parsers

import fastparsers.error.ParseError
import fastparsers.input._

import scala.collection.mutable.ListBuffer

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
 * Responsible for transforming base parsers
 */
trait BaseParseQueryImpl extends ParseQueryImplBase with Traversers {
  self: BaseParsersImpl =>

  import c.universe._, c.internal._, decorators._

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
  override def transform(tree: c.Tree): c.Tree = tree match {
    case topExpression @ q"$parser map[$t] $f" =>
      (rewriteMapOnParsers(parser, f) map {
        (t: (c.Tree, c.Tree, TransformInfo)) =>
          val (newMatchCase, parsers, info) = t
          val finalRes = q"$parsers map[(Int, Int)] $newMatchCase"
          log("Final result", show(finalRes))
          c.typecheck(finalRes)
      }).getOrElse(topExpression)
    case t =>
      println("Any match in `transform`")
      println(show(t))
      super.transform(tree)
  }

  type FinalRes = Option[(c.Tree, c.Tree, TransformInfo)]

  private def unwrap(parser: c.Tree)(f: c.Tree => FinalRes): FinalRes = {
    parser match {
      case q"$p.baseParsers[$t]($inner)" =>
        f(inner) map {
          (t: (c.Tree, c.Tree, TransformInfo)) =>
            val (newMatchCase, mappedParsers, info) = t
            val res = c.typecheck(q"$p.baseParsers[(Int, Int)]($mappedParsers)")
            (newMatchCase, res, info)
        }
      case _ =>
        abort("Could not unwrap parser")
    }
  }

  private def rewriteMapOnParsers(parser: c.Tree, f: c.Tree): FinalRes = {
    unwrap(parser) {
      case q @ q"$p.baseParsers[$t]($a) ~[$_] $b" =>
        identifyMatchCase(f) map {
          (t: (c.Tree, TransformInfo)) =>
            val (newMatchCase, info) = t
            info.fs match {
              case List(first, second) =>
                val parser1 = c.typecheck(q"$a map[Int] $first")
                val parser2 = c.typecheck(q"$b map[Int] $second")
                (newMatchCase, c.typecheck(q"$parser1 ~[Int] $parser2"), info)
              case _ =>
                abort("We haven't still generalized this part")
            }
        }

      case x =>
        println(showRaw(x))
        abort("It didn't match on `rewriteMapOnParsers`")
    }
  }

  private def identifyMatchCase(m: c.Tree): Option[(c.Tree, TransformInfo)] = {
    m match {
      case q"($x => $x2 match { case ..$cases })" =>
        if (cases.size != 1) None
        else {
          val (rewrittenCase, info) = rewriteCase(cases.head)
          val newCase = List(rewrittenCase)
          val f = q"((x: ${info.resultType}) => x match { case ..$newCase })"
          Some((f, info))
        }
      case _ =>
        error(show(m))
        abort("Couldn't find match function")
    }
  }

  case class TransformInfo(
    fs: List[c.Tree],
    resultTypes: List[Type],
    resultType: c.Tree
  )

  private def rewriteCase(cs: Tree): (Tree, TransformInfo) =
    cs match {
      case cse @ cq"$pattern => $body" =>
        val usedSymbols = (new UsedVariablesInPattern).inspect(pattern)
        val funs = (new TransformLogic(usedSymbols)).replicate(body)

        val q"$prefix[..$targs](..$args)" = body

        val resultTypes: List[Type] = funs map {
          (f: c.Tree) =>
            val q"($header => $body)" = f
            body.tpe.finalResultType
        }

        val seedResult = tq"(${resultTypes.head}, ${resultTypes.tail.head})"
        val resultType = resultTypes.tail.tail.foldLeft(seedResult){
          (t: Tree, tp: Type) =>
            tq"($t, $tp)"
        }

        val newVariables: List[TermSymbol] = resultTypes map {
          (t: Type) =>
            val newName = c.freshName("temp")
            setInfo(enclosingOwner.newTermSymbol(newName), t)
        }

        val names = newVariables.map(_.name)
        val seed = pq"(${names.head}, ${names.tail.head})"
        val newPattern = names.tail.tail.foldLeft(seed) {
          (t: c.Tree, tn: TermName) =>
            pq"($t, $tn)"
        }

        val newBody = q"$prefix[..$resultTypes](..$names)"
        val caseClause = cq"$newPattern => $newBody"
        val info = TransformInfo(funs, resultTypes, resultType)
        (caseClause, info)

      case _ =>
        abort("Failed to identify pattern and body in case clause")
    }


  class TransformLogic(val symbols: List[Symbol]) {
    def replicate(tree: c.Tree): List[c.Tree] = tree match {
      case a @ Apply(typeApply, elems) =>
        val res = elems.foldLeft[Option[List[c.Tree]]](Some(List())) {
          (acc: Option[List[c.Tree]], elem: c.Tree) =>
            elem match {
              case q"$f(..$fargs)" =>

                val leftVariable = lookUpInFunctionSelect(f)
                log("Symbol on the left", leftVariable.toString)

                val rightVariables = fargs.foldLeft(List.empty[Symbol]) {
                  (acc, t) => acc ++ lookUpFunctionParameters(t)
                }
                log("Symbols on the right", rightVariables.toString)

                /* Optimize only if there is only one reference to
                 * `TermName`s representing the variables on which
                 * we pattern match. Otherwise, returned the original tree. */
                acc map { ls =>
                  (leftVariable, rightVariables) match {
                    case (Some(left), Nil) =>
                      createAnonFunction(elem, left) :: ls
                    case (Some(left), xs) => Nil
                    case (None, List(right)) =>
                      createAnonFunction(elem, right) :: ls
                    case (None, Nil) => Nil
                  }
                }
              case x =>
                abort(s"Couldn't find UnApply but ${showRaw(x)}")
            }
        }
        log("Result of `genFunctionsFromLogic`", showRaw(res))
        res.getOrElse(Nil).reverse
    }

    def lookUpInFunctionSelect(s: c.Tree): Option[Symbol] = {
      s match {
        case Select(i @ Ident(tn), _)
          if symbols.contains(i.symbol) => Some(i.symbol)
        case _ => None
      }
    }

    def lookUpFunctionParameters(s: c.Tree): List[Symbol] =
      new UsedRefIdent(symbols).getReferences(s)

    def createAnonFunction(body: c.Tree, old: Symbol): c.Tree = {
      val temp = c.freshName("temp")
      val oldType = old.typeSignature
      val tempSym = setInfo(enclosingOwner.newTermSymbol(temp), oldType)
      val tempDef = valDef(tempSym)

      val renamedBody = typingTransform(body)((tree, api) => tree match {
        case i @ Ident(n) if i.symbol == old =>
          api.typecheck(q"$tempSym")
        case x =>
          api.default(tree)
      })

      val newBody = changeOwner(renamedBody, old.owner, enclosingOwner)
      c.typecheck(q"($tempDef => $newBody)")
    }
  }

  def log(what: String, msg: String): Unit = {
    println(s"=========== $what ===========")
    println(s"$msg")
  }

  def error(msg: String): Unit = {
    println(s"=========== ERROR ===========")
    println(s"$msg")
  }

  def abort(msg: String) =
    c.abort(c.enclosingPosition, msg)

}

trait Traversers {
  self: BaseParsersImpl with ParseQueryImplBase=>
  import c.universe._

  /** Inspect a pattern and gives us the names of the
    * variable on which we are pattern matching. */
  class UsedVariablesInPattern extends Traverser {

    private val usedSymbols = ListBuffer[Symbol]()

    def inspect(tree: c.Tree): List[Symbol] = {
      traverse(tree)
      usedSymbols.toList
    }

    override def traverse(tree: c.Tree) = tree match {
      case u @ UnApply(_, args) =>
        args map {
          case d @ Bind(n, _) =>
            val sym = d.symbol
            if(!usedSymbols.contains(sym)) {
              usedSymbols += sym
            }
          case x =>
            c.abort(c.enclosingPosition,
              s"invalid expression, couldn't find Bind but ${showRaw(x)}")
        }
      case x =>
        c.abort(c.enclosingPosition,
          s"invalid expression, couldn't find UnApply but ${showRaw(x)}")
    }
  }

  /** Inspect all the references to `Ident`, that is, any reference
    * to an existing variable in scope and return them. */
  class UsedRefIdent(symbols: List[Symbol]) extends Traverser {

    def getReferences(s: c.Tree): List[Symbol] = {
      val collector = new UsedRefIdent(symbols)
      collector.traverse(s)
      collector.collectedTermNames.toList
    }

    val collectedTermNames = ListBuffer[Symbol]()

    override def traverse(tree: c.Tree) = tree match {
      case i @ Ident(_) if symbols.contains(i.symbol) =>
        collectedTermNames += i.symbol
      case _ => super.traverse(tree)
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
