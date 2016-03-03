package fastparsers.parsers

import scala.annotation.compileTimeOnly

/**
 * Created by Eric on 22.04.14.
 * Interface for parsers representing repetition
 */
trait RepParsers {

  @compileTimeOnly("can’t be used outside FastParser")
  def rep[T](p: Parser[T], min: Int = 0, max: Int = -1): Parser[List[T]] = ???

  @compileTimeOnly("can’t be used outside FastParser")
  def rep1[T](p: Parser[T]): Parser[List[T]] = ???

  @compileTimeOnly("can’t be used outside FastParser")
  def repN[T](p: Parser[T], rep: Int): Parser[List[T]] = ???

  @compileTimeOnly("can’t be used outside FastParser")
  def opt[T](p: Parser[T]): Parser[Option[T]] = ???

  @compileTimeOnly("can’t be used outside FastParser")
  def repsep[T, U](p: Parser[T], sep: Parser[U]): Parser[List[T]] = ???

  @compileTimeOnly("can’t be used outside FastParser")
  def repsep1[T, U](p: Parser[T], sep: Parser[U]): Parser[List[T]] = ???

  @compileTimeOnly("can’t be used outside FastParser")
  def until[T,U](p: Parser[T], sep: Parser[U]): Parser[List[T]] = ???

  implicit class repParser[T](p: Parser[T]) {
    @compileTimeOnly("can’t be used outside FastParser")
    def foldLeft[U](init: U, f: (U, T) => U): Parser[U] = ???

    @compileTimeOnly("can’t be used outside FastParser")
    def foldRight[U, X >: T](init: U, f: (T, U) => U): Parser[U] = ???

    @compileTimeOnly("can’t be used outside FastParser")
    def reduceLeft[U >: T](f: (U, T) => U): Parser[U] = ???

    @compileTimeOnly("can’t be used outside FastParser")
    def reduceRight[U >: T](f: (T, U) => U): Parser[U] = ???

  }

  @compileTimeOnly("can’t be used outside FastParser")
  def repF[T](p: Parser[T]): FoldParser[T] = ???

  type Combine[T, R] = (R, T) => R

  abstract class FoldParser[T] { self =>

    def fold[R](z: R, combine: Combine[T, R]): Parser[R]

    def map[U](f: T => U) = new FoldParser[U] {
      def fold[R](z: R, combine: Combine[U, R]): Parser[R] = self.fold(
        z,
        (acc: R, elem: T) => combine(acc, f(elem))
      )
    }

    def filter(p: T => Boolean) = new FoldParser[T] {
      def fold[R](z: R, comb: Combine[T, R]) = self.fold(
        z,
        (acc: R, elem: T) => if (p(elem)) comb(acc, elem) else acc
      )
    }
  }


}
