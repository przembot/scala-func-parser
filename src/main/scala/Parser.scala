package com.przembot

import scalaz._
import Scalaz._

// Currently without monadic transformers
//final case class Parser[T](parse: String => List[(T, String)])
trait Parser[A] extends (String => List[(A, String)]) {
  def map[B](f: A => B): Parser[B] = {
    s => this(s).map({case (a, str) => (f(a), str)})
  }

  def flatMap[B](f: A => Parser[B]): Parser[B] = ParserMonad.bind(this)(f)
}

object ParserMonad extends Monad[Parser] {
  def point[A](a: => A): Parser[A] = {
    s => List((a,s))
  }

  def bind[A, B](fa: Parser[A])(f: A => Parser[B]): Parser[B] = {
    s => fa(s).flatMap( {case (a, sprim) => f(a)(sprim)} )
  }
}

// Function which can be used with Parser type
object ParserF {
  implicit val parserMonad: Monad[Parser] = ParserMonad

  implicit def runParser[T](p: Parser[T])(input: String): Option[T] = {
    p(input) match {
      case (res, "") :: Nil => Some(res)
      case (_, rs) :: Nil => None // not everything was parsed
      case _  => None // parse failure
    }
  }

  // Retrieves one char from string
  implicit def item: Parser[Char] =
    s => s match {
      case "" => List()
      case _ => List((s.charAt(0), s.substring(1)))
    }

  implicit def char(c: Char): Parser[Char] =
    satisfy(x => x == c)

  // Expresses parsing failure
  implicit def failure[T]: Parser[T] =
    x => List()

  // Try p, then q if p fails
  implicit def option[T](p: Parser[T], q: Parser[T]): Parser[T] =
    s => p(s) match {
      case Nil => q(s)
      case res => res
    }

  // Verify 1 char with predicate
  implicit def satisfy(pred: Char => Boolean): Parser[Char] =
    item >>= (c =>
      if (pred(c))
        Monad[Parser].point(c)
      else
        failure
      )
}
