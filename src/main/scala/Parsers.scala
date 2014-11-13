package suag

import language.higherKinds

object Applicatives {


  trait Applicative[P[_]] {
    def pure[A](a: => A): P[A]
    def <*>[A, B](p: => P[B => A])(pb: => P[B]): P[A]
    def <&>[A, B](f: B => A)(pb: => P[B]): P[A] = <*>(pure(f))(pb)
  }

  trait Alternative[P[_]] {
    def empty[A]: P[A]
    def <|>[A](p: => P[A])(q: => P[A]): P[A]
  }

  trait Monad[P[_]] extends Applicative[P] {
    def flatMap[A, B](fa: P[B])(f: B => P[A]): P[A]
    def map[A, B](fa: P[B])(f: B => A): P[A] = <&>(f)(fa)
    def <*>[A, B](p: P[B => A])(pb: P[B]): P[A] = flatMap(p)(f => map(pb)(b => f(b)))
  }

  // Implicit so for-comprehensions work on applicatives
  implicit class ScalaSyntax[F[_], A](val m: F[A]) extends AnyVal {
    def map[B](f: A => B)(implicit ev: Applicative[F]): F[B] = ev.<&>(f)(m)
  }

  // Implicit so for-comprehensions work on monads
  implicit class ScalaMSyntax[F[_], A](val m: F[A]) extends AnyVal {
    def map[B](f: A => B)(implicit ev: Monad[F]): F[B] = ev.map(m)(f)
    def flatMap[B](f: A => F[B])(implicit ev: Monad[F]): F[B] = ev.flatMap(m)(f)
  }

  // Methods on specific B => A of Applicatives
  implicit class AppSyntax1[F[_], B, A](val p: F[B => A]) extends AnyVal {
    def <*>(q: => F[B])(implicit ev: Applicative[F]): F[A] = ev.<*>(p)(q)
  }

  // Lifting function B => A into Applicatives
  implicit class AppSyntax2[F[_], B, A](val f: B => A) extends AnyVal {
    def <&>(p: => F[B])(implicit ev: Applicative[F]): F[A] = ev.<&>(f)(p)
  }

  // General Applicatives
  implicit class AppSyntax3[F[_], A](val p: F[A]) extends AnyVal {
    def <*[B](q: => F[B])(implicit ev: Applicative[F]): F[A] = {
      def dropRight[X, Y]: X => Y => X = x => _ => x
      dropRight[A, B] <&> p <*> q
    }

    def *>[B](q: => F[B])(implicit ev: Applicative[F]): F[B] = {
      def dropLeft[X, Y]: X => Y => Y = _ => y => y
      dropLeft[A, B] <&> p <*> q
    }

    def <|>(q: => F[A])(implicit ev: Alternative[F]): F[A] = ev.<|>(p)(q)

    def opt(v: => A)(implicit ev: Alternative[F], ev2: Applicative[F]): F[A] = p <|> ev2.pure(v)

    def zeroOrMore(implicit appEv: Applicative[F], altEv: Alternative[F]): F[Stream[A]] = {

      def cons[A] = (a: A) => (l: Stream[A]) => (a #:: l)
      cons <&> p <*> p.zeroOrMore opt Stream.empty
    }

    def <**>[B](q: => F[A => B])(implicit ev: Applicative[F]): F[B] = ((a: A) => (f: A => B) => f(a)) <&> p <*> q
   
    def `<??>`(q: => F[A => A])(implicit appEv: Applicative[F], altEv: Alternative[F]): F[A] = p <**> (q opt identity)
  }

  // Having F[_] is awkward but needed to aid implicit resolution
  implicit class AppSyntax4[F[_], A](val a: A) extends AnyVal {
    def <&[B](p: => F[B])(implicit ev: Applicative[F]): F[A] = ev.pure(a) <* p
  }
}

object Parsers {
  import Applicatives._

  implicit def ParserApplicative[S] = new Applicative[({ type P[A] = Parser[S, A] })#P] {
    def pure[A](a: => A) = pSucceed(a)

    def <*>[A, B](p: => Parser[S, B => A])(pb: => Parser[S, B]) = Parser(inp =>
      for ((b2a, r) <- p(inp); (b, rr) <- pb(r)) yield (b2a(b), rr))

  }

  implicit def ParserAlternative[S] = new Alternative[({ type P[A] = Parser[S, A] })#P] {
    def empty[A] = pFail[S, A]

    // It should take Parser[S, A], Parser[S, B] and return Parser[S, C] where C >: A and C >: B
    def <|>[A](p: => Parser[S, A])(q: => Parser[S, A]) = new Parser[S, A](inp => p(inp) #::: q(inp))
  }

  /**
   * Need all these helpers to aid implicit resolution. Is there a way to generalize this to every curried Applicative?
   * Scala implicit resolution can't work with higher kinds properly, hence we add this implicits to lower the kind lvl for parsers.
   */
  type PParser[S] = {
    type P[+A] = Parser[S, A]
  }

  implicit def parserSyntax1[A, B, S] = AppSyntax1[PParser[S]#P, A, B](_: PParser[S]#P[A => B])
  implicit def parserSyntax2[A, B, S] = AppSyntax2[PParser[S]#P, A, B](_: A => B)
  implicit def parserSyntax3[A, S] = AppSyntax3[PParser[S]#P, A](_: PParser[S]#P[A])
  implicit def parserSyntax4[A, S] = AppSyntax4[PParser[S]#P, A](_: A)

  /**
   * type Parser s a = [s] -> [(a, [s])]
   * Implemented as synthetic wrapper over Stream[S] => Stream[(A, Stream[S])] to simplify type error reporting to Parser[S, A]
   */
  class Parser[S, +A](val f: Stream[S] => Stream[(A, Stream[S])]) extends AnyVal {
    def apply(s: => Stream[S]): Stream[(A, Stream[S])] = f(s)
    //def apply(s: => Seq[S]): Stream[(A, Stream[S])] = f(s.toStream)
  }
  def Parser[S, A](f: => Stream[S] => Stream[(A, Stream[S])]): Parser[S, A] = new Parser[S, A](f)

  def pFail[S, A]: Parser[S, A] = Parser(inp => Stream.empty)

  def pSucceed[S, A](v: => A): Parser[S, A] = Parser(inp => Stream((v, inp)))

  def pSym[S](a: => S): Parser[S, S] = Parser {
    case s #:: ss if a == s => Stream((a, ss))
    case _ => Stream.empty
  }

  // Accepts and consumes any input
  def pAny[S]: Parser[S, S] = Parser {
    case head #:: tail => Stream((head, tail))
    case Stream.Empty => Stream.Empty
  }

  def pTok[S](tok: => Stream[S]): Parser[S, S] = {
    def zippy(toks: Stream[S], inp: Stream[S]): Stream[(S, Stream[S])] = (toks, inp) match {
      case (Stream.Empty, _) => Stream.Empty
      case (_, Stream.Empty) => Stream.Empty
      case (a #:: as, s #:: ss) => (a, ss) #:: zippy(as, ss)
    }
    
    Parser {
      case s if s startsWith tok => zippy(tok, s)
      case _ => Stream.empty
    }
  }

  // Wrappers
  def pParens[S, A](l: S)(r: S)(p: Parser[S, A]): Parser[S, A] = {
    pSym(l) *> p <* pSym(r)
  }
}

object Test2 extends App {
  import Parsers._, Applicatives._

  // Lists reduction, are these foldLeft and foldRight?
  def flip[A, B, C] = (f: A => B => C) => (b: B) => (a: A) => f(a)(b)
  
  def pChainl[S, C](op: Parser[S, C => C => C])(x: Parser[S, C]): Parser[S, C] = {
    def f: C => Stream[C => C] => C = x => {
      case Stream.Empty => x
      case func #:: rest => f(func(x))(rest)
    }
    f <&> x <*> (flip <&> op <*> x).zeroOrMore
  }

  def pChainr[S, C](sep: Parser[S, C => C => C])(p: Parser[S, C]): Parser[S, C] = {
    p `<??>` (flip <&> sep <*> pChainr(sep)(p))
  }

  object Expressions {
    type Id = String

    case class TypeDescr()

    sealed trait Expr
    case class Lambda(id: Id, expr: Expr) extends Expr
    case class App(l: Expr, r: Expr) extends Expr
    case class TypedExpr(td: TypeDescr, e: Expr) extends Expr
    case class Ident(id: Id) extends Expr

    def pFactor: Parser[Char, Expr] = {
      ((Lambda.curried <& pSym('\\') <*> pIdent <* pSym('.') <*> pExpr):Parser[Char, Expr]) <|>
      (pParens('(')(')')(pExpr)) <|>
      (Ident.apply _ <&> pIdent)
    }

    def pExpr: Parser[Char, Expr] = pChainl[Char, Expr](pSucceed(App.curried))(pFactor `<??>`
      (TypedExpr.curried <& pTok("::".toStream) <*> pTypeDescr))

    def pTypeDescr: Parser[Char, TypeDescr] = pFail // TODO

    def pIdent: Parser[Char, Id] = Parser {
      inp =>
        val ident = inp.takeWhile(c => ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')).mkString
        if (ident.isEmpty) Stream.Empty
        else Stream((ident, inp drop ident.length))
    }
  }
 
  object Factorization {
    type S = Char
    type Q = Char
    type B = Any
    def q: Parser[S, Q] = ???
    def f: Q => Q => B = ???
    def g: Q => Q => B = ???
    def r1: Parser[S, Q] = ???
    def r2: Parser[S, Q] = ???

    def flip[A, B] = (f: A => A => B) => (x: A) => (y: A) => f(y)(x)
    val p = q <**> (flip(f) <&> r1 <|> (flip(g) <&> r2))
  }

  object PreMonadic {
    def times[S, A](n: Int)(p: => Parser[S, A]): Parser[S, Stream[A]] = n match {
      case 0 => pSucceed[S, Stream[A]](Stream.Empty)
      case n =>
        val concat = (a: A) => (as: Stream[A]) => a #:: as
        concat <&> p <*> times(n - 1)(p)
    }

    val a = pSym('a')
    val b = pSym('b')
    val c = pSym('c')

    def abc(n: Int): Parser[Char, Int] = n <& times(n)(a) <* times(n)(b) <* times(n)(c)

    def foldr[A, B](as: Stream[A])(z: => B)(f: (=> A, => B) => B): B = as match {
      case x #:: xs => f(x, foldr(xs)(z)(f))
      case _ => z
    }

    val count: Stream[Int] = {
      def loop(v: Int): Stream[Int] = v #:: loop(v + 1)
      loop(0)
    }

    def func(p: => Parser[Char, Int], q: => Parser[Char, Int]) = p <|> q

    // Diverges after n is higher than 'a' and will never match again
    val pABC = foldr(count map abc)(pFail[Char, Int])(func)
  }
}
