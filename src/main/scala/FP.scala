import scala.annotation.tailrec

object Ch2 {
 def fib(n: Int) = {
   @tailrec def internalfib(i: Int, m_2: Int, m_1: Int): Int = i match {
     case 0 | 1 => m_2 + m_1
     case i => internalfib(i - 1, m_1, m_2 + m_1)
   }

   internalfib(n, 0, 1)
 }

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = (a: A) => (b: B) => f(a, b)
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a: A, b: B) => f(a)(b)
  def compose[A, B, C](f: B => C, g: A => B): A => C = (a: A) => f(g(a))
}

object Ch3 {

  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }

    def product(ds: List[Double]): Double = ds match {
      case Nil => 0
      case Cons(0.0, _) => 0.0
      case Cons(x, xs) => x * product(xs)
    }

    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))

    def tail[A](l: List[A]): List[A] = l match {
      case Nil => throw new Exception("Nil doesn't have tail!")
      case Cons(_, xs) => xs
    }

    def setHead[A](head: A)(l: List[A]): List[A] = l match {
      case Nil => throw new Exception("Nil doesn't have head!")
      case Cons(_, xs) => Cons(head, xs)
    }

    def drop[A](l: List[A], n: Int): List[A] = n match {
      case 0 => l
      case n => l match {
        case Nil => Nil
        case Cons(_, xs) => drop(xs, n - 1)
      }
    }

    def dropWhile[A](l: List[A])(p: A => Boolean): List[A] = l match {
      case Cons(x, xs) if p(x) => dropWhile(xs)(p)
      case l => l
    }

    def init[A](l: List[A]): List[A] = l match {
      case Cons(x, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
      case Nil => Nil
    }

    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
      as match {
        case Nil => z
        case Cons(x, xs) => f(x, foldRight(xs, z)(f))
      }
    }

    def sum2(ns: List[Int]) = foldRight(ns, 0)(_ + _)
    def product2(ns: List[Int]) = foldRight(ns, 1)(_ * _)

    @tailrec def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
      as match {
        case Nil => z
        case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
      }
    }

    def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((acc, h) => Cons(h, acc))

    def foldRight2[A, B](as: List[A], z: B)(f: (A, B) => B): B = foldLeft(reverse(as), z)((b, a) => f(a, b))
  }
}

object Ch6 {

  trait RNG {
    def nextInt: (Int, RNG)
  }

  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (next, nextRNG) = rng.nextInt
    (if (next == Int.MinValue) 0 else math.abs(next), nextRNG)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (next, nextRNG) = nonNegativeInt(rng)
    (if (next == Int.MaxValue) 0 else (next.toDouble / Int.MaxValue.toDouble), nextRNG)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (t, r) = intDouble(rng)
    (t.swap, r)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    count match {
      case 0 => (Nil, rng)
      case x =>
        val (i, r1) = rng.nextInt
        val (l, r2) = ints(count - 1)(r1)
        (i :: l, r2)
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def flatMap[A, B](s: Rand[A])(f: A => Rand[B]): Rand[B] = rng => {
    val (a, r1) = s(rng)
    f(a)(r1)
  }

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  def _double(rng: RNG): Rand[Double] = {
    map(nonNegativeInt)(next => if (next == Int.MaxValue) 0.0 else (next.toDouble / Int.MaxValue.toDouble))
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

  def randIntDouble: Rand[(Int, Double)] = both(int, double)
  def randDoubleInt: Rand[(Double, Int)] = both(double, int)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs.foldLeft(unit(Nil: List[A])) { (acc, x) =>
      rng => {
        val (l, r1) = acc(rng)
        val (e, r2) = x(r1)
        (e :: l, r2)
      }
    }
  }

  def sequence2[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs.foldLeft(unit(Nil: List[A])) { (acc, x) =>
      flatMap(acc)(l => map(x)(_ :: l))
    }
  }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n -1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }


  object State {
    def unit[S, A](a: A): State[S, A] = State(s => (a, s))

    def get[S]: State[S, S] = State(s => (s, s))
    def set[S](s: S): State[S, Unit] = State(_ => ((), s))

    def modify[S](f: S => S): State[S, Unit] = for {
      s <- get
      _ <- set(f(s))
    } yield ()

    def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = fs.foldLeft(State.unit[S, List[A]](Nil)) { (acc, x) =>
      acc.flatMap(l => x.map(_ :: l))
    }
  }

  case class State[S, +A](run : S => (A, S)) {
    def flatMap[B](f: A => State[S, B]): State[S, B] = State(rng => {
      val (a, r1) = run(rng)
      f(a).run(r1)
    })

    def map[B](f: A => B): State[S, B] = flatMap(a => State.unit(f(a)))

    def map2[B, C](rb: State[S, B])(f: (A, B) => C): State[S, C] = for {
      a <- this
      b <- rb
    } yield f(a, b)

    def map3[B, C, D](rb: State[S, B], rc: State[S, C])(f: (A, B, C) => D): State[S, D] = for {
      a <- this
      b <- rb
      c <- rc
    } yield f(a, b, c)
  }

  object RNGasState {
    type Rand[A] = State[RNG, A]

    val int: Rand[Int] = State(_.nextInt)

    // Probably better with traverse
    def ints(count: Int): Rand[List[Int]] = State.sequence(List.fill(count)(int))

    val nonNegativeInt: Rand[Int] = for {
      next <- int
    } yield if (next == Int.MinValue) 0 else math.abs(next)

    val double: Rand[Double] = for {
      next <- nonNegativeInt
    } yield if (next == Int.MaxValue) 0.0 else (next.toDouble / Int.MaxValue.toDouble)

    val intDouble: Rand[(Int, Double)] = int.map2(double)((_, _))

    val doubleInt: Rand[(Double, Int)] = intDouble.map(_.swap)

    val double3: Rand[(Double, Double, Double)] = double.map3(double, double)((_, _, _))
  }

  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input

  case class Machine(locked: Boolean, candies: Int, coins: Int)

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    def step(s: Machine, i: Input): Machine = (i, s) match {
      case (_, Machine(_, 0, _)) => s
      case (Coin, Machine(false, _, _)) => s
      case (Turn, Machine(true, _, _)) => s
      case (Coin, Machine(true, candies, coins)) => Machine(false, candies, coins + 1)
      case (Turn, Machine(false, candies, coins)) => Machine(true, candies - 1, coins)
    }

    for {
      _ <- State.sequence(inputs.map(i => State.modify[Machine](step(_, i))))
      s <- State.get
    } yield (s.coins, s.candies)
  }

}