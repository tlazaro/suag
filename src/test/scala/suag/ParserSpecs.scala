package suag

import org.scalacheck._
import Parsers._

object ParserSpecs extends Properties("Parsers") {
  import org.scalacheck.Prop.{forAll, BooleanOperators, AnyOperators}

  property("pSym wraps a single element") = forAll { (a: Char) =>
    val p = pSym(a)

    ("sym must match" |: p(Stream(a)) =? Stream((a, Stream.Empty))) &&
      ("sym must not match on empty stream" |: p(Stream()) =? Stream.Empty)
  }

  property("pSym doesn't match other input") = forAll { (a: Char, b: Char) =>
    val p = pSym(a)

    ("sym must match" |: p(Stream(a)) =? Stream((a, Stream.Empty))) &&
      ("if a != b it must not match" |: (a == b || p(Stream(b)) =? Stream.Empty))
  }

  property("<|> operator works for single symbols") = forAll { (a: Char, b: Char) =>
    val p = pSym(a)
    val q = pSym(b)

    val s = p <|> q

    ("res s(a) is a" |: s(Stream(a)) =? Stream((a, Stream.Empty))) &&
    ("res s(b) is b" |: s(Stream(b)) =? Stream((b, Stream.Empty)))
  }

  property("<*> combines two symbols") = forAll { (a: Char, b: Char) =>

    def pair[A, B] = (a: A) => (a, _: B)
    def comb[S, A, B]: Parser[S, A => B => (A, B)] = pSucceed(pair)

    val p = pSym(a)
    val q = pSym(b)

    val s = comb <*> p <*> q
    val t = pair <&> p <*> q

    ("res s(a) is a" |: s(Stream(a, b)) =? Stream(((a, b), Stream.Empty))) &&
      ("<&> threads functions through <*>" |: s(Stream(a, b)) =? t(Stream(a, b)))
  }

  val smallEvenInteger = Gen.choose(0,40)

  property("zeroOrMore matches zero or more elements") = forAll(Gen.choose(0,40), Gen.alphaNumChar) { (n: Int, a: Char) =>
    val p = pSym(a)
    val s = p.zeroOrMore

    val stream = ((a + "") * n).toStream.force

    // I think there must be some construction that describes this already
    // Sequences: [matches all elements, leaving none unmatched, matches 1 less element leaves 1 unmatched,... matches 0 elements leaving all unmatched]
    def matches[S](inp: Stream[S]): Stream[(Stream[S], Stream[S])] = {
      def generate[S](inp: Stream[S], m: Int, n: Int): Stream[(Stream[S], Stream[S])] = n match {
        case x if m >= 0 && n >= 0 => Stream((inp.take(m), inp.takeRight(n))) #::: generate(inp, m - 1, n + 1)
        case _ => Stream.Empty
      }

      generate(inp, stream.size, 0)
    }

    ("zeroOrMore generates all possible captures of zero or more elements" |: s(stream).force =? matches(stream).force)
  }
}
