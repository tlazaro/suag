package suag

import org.scalacheck._
import Parsers._

object ParserSpecs extends Properties("Parsers") {
  import org.scalacheck.Prop.{forAll, BooleanOperators, AnyOperators}

  property("pSym wraps a single element") = forAll { (a: Char) =>
    val p = pSym(a)

    ("sym must match" |: p(Stream(a)) =? Stream((a, Stream.empty))) &&
      ("sym must not match on empty stream" |: p(Stream()) =? Stream.empty)
  }

  property("pSym doesn't match other input") = forAll { (a: Char, b: Char) =>
    val p = pSym(a)

    ("sym must match" |: p(Stream(a)) =? Stream((a, Stream.empty))) &&
      ("if a != b it must not match" |: (a == b || p(Stream(b)) =? Stream.empty))
  }

  property("<|> operator works for single symbols") = forAll { (a: Char, b: Char) =>
    val p = pSym(a)
    val q = pSym(b)

    val s = p <|> q

    ("res s(a) is a" |: s(Stream(a)) =? Stream((a, Stream.empty))) &&
    ("res s(b) is b" |: s(Stream(b)) =? Stream((b, Stream.empty)))
  }

  property("<*> combines two symbols") = forAll { (a: Char, b: Char) =>

    def pair[A, B] = (a: A) => (a, _: B)
    def comb[S, A, B]: Parser[S, A => B => (A, B)] = pSucceed(pair)

    val p = pSym(a)
    val q = pSym(b)

    val s = comb <*> p <*> q
    val t = pair <&> p <*> q

    ("res s(a) is a" |: s(Stream(a, b)) =? Stream(((a, b), Stream.empty))) &&
      ("<&> threads functions through <*>" |: s(Stream(a, b)) =? t(Stream(a, b)))
  }

  property("zeroOrMore matches zero or more elements") = forAll(Gen.choose(0,40), Gen.alphaNumChar) { (n: Int, a: Char) =>
    val p = pSym(a)
    val s = p.zeroOrMore

    val stream = ((a + "") * n).toStream.force

    // I think there must be some construction that describes this already
    // Sequences: [matches all elements, leaving none unmatched, matches 1 less element leaves 1 unmatched,... matches 0 elements leaving all unmatched]
    def matches[S](inp: Stream[S]): Stream[(Stream[S], Stream[S])] = {
      def generate[S](inp: Stream[S], m: Int, n: Int): Stream[(Stream[S], Stream[S])] = n match {
        case x if m >= 0 && n >= 0 => Stream((inp.take(m), inp.takeRight(n))) #::: generate(inp, m - 1, n + 1)
        case _ => Stream.empty
      }

      generate(inp, stream.size, 0)
    }

    ("zeroOrMore generates all possible captures of zero or more elements" |: s(stream).force =? matches(stream).force)
  }

  case class SemIfStat(cond: String, ifpart: String, thenpart: String)

  def pIfStatPrelude[T]: T => String => T => String => T => String => T => SemIfStat =
    _ => c => _ => t => _ => e => _ => SemIfStat(c, t, e)

  val pIfToken = pSym("IF")
  val pThenToken = pSym("THEN")
  val pElseToken = pSym("ELSE")
  val pFiToken = pSym("FI")

  def pExpr[A] = pAny[A]

  val pIfStat = pIfStatPrelude <&>
    pIfToken <*> pExpr <*>
    pThenToken <*> pExpr <*>
    pElseToken <*> pExpr <*>
    pFiToken

  property("Using functions to capture values with <&> and <*> works") = forAll { (x: String, y: String, z: String) =>
    val ifTest = pIfStat(Stream("IF", x, "THEN", y, "ELSE", z, "FI"))

    ("<&> works for curried functions" |: ifTest =? Stream((SemIfStat(x, y, z), Stream.empty)))
  }

  case class SemIfStat2(cond: String, ifpart: String, thenpart: String)

  val pIfStat2 = SemIfStat2.curried <&
    pIfToken <*> pExpr <*
    pThenToken <*> pExpr <*
    pElseToken <*> pExpr <*
    pFiToken

  property("Dropping captures with <* works") = forAll { (x: String, y: String, z: String) =>
    val ifTest = pIfStat2(Stream("IF", x, "THEN", y, "ELSE", z, "FI"))

    ("<&> works for curried functions" |: ifTest.force =? Stream((SemIfStat2(x, y, z), Stream.empty)))
  }

  property("Take function value upong application") = forAll(Gen.choose(0,40)) { (n: Int) =>
    // Count: S -> (S)
    def count[S]: S => Int = _ => n
    val pPPP = count <&> pSym('(')

    ("Counts the depth of parens" |: pPPP("(".toStream).force =? Stream((n, Stream.empty)))
  }

  // TODO generate valid Strings to count
  property("Can count nested parenthesis") = forAll { (n: Int) =>
    // Balanced nested parenthesis
    // S -> (S) S | ·
    def pP: Parser[Char, Int] = ((math.max _).curried compose (1 + (_: Int))) <& pSym('(') <*> pP <* pSym(')') <*> pP opt 0

    val pPTest = pP("()".toStream)

    ("Counts the depth of parens" |: pPTest.force =? Stream((1, Stream.empty), (0, "()".toStream)))
  }
}
