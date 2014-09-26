package suag

import scala.language.{ implicitConversions, higherKinds }
import scala.language.experimental.macros
import shapeless._
import syntax.singleton._
import record._

sealed trait AttributedFunction {
  type Dependencies <: HList
  type Products <: HList
  def apply(ctx: Context[Dependencies]): Context[Products]
}
object AttributedFunction {
  type Aux[Deps, Prods] = AttributedFunction {
    type Dependencies = Deps
    type Products = Prods
  }

  class Applier[Deps <: HList, AttrValues <: HList] {
    def define[Prods <: HList: AttrValueList](f: Context[AttrValues] => Prods): Aux[Deps, Prods] = ???
  }
  
  def apply[Deps <: HList: AttrList, AttrValues <: HList](deps: Deps)(
    implicit attributedDeps: ops.hlist.Mapper.Aux[attr2attrValue.type, Deps, AttrValues]/*,
    attributedProds: ops.hlist.Mapper[attr2attrValue.type, Prods]*/) = new Applier[Deps, AttrValues]
}

trait AttrList[H <: HList]
object AttrList {
  implicit def emptyListIsContext: AttrList[HNil] = null
  implicit def listIsContext[KT <: Attribute[_, _], HL <: HList](implicit tail: AttrList[HL]): AttrList[KT :: HL] = null
}
trait AttrValueList[H <: HList]
object AttrValueList {
  implicit def emptyListIsContext: AttrValueList[HNil] = null
  implicit def listIsContext[KT <: AttributedValue[_, _], HL <: HList](implicit tail: AttrValueList[HL]): AttrValueList[KT :: HL] = null
}

case class Context[H <: HList: AttrValueList](values: H) {
  def apply[Out](key: Witness)(implicit selector: Context.ValueSelector[key.T, H, Out]) = selector(values)
}
object Context {
  trait ValueSelector[Key, H <: HList, Out] {
    def apply(c: H): Out
  }
  implicit def headSelect[Key, Out, A[K, V] <: AttributedValue[K, V], Tail <: HList]: ValueSelector[Key, A[Key, Out] :: Tail, Out] = head.asInstanceOf[ValueSelector[Key, A[Key, Out] :: Tail, Out]]
  implicit def tailSelect[Key, Out, H, Tail <: HList](implicit tailSel: ValueSelector[Key, Tail, Out]): ValueSelector[Key, H :: Tail, Out] = new ValueSelector[Key, H :: Tail, Out] {
    def apply(c) = tailSel(c.tail)
  }

  private val head = new ValueSelector[Any, _ :: HList, Any] {
    def apply(c) = c.asInstanceOf[Any :: HList].head.asInstanceOf[AttributedValue[_, _]].value
  }
}

class AttributesMacros(val c: scala.reflect.macros.whitebox.Context) {
  import c.universe._
  def mkAttributeImpl(a: c.Tree): c.Tree = {
    val witness = SingletonTypeMacros.convertImpl(c)(c.Expr[Any](a)).tree
    //    println(s"Witness: $witness")
    val witnessName = TermName(c.freshName("witness"))
    val res = q"""
      val $witnessName = $witness
      new _root_.suag.Attributes.AttributeDefinition[$witnessName.T] {
      import shapeless._, syntax.singleton._
      val witness = $witnessName
      override def @:[T]: Attribute[$witnessName.T, T] = null
      }"""
    res
  }
}

sealed trait Attribute[Key, Value]
case class AttributedValue[Key, Value](value: Value) extends Attribute[Key, Value]

object attr2attrValue extends Poly1 {
  implicit def attr[K, V]: Case[suag.Attribute[K, V]] { type Result = suag.AttributedValue[K, V] } = null
}

object Attributes {
  trait AttributeDefinition[K] {
    val witness: Witness.Aux[K]
    def @:[T]: Attribute[K, T] = null
  }

  implicit def mkAttributeOps(a: Any): AttributeDefinition[_] = macro AttributesMacros.mkAttributeImpl

  implicit class AttributeOps[Key, Value](val attr: Attribute[Key, Value]) extends AnyVal {
    def :=(v: Value) = AttributedValue[Key, Value](v)
  }
}