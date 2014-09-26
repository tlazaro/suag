package suag

import scala.language.implicitConversions
import scala.language.experimental.macros
import shapeless._
import syntax.singleton._
import record._

sealed trait AttributedFunction {
  type Dependencies <: HList
  type Products <: HList

  def apply[Ctx <: HList: AttrList](ctx: Ctx): Ctx
}
object AttributedFunction {
  type Aux[Deps, Prods] = AttributedFunction {
    type Dependencies = Deps
    type Products = Prods
  }
  
  def apply[Deps <: HList: AttrList, Prods <: HList: AttrList](deps: Deps): Aux[Deps, Prods] = ???
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
  def apply(key: Witness) = ???
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