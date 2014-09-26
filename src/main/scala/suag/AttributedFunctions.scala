package suag

import scala.language.implicitConversions
import scala.language.experimental.macros
import shapeless._
import syntax.singleton._
import record._

class AttributedFunction {
  type Dependencies <: HList
  type Products <: HList

}
object AttributedFunction {
  type Aux[Deps, Prods] = AttributedFunction {
    type Dependencies = Deps
    type Products = Prods
  }
}

class AttributesMacros(val c: scala.reflect.macros.whitebox.Context) {
  import c.universe._
  def mkAttributeImpl(a: c.Tree): c.Tree = {
    val witness = SingletonTypeMacros.convertImpl(c)(c.Expr[Any](a)).tree
    q"""new AttributeOps {
      val witness = $witness
      def @:[T]: KeyTag[witness.T, T] = null
      }"""
  }
}
object AttributesMacros {
  trait AttributeOps {
    val witness: Witness.Aux[_]
    def @:[T]: KeyTag[witness.T, T]
  }

  implicit def mkAttributeOps(a: Any): AttributeOps = macro AttributesMacros.mkAttributeImpl
}