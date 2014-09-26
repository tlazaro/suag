package suag

import shapeless._, syntax.singleton._, record._
import Attributes._

class AttributedFunctionsTest {

  val s = "length".@:[Int]
  
  implicitly[AttrList[s.type :: s.type :: HNil]]
}