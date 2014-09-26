package suag

import shapeless._, syntax.singleton._, record._
import Attributes._

class AttributedFunctionsTest {

  val width = "width".@:[Double]
  val height = "height".@:[Double]
  val name = "name".@:[String]
  val context = Context((width := 100) :: (height := 200) :: (name := "robert") :: HNil)

  context("width")
  context("height")
  context("name")
  test.illTyped("""context("doesNotCompile")""")
  
  val pf = AttributedFunction(width :: height :: name :: HNil).define { ctx =>
    ctx("width")
    (name := "naaame") :: HNil
  }

}