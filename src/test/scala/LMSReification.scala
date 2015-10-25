package reification

import org.scalatest._
import reflect.runtime.universe._

class GenericTranslationSpec extends FlatSpec with ShouldMatchers {
  "LMS " should "be able to reify externally" in {
    val dsl = new DSL
    import dsl._
    val sym: dsl.Rep[Int] = dsl.fresh[Int]
    val res = __ifThenElse(sym < dsl.unit(0), sym, sym + 1)
    val res1 = __ifThenElse(res < dsl.unit(0), res, res + 1)
  }

   val expr = reify {
    val dsl = new DSL
    import dsl._
    val sym: dsl.Rep[Int] = dsl.fresh[Int]
    val res = __ifThenElse(sym < dsl.unit(0), sym, sym + 1)
    val res1 = __ifThenElse(res < dsl.unit(0), res, res + 1)
    1
  }
  import scala.tools.reflect.ToolBox
  val tb = scala.reflect.runtime.universe.runtimeMirror(
  getClass.getClassLoader).mkToolBox()
  tb.eval(expr.tree)
}
