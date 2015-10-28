package reification

import reflect.runtime.universe._
import slick.driver.H2Driver.api._
import scala.concurrent.ExecutionContext.Implicits.global
import benchmark._

import org.scalameter.api._
import org.scalameter.reporting._

trait Exp
case class IR2(x: Int) extends Exp
case class IR1(x: Exp, y: Exp) extends Exp
class Box(i: Int)

object ReificationBenchmark extends PerformanceTest.OfflineReport with TypecheckingBenchmarkingSuite {
  // Definition of the SUPPLIERS table
class Suppliers(tag: Tag) extends Table[(Int, String, String, String, String, String)](tag, "SUPPLIERS") {
  def id = column[Int]("SUP_ID", O.PrimaryKey) // This is the primary key column
  def name = column[String]("SUP_NAME")
  def street = column[String]("STREET")
  def city = column[String]("CITY")
  def state = column[String]("STATE")
  def zip = column[String]("ZIP")
  // Every table needs a * projection with the same type as the table's type parameter
  def * = (id, name, street, city, state, zip)
}

  val suppliers = TableQuery[Suppliers]

// Definition of the COFFEES table
class Coffees(tag: Tag) extends Table[(String, Int, Double, Int, Int)](tag, "COFFEES") {
  def name = column[String]("COF_NAME", O.PrimaryKey)
  def supID = column[Int]("SUP_ID")
  def price = column[Double]("PRICE")
  def sales = column[Int]("SALES")
  def total = column[Int]("TOTAL")
  def * = (name, supID, price, sales, total)
  // A reified foreign key relation that can be navigated to create a join
  def supplier = foreignKey("SUP_FK", supID, suppliers)(_.id)
}


  val fieldIndexes = Gen.range("LoC")(0, 100, 4)
  def generate(n: Int, res: Boolean): String = {
    var valNameBase = "coffees"
    def variable(i: Int): String = valNameBase + i

    val prog = """
      import slick.driver.H2Driver.api._
      val coffees0 = TableQuery[reification.ReificationBenchmark.Coffees]
      """

    val prog1 = (0 until n).foldLeft(prog) {(prog, i) =>
      if (i == 0) {
        prog + "\n" + s"""val ${variable(i + 1)} = ${variable(i)}.map(x => x.price)"""
      } else {
        prog + "\n" + s"""val ${variable(i + 1)} = ${variable(i)}.map(x => (x - 1.0) * 1.0) """
      }
    }

    // return a dummy result
    if (res) prog1 + "\n" + variable(n)
    else prog1 +  "\n1"
  }

  def generateCaseClasses(n: Int, res: Boolean): String = {
    var valNameBase = "coffees"
    def variable(i: Int): String = valNameBase + i

    val prog = s"""
      val ${variable(0)} = IR2(1)
      """

    val prog1 = (0 until n).foldLeft(prog) {(prog, i) =>
        prog + "\n" + s"""val ${variable(i + 1)} = IR1(IR2(1), IR1(${variable(i)}, IR1(${variable(i)}, IR2(1))))"""
    }

    // return a dummy result
    if (res) prog1 + "\n" + variable(n)
    else prog1 +  "\n1"
  }

  var func: () => Unit = () => IR2(1)

    /*measure method s"simple reification" in {
      using(fieldIndexes)
        .setUp(v => {

          setupCompiler()

          val className = "SimpleReification" + v
          val src = s"""
          |  import reification._
          |  class $className() extends Function0[Unit] {
          |    def apply(): Unit = {
          """.stripMargin +
          generate(v, true) + """
          |    }
          |  }""".stripMargin
          func = compileAndLoadSource(src, className)
        })
        .in { x =>
          val x = func()
          x
         }
       }*/

        measure method s"simplest reification" in {
      using(fieldIndexes)
        .setUp(v => {

          setupCompiler()

          val className = "SimpleReification" + v
          val src = s"""
          |  import reification._
          |  class $className() extends Function0[Unit] {
          |    def apply(): Unit = {
          """.stripMargin +
          generateCaseClasses(v, true) +
          """
          |    }
          |  }""".stripMargin
          func = compileAndLoadSource(src, className)
        })
        .in { x =>
          func()
        }
       }
}
