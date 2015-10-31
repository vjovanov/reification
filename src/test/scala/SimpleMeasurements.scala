package reification

import reflect.runtime.universe._
import slick.driver.H2Driver.api._
import scala.concurrent.ExecutionContext.Implicits.global
import benchmark._

import org.scalameter.api._
import org.scalameter.reporting._

object SimpleBenchmark extends PerformanceTest.OfflineReport with TypecheckingBenchmarkingSuite {

  val sortIndexes = Gen.range("Sort")(0, 1000, 50)
  val copyIndexes = Gen.range("Copy")(0, 200000, 1000)

  var arr: Array[Int] = Array()
  var newArray: Array[Int] = null
  measure method s"sorting reification" in {
      using(sortIndexes)
        .setUp{v =>
          arr = Array.fill(v)(42)
          arr = arr.map(_ => new util.Random().nextInt)
        }
        .in { x =>
          var i = 0
          do {
            arr.sorted
            i = i + 1
          } while (i < 1000)
        }
       }

    measure method s"simplest reification" in {
      using(copyIndexes)
        .setUp{v =>
          arr = Array.fill(v)(42)
          newArray = Array.fill(v)(42)
          arr = arr.map(_ => new util.Random().nextInt)
        }
        .in { x =>
          var i = 0

          do {
            Array.copy(arr, 0, newArray, 0, x)
            i = i + 1
          } while (i < 1000)
        }
       }
}

