package org.deusaquilus

import org.scalameter.api._
import org.scalameter.picklers.Implicits._
import scala.collection.mutable

trait BenchBase {
  lazy val executor = LocalExecutor(
    new Executor.Warmer.Default,
    Aggregator.median[Double],
    measurer
  )
  lazy val measurer = new Measurer.Default
  lazy val reporter = new LoggingReporter[Double]
  lazy val persistor = Persistor.None

  val oneGen = Gen.unit("gen-time")

  /* inputs */

  val sizes = Gen.range("size")(300000, 1500000, 300000)

  val ranges = for {
    size <- sizes
  } yield 0 until size

  import Util._
  val p = Person("Joe", "Bloggs", 123)
  val fieldsPre = reflectCaseClassFields(p)

  val opts = 
    new org.scalameter.japi.ContextBuilder()
      .put("exec.minWarmupRuns", 500)
      .put("exec.maxWarmupRuns", 1000)
      .put("exec.benchRuns", 400000)
      .build()
}

object RangeBenchmarkA extends Bench[Double] with BenchBase {
  import Util._

  measure method "Using Reflection No Fields" in {
    var d: Double = 0
    using(oneGen).config(opts) in { v =>
      val map = mutable.Map[String, Any]()
      for (field <- fieldsPre) {
        map.put(field.getName,  field.invoke(p)) // 0.000123
      }
      d = d + map.size.toDouble
    }
  }
}

object RangeBenchmarkB extends Bench[Double] with BenchBase {
  import Util._

  measure method "Using Reflection" in {
    using(oneGen).config(opts) in { v =>
      val fields = reflectCaseClassFields(p)
      val map = mutable.Map[String, Any]()
      for (field <- fieldsPre) {
        map.put(field.getName,  field.invoke(p)) // 0.001169 ms
      }
    }
  }
}

object RangeBenchmarkReflectFields extends Bench[Double] with BenchBase {
  import Util._

  measure method "Using Reflection" in {
    using(oneGen).config(opts) in { v =>
      reflectCaseClassFields(p) // 0.001141 ms
    }
  }
}

object RangeBenchmarkC extends Bench[Double] with BenchBase {
  import Util._
  measure method "Manual" in {
    using(oneGen).config(opts) in { v =>
      val map = mutable.Map[String, Any]()
      map.put("firstName",  p.firstName)
      map.put("lastName",  p.lastName)
      map.put("age",  p.age)
      // 0.000096 ms
    }
  }
}


object RangeBenchmarkD extends Bench[Double] with BenchBase {
  import Util._
  measure method "Manual" in {
    using(oneGen).config(opts) in { v =>
      val map = mutable.Map[String, Any]()
      MacUtil.populateMap(p, map) // 0.000096 ms
    }
  }
}


object RangeBenchmarkFunctionalJustLoadMap extends Bench[Double] with BenchBase {
  import Util._
  val l = List[(String, Any)]("firstName" -> "Joe", "lastName" -> "Bloggs", "age" -> 123)

  measure method "Manual" in {
    using(oneGen).config(opts) in { v =>
      l.foldLeft(Map[String, Any]())((map, pair) => map + pair) // 0.000161 ms
    }
  }
}

object RangeBenchmarkFunctionalWithReflect extends Bench[Double] with BenchBase {
  import Util._
  val fields = reflectCaseClassFields(p)

  measure method "Manual" in {
    using(oneGen).config(opts) in { v =>
      fields.foldLeft(Map[String, Any]())(
        (map, field) => map + ((field.getName, field.invoke(p))) // 0.000181
      )
    }
  }
}

object RangeBenchmarkJustLoadTest {
  def main(args: Array[String]): Unit = {
    val l = List[(String, Any)]("firstName" -> "Joe", "lastName" -> "Bloggs", "age" -> 123)
    println( l.foldLeft(Map[String, Any]())((map, pair) => map + pair) )
  }
}
