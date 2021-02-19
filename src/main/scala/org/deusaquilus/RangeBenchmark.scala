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


object RangeBenchmarkMacro extends Bench[Double] with BenchBase {
  import Util._
  measure method "Manual" in {
    using(oneGen).config(opts) in { v =>
      val map = mutable.Map[String, Any]()
      PopulateMap.populateMap(p, map) // 0.000096 ms
    }
  }
}

object RangeBenchmarkFunMacro extends Bench[Double] with BenchBase {
  import Util._
  measure method "Manual" in {
    using(oneGen).config(opts) in { v =>
      PopulateMapFun.populateMap(p) // 0.000103 ms
    }
  }
}


object RangeBenchmarkFunctionalJustLoadMap extends Bench[Double] with BenchBase {
  import Util._
  val l = List[(String, Any)]("firstName" -> p.firstName, "lastName" -> p.lastName, "age" -> p.age)

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
      fields.foldLeft(Map[String, Any]()) { (map, field) => 
        map + ((field.getName, field.invoke(p))) // 0.000181
      }
    }
  }
}

object RangeBenchmarkNestedObjectMapManual extends Bench[Double] with BenchBase {
  case class Name(first: String, last: String)
  case class Person(name: Name, age: Int)
  val p = Person(Name("Joe", "Bloggs"), 123)

  measure method "Manual" in {
    using(oneGen).config(opts) in { v =>
    val map = mutable.Map[String, Any]()
    val nameMap = mutable.Map[String, Any]()
    nameMap.put("first", p.name.first)
    nameMap.put("last", p.name.last)
    map.put("name", nameMap)
    map.put("age", p.age)
    } // 0.000156 ms
  }
}


object RangeBenchmarkNestedObjectMap extends Bench[Double] with BenchBase {
  case class Name(first: String, last: String)
  case class Person(name: Name, age: Int)
  val p = Person(Name("Joe", "Bloggs"), 123)

  measure method "Manual" in {
    using(oneGen).config(opts) in { v =>
      val map = mutable.Map[String, Any]()
      PopulateSubMaps.populateMap(p, map)
    }
  } // 0.000157
}


object RangeBenchmarkDerived extends Bench[Double] with BenchBase {
  import Derived._
  import WriteToMapOps._
  case class Person(firstName: String, lastName: String, age: Int) derives WriteToMap
  val p = Person("Joe", "Bloggs", 123)

  measure method "Manual" in {
    using(oneGen).config(opts) in { v =>
      p.writeToMap
    }
  } // 0.000103 ms
}

object RangeBenchmarkDerivedLong extends Bench[Double] with BenchBase {
  import Derived._
  import WriteToMapOps._

  given writeLong: WriteToMap[Long] with
    def writeToMap(map: mutable.Map[String, Any])(key: String, value: Long): Unit = map.put(key, value)

  case class Person(firstName: String, lastName: String, age: Long) derives WriteToMap
  val q = Person("Q", "Q", 4000000000L)

  measure method "Manual" in {
    using(oneGen).config(opts) in { v =>
      q.writeToMap
    }
  } // 0.000104 ms
}

object RangeBenchmarkMirrorSummon extends Bench[Double] with BenchBase {
  import DerivedMirrorSummon._
  import WriteToMapOps._

  inline given writeArbitraryToMap[T]: WriteToMap[T] = WriteToMap.derived
  case class Person(firstName: String, lastName: String, age: Int)
  val p = Person("Joe", "Bloggs", 123)

  measure method "Manual" in {
    using(oneGen).config(opts) in { v =>
      
      p.writeToMap
    }
  } // 
}

object RangeBenchmarkDerivedNaive extends Bench[Double] with BenchBase {
  import DerivedNaive._
  import WriteToMapOps._
  case class Person(firstName: String, lastName: String, age: Int) derives WriteToMap
  val p = Person("Joe", "Bloggs", 123)

  measure method "Manual" in {
    using(oneGen).config(opts) in { v =>
      p.writeToMap
    }
  } // 0.000214
}


object RangeBenchmarkJustLoadTest {
  def main(args: Array[String]): Unit = {
    val l = List[(String, Any)]("firstName" -> "Joe", "lastName" -> "Bloggs", "age" -> 123)
    println( l.foldLeft(Map[String, Any]())((map, pair) => map + pair) )
  }
}
