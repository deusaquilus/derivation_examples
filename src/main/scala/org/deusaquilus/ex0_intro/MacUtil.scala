package org.deusaquilus

import scala.quoted._
import scala.collection.mutable

object PopulateSubMaps {
inline def populateMap[T](from: T, map: mutable.Map[String, Any]): Unit = 
  ${ populateMapImpl('from, 'map) }

inline def recurseGetMap[T](from: T): mutable.Map[String, Any] =
  val map = mutable.Map[String, Any]()
  populateMap(from, map)
  map

def populateMapImpl[T: Type](from: Expr[T], map: Expr[mutable.Map[String, Any]])(using qctx: Quotes): Expr[Unit] =
  import qctx.reflect._
  val cls = TypeRepr.of[T].classSymbol.get
  val selects = 
    cls.caseFields.map { f =>
      val field = from.asTerm.select(f)
      val value =
        // if you do just this: from.asTerm.select(f).asExpr inner maps will be empty because they don't know what type to infer on
        if (field.tpe <:< TypeRepr.of[Product])
          field.tpe.asType match
            case '[tpe] => 
              '{ recurseGetMap[tpe](${from.asTerm.select(f).asExprOf[tpe]}) }
        else
          from.asTerm.select(f).asExpr

      val key = Expr(f.name)
      '{ $map.put($key, $value) }
    }
  Expr.block(selects, '{ () })
}


object PopulateMap {
  inline def populateMap[T](from: T, map: mutable.Map[String, Any]): Unit = 
    ${ populateMapImpl('from, 'map) }

  def populateMapImpl[T: Type](from: Expr[T], map: Expr[mutable.Map[String, Any]])(using qctx: Quotes): Expr[Unit] = {
    import qctx.reflect._
    val cls = TypeRepr.of[T].classSymbol.get
    val selects = 
      cls.caseFields.map { f =>
        val value = from.asTerm.select(f).asExpr
        val key = Expr(f.name)
        '{ $map.put($key, $value) }
      }
    Expr.block(selects, '{ () })
  }
}


object PopulateMapFun {
inline def populateMap[T](from: T): scala.collection.MapView[String, Any] =
  val myMap = mutable.Map[String, Any]()
  populateMapBlock(from, myMap)
  myMap.view

inline def populateMapBlock[T](from: T, map: mutable.Map[String, Any]): Unit = 
  ${ populateMapImpl('from, 'map) }

def populateMapImpl[T: Type](
  from: Expr[T], 
  map: Expr[mutable.Map[String, Any]]
)(using qctx: Quotes): Expr[Unit] =
  import qctx.reflect._
  val cls = TypeRepr.of[T].classSymbol.get
  val selects = 
    cls.caseFields.map { f =>
      val value = from.asTerm.select(f).asExpr
      val key = Expr(f.name)
      '{ $map.put($key, $value) }
    }
  Expr.block(selects, '{ () })
}
