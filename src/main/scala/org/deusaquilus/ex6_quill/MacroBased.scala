package org.deusaquilus

import scala.quoted._
import scala.deriving._

enum Customer:
  case Person(name: String, lastName: String)
  case Robot(name: String, cpu: String)
  case Yeti(name: String, furColor: String)

object MacroBased {

  enum QueryStatus:
    case Success
    case Failure

  class Database(uri: String) {
    def runQuery(str: String): QueryStatus =
      QueryStatus.Success
  }

  object RunQuery {
    inline def apply[T](inline uri: String): String = ${ RunQueryMacro[T]('uri) }
  }

  object RunQueryMacro {
    import MacroBased.Database
    import MacroBased.QueryStatus

    def nameOf[T: Type](using qctx: Quotes): String =
      import qctx.reflect._
      TypeRepr.of[T].typeSymbol.name

    def constValue[T: Type](using qctx: Quotes): String =
      import qctx.reflect._
      TypeRepr.of[T] match
        case ConstantType(StringConstant(value)) => value.toString


    object GatherFields {
      def collectLabels[Names: Type](using qctx: Quotes): List[String] =
        import qctx.reflect._
        Type.of[Names] match
          case '[name *: names] => constValue[name] +: collectLabels[names]
          case '[EmptyTuple] => Nil
      
      def gatherTypeFields[Types: Type](using qctx: Quotes): List[String] =
        import qctx.reflect._
        Type.of[Types] match
          case '[tpe *: types] => base[tpe] ++ gatherTypeFields[types]
          case '[EmptyTuple] => Nil

      def base[T: Type](using qctx: Quotes): List[String] =
        import qctx.reflect._
        Expr.summon[Mirror.Of[T]] match
          case Some(mir) =>
            mir match
              case '{ $m: Mirror.ProductOf[T] { type MirroredElemLabels = elementLabels; type MirroredElemTypes = elementTypes }} =>
                collectLabels[elementLabels]
              case '{ $m: Mirror.SumOf[T] { type MirroredElemLabels = elementLabels; type MirroredElemTypes = elementTypes }} =>
                gatherTypeFields[elementTypes]
          case _ =>
            report.throwError("Whoops, can't summon a mirror for: " + Type.of[T])
    }

    def generateQuery[T: Type](using qctx: Quotes): String = {
      import qctx.reflect._
      val allFields: List[String] = GatherFields.base[T]
      s"SELECT ${allFields.mkString(", ")} FROM ${nameOf[T]}"
    }

    def apply[T: Type](db: Expr[String])(using qctx: Quotes): Expr[String] = {
        val q: String = generateQuery[T] // Generate the Query
        println("Query Is: " + q)

        // Run it on the DB and tell me if it works
        db match
          case Expr(str: String) =>
            new Database(str).runQuery(q) match
              case QueryStatus.Success => println("Query Succeeded")
              case QueryStatus.Failure => println("Query Failed")
        

        // Return the Generated Query
        Expr(q)

        // (Check!) All while you're compiling...
    }
  }

}