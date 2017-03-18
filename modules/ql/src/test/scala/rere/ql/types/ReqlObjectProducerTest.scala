package rere.ql.types

import io.circe.{Json, JsonObject}
import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import rere.ql.queries.values
import rere.ql.shapes.ReqlModel

class ReqlObjectProducerTest extends FlatSpec {

  class Container[T <: ReqlDatum](v: T)

  val objectContainer: Container[ReqlObject] =
    new Container[ReqlObject](values.expr(JsonObject.empty))

  val integerContainer: Container[ReqlInteger] =
    new Container[ReqlInteger](values.expr(123))

  implicit class ContainerOps[T <: ReqlDatum](container: Container[T]) {
    def method[U <: ReqlObject](producer: ReqlObjectProducer[T, U]): ReqlExpr = {
      producer
    }

    def method2[U <: ReqlDatum](producer: ReqlObjectProducer[T, U], i: Int = 1): ReqlExpr = {
      producer
    }
  }

  behavior of "ReqlObjectProducer"

  it should "work with ReqlNull" in {
    val reqlNull: ReqlNull = values.expr(null)

    objectContainer.method(reqlNull)
    objectContainer.method2(reqlNull)

    integerContainer.method(reqlNull)
    integerContainer.method(reqlNull)
  }

  it should "work with JsonObject" in {
    val jsonObject: JsonObject = JsonObject.empty

    objectContainer.method(jsonObject)
    objectContainer.method2(jsonObject)

    integerContainer.method(jsonObject)
    integerContainer.method(jsonObject)
  }

  it should "work with Map of ReqlDatum" in {
    val mapOfDatum: Map[String, ReqlDatum] = Map("abc" -> values.expr(123), "bcd" -> values.expr("def"))

    objectContainer.method(mapOfDatum)
    objectContainer.method2(mapOfDatum)

    integerContainer.method(mapOfDatum)
    integerContainer.method2(mapOfDatum)
  }

  it should "work with subtypes of ReqlObject" in {
    val reqlObject: ReqlObject = values.expr(Map("abc" -> values.expr(123), "bcd" -> values.expr("def")))

    objectContainer.method(reqlObject)
    objectContainer.method2(reqlObject)

    integerContainer.method(reqlObject)
    integerContainer.method2(reqlObject)


    class TestClass
    val reqlModel: ReqlModel[TestClass] = null

    objectContainer.method(reqlModel)
    objectContainer.method2(reqlModel)

    integerContainer.method(reqlModel)
    integerContainer.method2(reqlModel)
  }

  it should "work with functions of Source => Target type" in {
    val reqlObject: ReqlObject = values.expr(JsonObject.empty)

    objectContainer.method({_: ReqlObject => reqlObject})
    objectContainer.method2({_: ReqlObject => reqlObject})

    integerContainer.method({_: ReqlInteger => reqlObject})
    integerContainer.method2({_: ReqlInteger => reqlObject})


    class TestClass
    val reqlModel: ReqlModel[TestClass] = null

    objectContainer.method({_: ReqlObject => reqlModel})
    objectContainer.method2({_: ReqlObject => reqlModel})

    integerContainer.method({_: ReqlInteger => reqlModel})
    integerContainer.method2({_: ReqlInteger => reqlModel})
  }

  it should "not work with not object-like types and not object-producing functions" in {
    val reqlInteger: ReqlInteger = values.expr(123)
    """objectContainer.method(reqlInteger)""".shouldNot(compile)
    """objectContainer.method2(reqlInteger)""".shouldNot(compile)

    """integerContainer.method(reqlInteger)""".shouldNot(compile)
    """integerContainer.method2(reqlInteger)""".shouldNot(compile)


    val reqlJson: ReqlJson = values.expr(Json.fromInt(123))
    """objectContainer.method(reqlJson)""".shouldNot(compile)
    """objectContainer.method2(reqlJson)""".shouldNot(compile)

    """integerContainer.method(reqlJson)""".shouldNot(compile)
    """integerContainer.method2(reqlJson)""".shouldNot(compile)


    val objectToIntegerProducer: ReqlObject => ReqlInteger = _ => reqlInteger
    """objectContainer.method(objectToIntegerProducer)""".shouldNot(compile)
    """objectContainer.method2(objectToIntegerProducer)""".shouldNot(compile)

    """integerContainer.method(objectToIntegerProducer)""".shouldNot(compile)
    """integerContainer.method2(objectToIntegerProducer)""".shouldNot(compile)


    val integerToIntegerProducer: ReqlInteger => ReqlInteger = _ => reqlInteger
    """objectContainer.method(integerToIntegerProducer)""".shouldNot(compile)
    """objectContainer.method2(integerToIntegerProducer)""".shouldNot(compile)

    """integerContainer.method(integerToIntegerProducer)""".shouldNot(compile)
    """integerContainer.method2(integerToIntegerProducer)""".shouldNot(compile)
  }

}
