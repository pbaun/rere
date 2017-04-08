package rere.ql.queries

import akka.util.ByteString
import org.scalatest.FlatSpec
import rere.ql.types.{ReqlBinary, ReqlDatum, ReqlPseudo}

class BinaryQueriesTest extends FlatSpec with ReqlMatchers {

  import rere.ql.queries.all.r
  import rere.ql.queries.binary._

  behavior of "BinaryQueries"

  it should "provide .binary operator on r" in {
    r.binary(ByteString(10, 20, 30, 40)) shouldBe subtypeOf [ReqlBinary]
    r.binary(ByteString(10, 20, 30, 40)) shouldBe subtypeOf [ReqlPseudo]
    r.binary(ByteString(10, 20, 30, 40)) shouldBe subtypeOf [ReqlDatum]
    r.binary(ByteString(10, 20, 30, 40)) shouldBe
      subtypeOf [ReqlBinary] and serializedTo("""{"$reql_type$":"BINARY","data":"ChQeKA=="}""")
  }

}
