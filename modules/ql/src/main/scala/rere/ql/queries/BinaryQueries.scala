package rere.ql.queries

import akka.util.ByteString
import rere.ql.types.{ReqlBinary, ReqlR}
import rere.ql.values.ReqlBinaryQuery

trait BinaryQueries {

  implicit class BinaryOp(r: ReqlR) {
    def binary(binary: ByteString): ReqlBinary = new ReqlBinaryQuery(binary)
  }

}
