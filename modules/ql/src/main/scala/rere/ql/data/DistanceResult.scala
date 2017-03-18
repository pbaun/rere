package rere.ql.data

case class DistanceResult[T](
  dist: BigDecimal,
  doc: T
)
