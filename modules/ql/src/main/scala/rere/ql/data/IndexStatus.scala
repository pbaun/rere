package rere.ql.data

case class IndexStatus(
  index: String,
  ready: Boolean,
  progress: Option[BigDecimal],
  function: IndexFunctionBinary,
  multi: Boolean,
  geo: Boolean,
  outdated: Boolean
)
