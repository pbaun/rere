package rere.ql.data

case class RebalancingResult(
  rebalanced: Long,
  statusChanges: Seq[ChangefeedNotification[TableStatus]]
)
