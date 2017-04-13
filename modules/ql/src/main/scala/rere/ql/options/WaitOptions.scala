package rere.ql.options

import rere.ql.queries.values

trait WaitOptions {

  sealed trait WaitOptions extends ComposableOptions

  case object ReadyForOutdatedReads extends WaitOptions {
    def isEmpty = false
    def view = "wait_for" -> values.expr("ready_for_outdated_reads") :: Nil
    val expr = exprFromView
  }

  case object ReadyForReads extends WaitOptions {
    def isEmpty = false
    def view = "wait_for" -> values.expr("ready_for_reads") :: Nil
    val expr = exprFromView
  }

  case object ReadyForWrites extends WaitOptions {
    def isEmpty = false
    def view = "wait_for" -> values.expr("ready_for_writes") :: Nil
    val expr = exprFromView
  }

  case object AllReplicasReady extends WaitOptions {
    def isEmpty = false
    def view = "wait_for" -> values.expr("all_replicas_ready") :: Nil
    val expr = exprFromView
  }

}
