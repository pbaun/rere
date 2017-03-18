package rere.ql.options

import rere.ql.queries.values

trait WaitOptions {

  sealed trait WaitOptions extends ComposableOptions

  case object ReadyForOutdatedReads extends WaitOptions {
    def isEmpty = false
    def view = "wait_for" -> values.expr("ready_for_outdated_reads") :: Nil
    def innerQuery = query
  }

  case object ReadyForReads extends WaitOptions {
    def isEmpty = false
    def view = "wait_for" -> values.expr("ready_for_reads") :: Nil
    def innerQuery = query
  }

  case object ReadyForWrites extends WaitOptions {
    def isEmpty = false
    def view = "wait_for" -> values.expr("ready_for_writes") :: Nil
    def innerQuery = query
  }

  case object AllReplicasReady extends WaitOptions {
    def isEmpty = false
    def view = "wait_for" -> values.expr("all_replicas_ready") :: Nil
    def innerQuery = query
  }

}
