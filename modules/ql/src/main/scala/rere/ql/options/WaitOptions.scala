package rere.ql.options

import rere.ql.queries.values

trait WaitOptions {

  sealed trait WaitOptions extends ComposableOptions

  case object ReadyForOutdatedReads extends WaitOptions with NonDefaultOption {
    def view = "wait_for" -> values.expr("ready_for_outdated_reads") :: Nil
  }

  case object ReadyForReads extends WaitOptions with NonDefaultOption {
    def view = "wait_for" -> values.expr("ready_for_reads") :: Nil
  }

  case object ReadyForWrites extends WaitOptions with NonDefaultOption {
    def view = "wait_for" -> values.expr("ready_for_writes") :: Nil
  }

  case object AllReplicasReady extends WaitOptions with NonDefaultOption {
    def view = "wait_for" -> values.expr("all_replicas_ready") :: Nil
  }

}
