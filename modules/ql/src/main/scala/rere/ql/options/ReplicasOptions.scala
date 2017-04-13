package rere.ql.options

import rere.ql.queries.values
import rere.ql.values.ReqlMakeObjFromPairsListQuery

trait ReplicasOptions { this: Classes =>

  sealed trait ReplicasOptions extends ComposableOptions

  case object SingleReplica extends ReplicasOptions {
    def isEmpty = true
    def view = Nil
    val expr = exprFromView
  }

  case class Replicas(amount: Int) extends ReplicasOptions {
    def isEmpty = false
    def view = "replicas" -> values.expr(amount) :: Nil
    def expr = exprFromView

    def allVoting(): VotingReplicasOptions = {
      NonVotingReplicas(this.view, Nil)
    }
  }

  //TODO: not allow empty pairs - at least they should contain primary
  case class ReplicasByTags(primary: ServerTag, pair: (ServerTag, Int), pairs: (ServerTag, Int)*) extends ReplicasOptions {
    def isEmpty = false
    def view = {
      "replicas" -> new ReqlMakeObjFromPairsListQuery((pair :: pairs.toList).map {
        case (serverTag, amount) => serverTag.tag -> values.expr(amount)
      }) ::
      "primary_replica_tag" -> values.expr(primary.tag) ::
      Nil
    }
    def expr = exprFromView

    def nonvoting(tags: ServerTag*): VotingReplicasOptions = {
      NonVotingReplicas(this.view, tags)
    }

    def allVoting(): VotingReplicasOptions = {
      NonVotingReplicas(this.view, Nil)
    }
  }

  sealed trait VotingReplicasOptions extends ComposableOptions

  //use ReplicasByTags(...).nonvoting(...) for safe construction
  private case class NonVotingReplicas(replicasConfigView: ComposableOptions.View, tags: Seq[ServerTag]) extends VotingReplicasOptions {
    def isEmpty = false
    def view = {
      if (tags.nonEmpty) {
        val replicaTags = tags.map(serverTag => values.expr(serverTag.tag))
        "nonvoting_replica_tags" -> values.expr(replicaTags) :: replicasConfigView //order already broken
      } else replicasConfigView
    }
    def expr = exprFromView
  }

}
