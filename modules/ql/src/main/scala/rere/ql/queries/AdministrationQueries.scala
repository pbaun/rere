package rere.ql.queries

import java.util.UUID

import rere.ql.options.all._
import rere.ql.options.{ComposableOptions, Options}
import rere.ql.ql2.Term.TermType
import rere.ql.types._

trait AdministrationQueries {

  // grant
  trait GrantRQuery extends ReqlGrantingResult
  trait GrantDbQuery extends ReqlGrantingResult
  trait GrantTableQuery extends ReqlGrantingResult

  //TODO: support for {write: null} (see .grant docs)
  //TODO: allow {connect: true} only on global level
  //ReqlOpFailedError: The `connect` permission is only valid at the global scope
  // .grant without permissions for read current permissions? No, right way is r.db('rethinkdb').table("permissions")
  implicit class GrantOnROp(val r: ReqlR) {
    def grant(username: ReqlString,
              permissions: ReqlUserPermissions): GrantRQuery = new GrantRQuery {
      val command = TermType.GRANT
      val string = "grant"
      val arguments = username :: permissions :: Nil
      val options = Options.empty
    }
  }

  implicit class GrantOnDbOp(val db: ReqlDatabase) {
    def grant(username: ReqlString,
              permissions: ReqlUserPermissions): GrantDbQuery = new GrantDbQuery {
      val command = TermType.GRANT
      val string = "grant"
      val arguments = db :: username :: permissions :: Nil
      val options = Options.empty
    }
  }

  implicit class GrantOnTableOp[T <: ReqlObject, PK](val table: ReqlTable[T, PK]) {
    def grant(username: ReqlString,
              permissions: ReqlUserPermissions): GrantTableQuery = new GrantTableQuery {
      val command = TermType.GRANT
      val string = "grant"
      val arguments = table :: username :: permissions :: Nil
      val options = Options.empty
    }
  }

  // config
  trait ConfigTableQuery extends ReqlSelectionOfObject[ReqlTableConfigResult, UUID]
  trait ConfigDbQuery extends ReqlSelectionOfObject[ReqlDatabaseConfigResult, UUID]

  implicit class ConfigOnTableOp[T <: ReqlObject, PK](val table: ReqlTable[T, PK]) {
    def config(): ConfigTableQuery = new ConfigTableQuery {
      val command = TermType.CONFIG
      val string = "config"
      val arguments = table :: Nil
      val options = Options.empty
    }
  }

  implicit class ConfigOnDbOp(val db: ReqlDatabase) {
    def config(): ConfigDbQuery = new ConfigDbQuery {
      val command = TermType.CONFIG
      val string = "config"
      val arguments = db :: Nil
      val options = Options.empty
    }
  }

  // rebalance
  trait RebalanceTableQuery extends ReqlRebalancingResult
  trait RebalanceDbQuery extends ReqlRebalancingResult

  implicit class RebalanceOnTableOp[T <: ReqlObject, PK](val table: ReqlTable[T, PK]) {
    def rebalance(): RebalanceTableQuery = new RebalanceTableQuery {
      val command = TermType.REBALANCE
      val string = "rebalance"
      val arguments = table :: Nil
      val options = Options.empty
    }
  }

  implicit class RebalanceOnDbOp(val db: ReqlDatabase) {
    def rebalance(): RebalanceDbQuery = new RebalanceDbQuery {
      val command = TermType.REBALANCE
      val string = "rebalance"
      val arguments = db :: Nil
      val options = Options.empty
    }
  }

  // reconfigure
  trait ReconfigureTableQuery extends ReqlReconfiguringResult
  trait ReconfigureDryTableQuery extends ReqlReconfiguringDryResult
  trait ReconfigureDbQuery extends ReqlReconfiguringResult
  trait ReconfigureDryDbQuery extends ReqlReconfiguringDryResult

  implicit class ReconfigureOnTableOp[T <: ReqlObject, PK](val table: ReqlTable[T, PK]) {
    def reconfigure(shards: ShardsOptions,
                    replicas: VotingReplicasOptions,
                    dryRun: RealRun.type): ReconfigureTableQuery = new ReconfigureTableQuery {
      val command = TermType.RECONFIGURE
      val string = "reconfigure"
      val arguments = table :: Nil
      val options = ComposableOptions.compose(shards, replicas, dryRun)
    }

    def reconfigure(shards: ShardsOptions,
                    replicas: VotingReplicasOptions,
                    dryRun: DryRun.type): ReconfigureDryTableQuery = new ReconfigureDryTableQuery {
      val command = TermType.RECONFIGURE
      val string = "reconfigure"
      val arguments = table :: Nil
      val options = ComposableOptions.compose(shards, replicas, dryRun)
    }

    def reconfigure(emergencyRepair: EmergencyRepairOptions,
                    dryRun: RealRun.type): ReconfigureTableQuery = new ReconfigureTableQuery {
      val command = TermType.RECONFIGURE
      val string = "reconfigure"
      val arguments = table :: Nil
      val options = ComposableOptions.compose(emergencyRepair, dryRun)
    }

    def reconfigure(emergencyRepair: EmergencyRepairOptions,
                    dryRun: DryRun.type): ReconfigureDryTableQuery = new ReconfigureDryTableQuery {
      val command = TermType.RECONFIGURE
      val string = "reconfigure"
      val arguments = table :: Nil
      val options = ComposableOptions.compose(emergencyRepair, dryRun)
    }
  }

  implicit class ReconfigureOnDbOp(val db: ReqlDatabase) {
    def reconfigure(shards: ShardsOptions,
                    replicas: VotingReplicasOptions,
                    dryRun: RealRun.type): ReconfigureDbQuery = new ReconfigureDbQuery {
      val command = TermType.RECONFIGURE
      val string = "reconfigure"
      val arguments = db :: Nil
      val options = ComposableOptions.compose(shards, replicas, dryRun)
    }

    def reconfigure(shards: ShardsOptions,
                    replicas: VotingReplicasOptions,
                    dryRun: DryRun.type): ReconfigureDryDbQuery = new ReconfigureDryDbQuery {
      val command = TermType.RECONFIGURE
      val string = "reconfigure"
      val arguments = db :: Nil
      val options = ComposableOptions.compose(shards, replicas, dryRun)
    }
  }

  // status
  trait StatusTableQuery extends ReqlSelectionOfObject[ReqlTableStatusResult, UUID]

  implicit class StatusOnTableOp[T <: ReqlObject, PK](val table: ReqlTable[T, PK]) {
    def status(): StatusTableQuery = new StatusTableQuery {
      val command = TermType.STATUS
      val string = "status"
      val arguments = table :: Nil
      val options = Options.empty
    }
  }

  // wait
  trait WaitTableQuery extends ReqlWaitingResult
  trait WaitDatabaseQuery extends ReqlWaitingResult
  //trait WaitRQuery extends ReqlObject

  //TODO: not by spec (.waitFor instead .wait)
  //TODO: docs says what it can be called on r like r.wait(r.table(...)), but js driver not allows it
  implicit class WaitOnTableOp[T <: ReqlObject, PK](val table: ReqlTable[T, PK]) {
    def waitFor(status: WaitOptions, timeout: TimeoutOptions): WaitTableQuery = new WaitTableQuery {
      val command = TermType.WAIT
      val string = "wait"
      val arguments = table :: Nil
      val options = ComposableOptions.compose(status, timeout)
    }
  }

  implicit class WaitOnDatabaseOp(val db: ReqlDatabase) {
    def waitFor(status: WaitOptions, timeout: TimeoutOptions): WaitDatabaseQuery = new WaitDatabaseQuery {
      val command = TermType.WAIT
      val string = "wait"
      val arguments = db :: Nil
      val options = ComposableOptions.compose(status, timeout)
    }
  }

  /*implicit class WaitOnROp(val r: ReqlR) extends AnyVal {
    def waitFor(table: ReqlTable, status: WaitOptions, timeout: TimeoutOptions): WaitRQuery = new WaitRQuery {
      val command = TermType.WAIT
      val string = "wait"
      val arguments = table :: Nil
      val options = ComposableOptions.compose(status, timeout)
    }

    def waitFor(db: ReqlDatabase, status: WaitOptions, timeout: TimeoutOptions): WaitRQuery = new WaitRQuery {
      val command = TermType.WAIT
      val string = "wait"
      val arguments = db :: Nil
      val options = ComposableOptions.compose(status, timeout)
    }
  }*/

}
