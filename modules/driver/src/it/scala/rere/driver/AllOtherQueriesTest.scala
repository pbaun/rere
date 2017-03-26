package rere.driver

import java.time.ZonedDateTime
import java.util.UUID
import java.util.concurrent.atomic.AtomicLong

import akka.actor.ActorSystem
import akka.stream.scaladsl.{Flow, Keep, Sink}
import akka.util.ByteString
import io.circe.Json
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{Millis, Seconds, Span}
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpec}
import rere.driver.pool.ConnectionPool
import rere.ql.data.{ChangefeedNotification, GeoPoint, ModificationResult, UserPermissions}
import rere.ql.shapes._

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Failure, Random, Success}

//TODO: split this
class AllOtherQueriesTest extends WordSpec with ScalaFutures with Matchers with BeforeAndAfterAll {

  implicit val defaultPatience =
    PatienceConfig(timeout = Span(15, Seconds), interval = Span(100, Millis))

  case class Author(id: String, name: String)
  case class Abc(id: String, name: Option[String])

  import io.circe.generic.auto._
  object AuthorsShape extends CirceShape[Author, String]
  object AbcShape extends CirceShape[Abc, String]

  object TestDatabase extends DatabaseShape("test") {
    implicit val authors = table("authors", AuthorsShape)
    implicit val abc = table("abc", AbcShape)
  }

  implicit val ec = ExecutionContext.Implicits.global
  implicit val system = ActorSystem("rere")
  val credentials = Credentials("admin", "")
  val settings = ConnectionSettings("127.0.0.1", 28015, ConnectionSettings.noSslConnection)   //TODO: constants and helpers for ssl config
  /*val settings2 = ConnectionSettings(
    host = "127.0.0.1",
    port = 28015,
    ConnectionSettings.sslConnection(
      pathToCert =  "/mnt/cert.pem",
      protocolVersion = TLSProtocolVersion.`TLSv1.2`
    )
  )*/
  val pool = ConnectionPool.create(credentials, settings, "pool", 4)
  import rere.driver.runners.all._
  import rere.ql.queries.all._

  override protected def afterAll(): Unit = {
    println("!!!!!!!!!!!! afterAll !!!!!!!!!!!!!!!!!!")
    Await.result(pool.shutdown(), 5.seconds)
    Await.result(system.terminate(), 5.seconds)
    ()
  }

  "driver" should {
    "update models" ignore {
      implicit val abcShape: ModelShape[Abc, String] = AbcShape
      val model: Abc = Abc("123-a", Some("abc name " + Random.nextInt()))

      val updateF = TestDatabase.abc.table().get("123-a").update(model).run(pool).future()
      updateF.onComplete {
        case Success(res) =>
          println(s"Update of model done: $res")

        case Failure(ex) =>
          println(s"Update of model failure: $ex")
      }
    }

    "update many models" ignore {
      implicit val abcShape: ModelShape[Abc, String] = AbcShape
      val model: Abc = Abc("123-a", Some("abc name " + Random.nextInt()))

      val updateF = TestDatabase.abc.table().filter(model).update(model).run(pool).future()
      updateF.onComplete {
        case Success(res) =>
          println(s"Update of many models done: $res")

        case Failure(ex) =>
          println(s"Update of many models failure: $ex")
      }
    }

    "replace model" ignore {
      implicit val abcShape: ModelShape[Abc, String] = AbcShape
      val model: Abc = Abc("123-a", Some("abc name " + Random.nextInt()))

      val replaceF = TestDatabase.abc.table().get("123-a").replace(model).run(pool).future()
      replaceF.onComplete {
        case Success(res) =>
          println(s"Replace of model done: $res")

        case Failure(ex) =>
          println(s"Replace of model failure: $ex")
      }
    }

    "delete model" ignore {
      implicit val abcShape: ModelShape[Abc, String] = AbcShape

      val deleteF = TestDatabase.abc.table().get("123-a").delete().run(pool).future()
      deleteF.onComplete {
        case Success(res) =>
          println(s"Delete of model done: $res")

        case Failure(ex) =>
          println(s"Delete of model failure: $ex")
      }
    }

    "insert and fetch models with complex types" ignore {
      case class Bcd(date: ZonedDateTime, uuid: UUID, json: Json, binary: ByteString, seq: Seq[String])

      implicit object BcdShape extends Shape(Bcd.apply _, PrimaryKey[UUID]) with IdeaTypeHint[Bcd] {
        implicit val date = field("date", _.date)
        implicit val uuid = field("uuid", _.uuid)
        implicit val json = field("json", _.json)
        implicit val binary = field("binary", _.binary)
        implicit val seq = field("seq", _.seq)

        def primaryKey = pk(uuid)
        def projection = date :-: uuid :-: json :-: binary :-: seq :-: SNil
      }

      object TestDatabase extends DatabaseShape("test") {
        val bcd = table("abc", BcdShape)
      }

      val date = ZonedDateTime.now()
      val uuid = UUID.fromString("84844d6a-6206-4e22-b790-96db6e092230")
      val json = Json.obj(
        "a" -> Json.fromInt(123),
        "str" -> Json.fromString("test"),
        "arr" -> Json.arr(
          Json.fromBoolean(false), Json.fromString("inner")
        )
      )
      val binary = ByteString("unicode string π")
      val seq = Seq("abc", "bcd", "cde")
      val model: Bcd = Bcd(date, uuid, json, binary, seq)

      val insF: Future[ModificationResult[Bcd, UUID]] = TestDatabase.bcd.table().insert(model).run(pool).future()
      insF.onComplete {
        case Success(res) =>
          println(s"Insert of model done: $res")
          TestDatabase.bcd.table().get(res.generatedKeys.get.head.toString).run(pool).future().onComplete {
            case Success(m) =>
              println(s"Get of model done: $m")

            case Failure(ex) =>
              println(s"Get of model failure: $ex")
          }

        case Failure(ex) =>
          println(s"Insert of model failure: $ex")
      }
    }

    "pass all supported options" ignore {
      import rere.ql.options.all._

      val f: Future[ZonedDateTime] = r.now().run(
        pool,
        readMode = Single,
        profile = DoProfile,
        durability = Hard,
        noreply = Reply,
        arrayLimit = ArrayLimit(10),
        minBatchRows = MinBatchRows(2),
        maxBatchRows = MaxBatchRows(100),
        maxBatchBytes = MaxBatchBytes(64 * 1024),
        maxBatchSeconds = MaxBatchSeconds(0.3),
        firstBatchScaledownFactor = FirstBatchScaledownFactor(2)
      ).future()

      f.onComplete {
        case Success(time) =>
          println(s"Success done: $time")

        case Failure(ex) =>
          println(s"Failure done: $ex")
      }
    }

    "fetch models by id" ignore {
      implicit val authorsShape = AuthorsShape

      val author = TestDatabase.authors.table().get("c3c7e2b4-059b-42ee-9e42-ed93b1cae3d0")
        .run(pool).future()

      author.onComplete {
        case Success(author) =>
          println(s"Success done: $author")

        case Failure(ex) =>
          println(s"Failure done: $ex")
      }
    }

    "receive changefeeds" ignore {
      val counter = new AtomicLong(0)
      val sinkCF = Flow[ChangefeedNotification[Abc]].toMat(Sink.foreach { change =>
        println(s"@@Sink: ${counter.getAndIncrement()} $change")
      })(Keep.right)

      implicit val abcShape = AbcShape

      val (sinkMatCF, doneCF) = TestDatabase.abc.table().changes().run(pool).drainTo(sinkCF)

      doneCF.onComplete {
        case Success(done) =>
          println(s"Success done: $done")

        case Failure(ex) =>
          println(s"Failure done: $ex")
      }
    }

    "create database" ignore {
      val dbName = "test_" + Math.abs(Random.nextInt().toLong)

      r.dbCreate(dbName).run(pool).future().onComplete {
        case Success(res) =>
          println(s"DB create done: $res")
          //DB create done: DatabaseCreationResult(1,List(ChangefeedNotification(None,Some(DatabaseConfig(f7f8ba59-9a95-4bec-8931-dd62ccae26d1,test_415130552)))))

        case Failure(ex) =>
          println(s"DB create failure: $ex")
      }
    }

    "drop database" ignore {
      val dbName = "test_415130552"

      r.dbDrop(dbName).run(pool).future().onComplete {
        case Success(res) =>
          println(s"DB drop done: $res")
          //DB drop done: DatabaseDroppingResult(1,0,List(ChangefeedNotification(Some(DatabaseConfig(f7f8ba59-9a95-4bec-8931-dd62ccae26d1,test_415130552)),None)))

        case Failure(ex) =>
          println(s"DB drop failure: $ex")
      }
    }

    "list databases" ignore {
      r.dbList().run(pool).future().onComplete {
        case Success(res) =>
          println(s"DB list done: $res")
          //DB list done: ListBuffer(rethinkdb, test)

        case Failure(ex) =>
          println(s"DB list failure: $ex")
      }
    }

    "create table" ignore {
      val tableName = "test_" + Math.abs(Random.nextInt().toLong)

      r.tableCreate(tableName).run(pool).future().onComplete {
        case Success(res) =>
          println(s"Table create done: $res")
          //Table create done: TableCreationResult(1,List(ChangefeedNotification(None,Some(TableConfig(92afe552-904e-4747-b7b1-074395e74b6d,test_1016647763,test,id,List(Shard(MacBook_Pavel_local_hn5,List(MacBook_Pavel_local_hn5),List())),List(),majority,hard)))))

        case Failure(ex) =>
          println(s"Table create failure: $ex")
      }
    }

    "drop table" ignore {
      val tableName = "test_1016647763"

      r.db("test").tableDrop(tableName).run(pool).future().onComplete {
        case Success(res) =>
          println(s"Table drop done: $res")
          //Table drop done: TableDroppingResult(1,List(ChangefeedNotification(Some(TableConfig(92afe552-904e-4747-b7b1-074395e74b6d,test_1016647763,test,id,List(Shard(MacBook_Pavel_local_hn5,List(MacBook_Pavel_local_hn5),List())),List(),majority,hard)),None)))

        case Failure(ex) =>
          println(s"Table drop failure: $ex")
      }
    }

    "list tables" ignore {
      r.db("test").tableList().run(pool).future().onComplete {
        case Success(res) =>
          println(s"Table list done: $res")
          //Table list done: ListBuffer(abc, authors, tags, tv_shows)

        case Failure(ex) =>
          println(s"Table list failure: $ex")
      }
    }

    "create index" ignore {
      TestDatabase.abc.table().indexCreate("name").run(pool).future().onComplete {
        case Success(res) =>
          println(s"Index create done: $res")
          //Index create done: IndexCreationResult(1)

        case Failure(ex) =>
          println(s"Index create failure: $ex")
      }
    }

    "drop index" ignore {
      TestDatabase.abc.table().indexDrop("name").run(pool).future().onComplete {
        case Success(res) =>
          println(s"Index drop done: $res")
          //Index drop done: IndexDroppingResult(1)

        case Failure(ex) =>
          println(s"Index drop failure: $ex")
      }
    }

    "list indexes" ignore {
      TestDatabase.abc.table().indexList().run(pool).future().onComplete {
        case Success(res) =>
          println(s"Index list done: $res")
          //Index list done: ListBuffer(area, code, code1, code3)

        case Failure(ex) =>
          println(s"Index list failure: $ex")
      }
    }

    "rename index" ignore {
      TestDatabase.abc.table().indexRename("name", "name1").run(pool).future().onComplete {
        case Success(res) =>
          println(s"Index rename done: $res")
          //Index rename done: IndexRenamingResult(1)

        case Failure(ex) =>
          println(s"Index rename failure: $ex")
      }
    }

    "check index status" ignore {
      TestDatabase.abc.table().indexStatus().run(pool).future().onComplete {
        case Success(res) =>
          println(s"Index status done: $res")

        case Failure(ex) =>
          println(s"Index status failure: $ex")
      }
    }

    "wait index" ignore {
      TestDatabase.abc.table().indexWait("area", "code").run(pool).future().onComplete {
        case Success(res) =>
          println(s"Index wait done: $res")
          //Index wait done: ListBuffer(IndexStatus(area,true,...

        case Failure(ex) =>
          println(s"Index wait failure: $ex")
      }
    }

    "check table status" ignore {
      TestDatabase.abc.table().status().run(pool).future().onComplete {
        case Success(res) =>
          println(s"Table status done: $res")
          //Table status done: TableStatus(f52e39dc-79d3-48c6-98f1-43eab399d449,abc,test,TableStatusFlags(true,true,true,true),List(TableShardStatus(List(MacBook_Pavel_local_hn5),List(TableReplicaStatus(MacBook_Pavel_local_hn5,ready)))),MacBook_Pavel_local_hn5)

        case Failure(ex) =>
          println(s"Table status failure: $ex")
      }
    }

    "fetch table config" ignore {
      TestDatabase.abc.table().config().run(pool).future().onComplete {
        case Success(res) =>
          println(s"Table config done: $res")
          //Table config done: TableConfig(f52e39dc-79d3-48c6-98f1-43eab399d449,abc,test,id,List(Shard(MacBook_Pavel_local_hn5,List(MacBook_Pavel_local_hn5),List())),List(area, code, code1, code3, name1),majority,hard)

        case Failure(ex) =>
          println(s"Table config failure: $ex")
      }
    }

    "fetch database config" ignore {
      TestDatabase.database().config().run(pool).future().onComplete {
        case Success(res) =>
          println(s"Database config done: $res")
          //Database config done: DatabaseConfig(437f7ba9-ba8e-4613-a041-e7a0cb11dfdf,test)

        case Failure(ex) =>
          println(s"Database config failure: $ex")
      }
    }

    "change global permissions" ignore {
      r.grant("user", UserPermissions(Some(true), Some(true), None, None)).run(pool).future().onComplete {
        case Success(res) =>
          println(s"Global grant done: $res")
          //Global grant done: GrantingResult(1,Vector(ChangefeedNotification(Some(UserPermissions(Some(true),Some(false),Some(true),Some(false))),Some(UserPermissions(Some(true),Some(true),Some(true),Some(false))))))

        case Failure(ex) =>
          println(s"Global grant failure: $ex")
      }
    }

    "change database permissions" ignore {
      TestDatabase.database().grant("user", UserPermissions(Some(true), Some(true), None, None)).run(pool).future().onComplete {
        case Success(res) =>
          println(s"Database grant done: $res")
          //Database grant done: GrantingResult(1,Vector(ChangefeedNotification(Some(UserPermissions(Some(true),Some(false),None,Some(false))),Some(UserPermissions(Some(true),Some(true),None,Some(false))))))

        case Failure(ex) =>
          println(s"Database grant failure: $ex")
      }
    }

    "change table permissions" ignore {
      TestDatabase.abc.table().grant("user", UserPermissions(Some(true), Some(true), None, None)).run(pool).future().onComplete {
        case Success(res) =>
          println(s"Table grant done: $res")
          //Table grant done: GrantingResult(1,Vector(ChangefeedNotification(Some(UserPermissions(Some(true),Some(false),None,Some(false))),Some(UserPermissions(Some(true),Some(true),None,Some(false))))))

        case Failure(ex) =>
          println(s"Table grant failure: $ex")
      }
    }

    "rebalance database" ignore {
      TestDatabase.database().rebalance().run(pool).future().onComplete {
        case Success(res) =>
          println(s"Database rebalance done: $res")
          //Database rebalance done: RebalancingResult(4,Vector(ChangefeedNotification(...

        case Failure(ex) =>
          println(s"Database rebalance failure: $ex")
      }
    }

    "rebalance table" ignore {
      TestDatabase.abc.table().rebalance().run(pool).future().onComplete {
        case Success(res) =>
          println(s"Table rebalance done: $res")
          //Table rebalance done: RebalancingResult(1,Vector(ChangefeedNotification(...

        case Failure(ex) =>
          println(s"Table rebalance failure: $ex")
      }
    }

    "reconfigure database [real run]" ignore {
      import rere.ql.options.all._

      TestDatabase.database().reconfigure(Shards(1), Replicas(1).allVoting(), RealRun).run(pool).future().onComplete {
        case Success(res) =>
          println(s"Database reconfigure done: $res")
          //Database reconfigure done: ReconfiguringResult(4,Vector(ChangefeedNotification(...

        case Failure(ex) =>
          println(s"Database reconfigure failure: $ex")
      }
    }

    "reconfigure database [dry run]" ignore {
      import rere.ql.options.all._

      TestDatabase.database().reconfigure(Shards(1), Replicas(1).allVoting(), DryRun).run(pool).future().onComplete {
        case Success(res) =>
          println(s"Database reconfigure done: $res")
          //Database reconfigure done: ReconfiguringDryResult(0,Vector(ChangefeedNotification(...

        case Failure(ex) =>
          println(s"Database reconfigure failure: $ex")
      }
    }

    "reconfigure table [real run]" ignore {
      import rere.ql.options.all._

      TestDatabase.abc.table().reconfigure(Shards(1), Replicas(1).allVoting(), RealRun).run(pool).future().onComplete {
        case Success(res) =>
          println(s"Table reconfigure done: $res")
          //Table reconfigure done: ReconfiguringResult(1,Vector(ChangefeedNotification(...

        case Failure(ex) =>
          println(s"Table reconfigure failure: $ex")
      }
    }

    "reconfigure table [dry run]" ignore {
      import rere.ql.options.all._

      TestDatabase.abc.table().reconfigure(Shards(1), Replicas(1).allVoting(), DryRun).run(pool).future().onComplete {
        case Success(res) =>
          println(s"Table reconfigure done: $res")
          //Table reconfigure done: ReconfiguringDryResult(0,Vector(ChangefeedNotification(...

        case Failure(ex) =>
          println(s"Table reconfigure failure: $ex")
      }
    }

    "reconfigure table in case of crash" ignore {
      //TODO: crash and check this
      import rere.ql.options.all._

      TestDatabase.abc.table().reconfigure(UnsafeRollback, RealRun).run(pool).future().onComplete {
        case Success(res) =>
          println(s"Table reconfigure done: $res")

        case Failure(ex) =>
          println(s"Table reconfigure failure: $ex")
          //This table doesn't need to be repaired.
      }
    }

    "wait for database ready" ignore {
      import rere.ql.options.all._

      TestDatabase.database().waitFor(AllReplicasReady, WithoutTimeout).run(pool).future().onComplete {
        case Success(res) =>
          println(s"Database wait done: $res")
          //Database wait done: WaitingResult(4)

        case Failure(ex) =>
          println(s"Database wait failure: $ex")
      }
    }

    "wait for table ready" ignore {
      import rere.ql.options.all._

      TestDatabase.abc.table().waitFor(ReadyForWrites, WithoutTimeout).run(pool).future().onComplete {
        case Success(res) =>
          println(s"Table wait done: $res")
          //Table wait done: WaitingResult(1)

        case Failure(ex) =>
          println(s"Table wait failure: $ex")
      }
    }

    "sync table" ignore {
      TestDatabase.abc.table().sync().run(pool).future().onComplete {
        case Success(res) =>
          println(s"Table sync done: $res")
          //Table sync done: SyncingResult(1)

        case Failure(ex) =>
          println(s"Table sync failure: $ex")
      }
    }

    "modify using forEach" ignore {
      import TestDatabase._

      abc.table().forEach(
        _ => authors.table().get("random_uuid").delete()
      ).run(pool).future().onComplete {
        case Success(res) =>
          println(s"Table forEach done: $res")
          //Table forEach done: ModificationResult(0,0,0,0,None,0,45,None,None,None)

        case Failure(ex) =>
          println(s"Table forEach failure: $ex")
      }
    }

    "work with point type" in {
      whenReady(r.point(180, 90).run(pool).future()) { res =>
        println(s"Point fetch done: $res")
        //Point fetch done: GeometryPoint(123.0,22.0)
        //Point fetch done: GeometryPoint(180.0,90.0)
      }
    }

    "work with line type" ignore {
      val p1 = GeoPoint(0, 0)
      val p2 = GeoPoint(1, 1)
      val p3 = GeoPoint(2, 2)
      val p4 = GeoPoint(3, 3)

      r.line(p1, p2, p3, p4).run(pool).future().onComplete {
        case Success(res) =>
          println(s"Line fetch done: $res")
          //Line fetch done: GeoLineString(GeoPoint(0.0,0.0),GeoPoint(1.0,1.0),List())
          //Line fetch done: GeoLineString(GeoPoint(0.0,0.0),GeoPoint(1.0,1.0),List(GeoPoint(2.0,2.0), GeoPoint(3.0,3.0)))

        case Failure(ex) =>
          println(s"Line fetch failure: $ex")
      }
    }

    "work with polygon type" ignore {
      val p1 = GeoPoint(0, 0)
      val p2 = GeoPoint(5, 0)
      val p3 = GeoPoint(5, 5)
      val p4 = GeoPoint(0, 5)

      r.polygon(p1, p2, p3, p4).run(pool).future().onComplete {
        case Success(res) =>
          println(s"Polygon fetch done: $res")
          //Polygon fetch done: GeoPolygon(GeoLinearRing(GeoPoint(0.0,0.0),GeoPoint(5.0,0.0),GeoPoint(5.0,5.0),List(GeoPoint(0.0,5.0), GeoPoint(0.0,0.0))),List())

        case Failure(ex) =>
          println(s"Polygon fetch failure: $ex")
      }
    }
  }

}
