package rere.ql.queries

import io.circe.JsonObject
import org.scalatest.WordSpec
import rere.ql.types._

class TableQueriesTest extends WordSpec with ReqlMatchers {

  import rere.ql.options.all._
  import rere.ql.queries.all.r
  import rere.ql.queries.document._
  import rere.ql.queries.math._
  import rere.ql.queries.table._
  import rere.ql.queries.values._

  "table operator" should {
    "be accessible on r and" should {
      "allow to call it only with table name" in {
        r.table("marvel") shouldBe
          subtypeOf [ReqlTable[JsonObject, PrimaryKey.String]] and serializedTo("""[15,["marvel"]]""")
      }

      "allow to call it with table name and read mode option" in {
        r.table("marvel", DefaultReadMode) shouldBe
          subtypeOf [ReqlTable[JsonObject, PrimaryKey.String]] and serializedTo("""[15,["marvel"]]""")

        r.table("marvel", Single) shouldBe
          subtypeOf [ReqlTable[JsonObject, PrimaryKey.String]] and serializedTo("""[15,["marvel"],{"read_mode":"single"}]""")

        r.table("marvel", Majority) shouldBe
          subtypeOf [ReqlTable[JsonObject, PrimaryKey.String]] and serializedTo("""[15,["marvel"],{"read_mode":"majority"}]""")

        r.table("marvel", Outdated) shouldBe
          subtypeOf [ReqlTable[JsonObject, PrimaryKey.String]] and serializedTo("""[15,["marvel"],{"read_mode":"outdated"}]""")
      }

      "allow to call it with table name and identifier format option" in {
        r.table("marvel", identifierFormat = DefaultIdentifierFormat) shouldBe
          subtypeOf [ReqlTable[JsonObject, PrimaryKey.String]] and serializedTo("""[15,["marvel"]]""")

        r.table("marvel", identifierFormat = NameIdentifier) shouldBe
          subtypeOf [ReqlTable[JsonObject, PrimaryKey.String]] and serializedTo("""[15,["marvel"],{"identifier_format":"name"}]""")

        r.table("marvel", identifierFormat = UuidIdentifier) shouldBe
          subtypeOf [ReqlTable[JsonObject, PrimaryKey.String]] and serializedTo("""[15,["marvel"],{"identifier_format":"uuid"}]""")
      }

      "allow to call it with table name and both options" in {
        r.table("marvel", Majority, UuidIdentifier) shouldBe
          subtypeOf [ReqlTable[JsonObject, PrimaryKey.String]] and
          serializedTo("""[15,["marvel"],{"read_mode":"majority","identifier_format":"uuid"}]""")
      }
    }

    "be accessible on ReqlDatabase and" should {
      "allow to call it only with table name" in {
        r.db("heroes").table("marvel") shouldBe
          subtypeOf [ReqlTable[JsonObject, PrimaryKey.String]] and serializedTo("""[15,[[14,["heroes"]],"marvel"]]""")
      }

      "allow to call it with table name and read mode option" in {
        r.db("heroes").table("marvel", DefaultReadMode) shouldBe
          subtypeOf [ReqlTable[JsonObject, PrimaryKey.String]] and serializedTo("""[15,[[14,["heroes"]],"marvel"]]""")

        r.db("heroes").table("marvel", Single) shouldBe
          subtypeOf [ReqlTable[JsonObject, PrimaryKey.String]] and
          serializedTo("""[15,[[14,["heroes"]],"marvel"],{"read_mode":"single"}]""")

        r.db("heroes").table("marvel", Majority) shouldBe
          subtypeOf [ReqlTable[JsonObject, PrimaryKey.String]] and
          serializedTo("""[15,[[14,["heroes"]],"marvel"],{"read_mode":"majority"}]""")

        r.db("heroes").table("marvel", Outdated) shouldBe
          subtypeOf [ReqlTable[JsonObject, PrimaryKey.String]] and
          serializedTo("""[15,[[14,["heroes"]],"marvel"],{"read_mode":"outdated"}]""")
      }

      "allow to call it with table name and identifier format option" in {
        r.db("heroes").table("marvel", identifierFormat = DefaultIdentifierFormat) shouldBe
          subtypeOf [ReqlTable[JsonObject, PrimaryKey.String]] and
          serializedTo("""[15,[[14,["heroes"]],"marvel"]]""")

        r.db("heroes").table("marvel", identifierFormat = NameIdentifier) shouldBe
          subtypeOf [ReqlTable[JsonObject, PrimaryKey.String]] and
          serializedTo("""[15,[[14,["heroes"]],"marvel"],{"identifier_format":"name"}]""")

        r.db("heroes").table("marvel", identifierFormat = UuidIdentifier) shouldBe
          subtypeOf [ReqlTable[JsonObject, PrimaryKey.String]] and
          serializedTo("""[15,[[14,["heroes"]],"marvel"],{"identifier_format":"uuid"}]""")
      }

      "allow to call it with table name and both options" in {
        r.db("heroes").table("marvel", Majority, UuidIdentifier) shouldBe
          subtypeOf [ReqlTable[JsonObject, PrimaryKey.String]] and
          serializedTo("""[15,[[14,["heroes"]],"marvel"],{"read_mode":"majority","identifier_format":"uuid"}]""")
      }
    }
  }

  "tableCreate operator" should {
    "be accessible on r and" should {
      "allow to call it only with table name" in {
        r.tableCreate("abc") shouldBe
          subtypeOf [ReqlTableCreationResult] and serializedTo("""[60,["abc"]]""")
      }

      "allow to call it with all options" in {
        r.tableCreate(
          "abc",
          PrimaryKeyField("code"),
          Hard,
          Shards(7),
          ReplicasByTags(ServerTag("spb"), ServerTag("spb") -> 3, ServerTag("msk") -> 2)
        ) shouldBe
          subtypeOf [ReqlTableCreationResult] and
          serializedTo("""[60,["abc"],{"primary_key":"code","durability":"hard","shards":7,"replicas":{"spb":3,"msk":2},"primary_replica_tag":"spb"}]""")
      }
    }

    "be accessible on ReqlDatabase and" should {
      "allow to call it only with table name" in {
        r.db("test").tableCreate("abc") shouldBe
          subtypeOf [ReqlTableCreationResult] and serializedTo("""[60,[[14,["test"]],"abc"]]""")
      }

      "allow to call it with all options" in {
        r.db("test").tableCreate(
          "abc",
          PrimaryKeyField("name"),
          Soft,
          Shards(3),
          Replicas(1)
        ) shouldBe
          subtypeOf [ReqlTableCreationResult] and
          serializedTo("""[60,[[14,["test"]],"abc"],{"primary_key":"name","durability":"soft","shards":3,"replicas":1}]""")

        r.db("test").tableCreate(
          "abc",
          PrimaryKeyField("code"),
          Hard,
          Shards(7),
          ReplicasByTags(ServerTag("spb"), ServerTag("spb") -> 3, ServerTag("msk") -> 2)
        ) shouldBe
          subtypeOf [ReqlTableCreationResult] and
          serializedTo("""[60,[[14,["test"]],"abc"],{"primary_key":"code","durability":"hard","shards":7,"replicas":{"spb":3,"msk":2},"primary_replica_tag":"spb"}]""")
      }
    }
  }

  "tableDrop operator" should {
    "be accessible on ReqlDatabase and" should {
      "allow to call it with table name" in {
        r.db("test").tableDrop("abc") shouldBe
          subtypeOf [ReqlTableDroppingResult] and serializedTo("""[61,[[14,["test"]],"abc"]]""")

        r.db("test").tableDrop(r.expr("abc").add("def")) shouldBe
          subtypeOf [ReqlTableDroppingResult] and serializedTo("""[61,[[14,["test"]],[24,["abc","def"]]]]""")
      }
    }
  }

  "tableList operator" should {
    "be accessible on ReqlDatabase and" should {
      "allow to call it without parameters" in {
        r.db("test").tableList() shouldBe
          subtypeOf [ReqlArray[ReqlString]] and serializedTo("""[62,[[14,["test"]]]]""")
      }
    }
  }

  "indexCreate operator" should {
    "be accessible on ReqlTable and" should {
      "allow to call it only with index name" in {
        r.table[JsonObject, PrimaryKey.String]("abc").indexCreate("code") shouldBe
          subtypeOf [ReqlIndexCreationResult] and serializedTo("""[75,[[15,["abc"]],"code"]]""")
      }

      "allow to call it with index name and all options" in {
        r.table[JsonObject, PrimaryKey.String]("abc").indexCreate("code", SimpleIndex, RangeIndex) shouldBe
          subtypeOf [ReqlIndexCreationResult] and serializedTo("""[75,[[15,["abc"]],"code"]]""")

        r.table[JsonObject, PrimaryKey.String]("abc").indexCreate("code", MultiIndex, GeoIndex) shouldBe
          subtypeOf [ReqlIndexCreationResult] and
          serializedTo("""[75,[[15,["abc"]],"code"],{"multi":true,"geo":true}]""")
      }

      "allow to call it with index name and function" in {
        r.table[JsonObject, PrimaryKey.String]("abc").indexCreate("code", _("code_name")) shouldBe
          subtypeOf [ReqlIndexCreationResult] and
          serializedTo("""[75,[[15,["abc"]],"code",[69,[[2,[0]],[170,[[10,[0]],"code_name"]]]]]]""")
      }

      "allow to call it with index name, function and all options" in {
        r.table[JsonObject, PrimaryKey.String]("abc").indexCreate("code", _("code_name"), SimpleIndex, RangeIndex) shouldBe
          subtypeOf [ReqlIndexCreationResult] and
          serializedTo("""[75,[[15,["abc"]],"code",[69,[[2,[0]],[170,[[10,[0]],"code_name"]]]]]]""")

        r.table[JsonObject, PrimaryKey.String]("abc").indexCreate("code", _("code_name"), MultiIndex, GeoIndex) shouldBe
          subtypeOf [ReqlIndexCreationResult] and
          serializedTo("""[75,[[15,["abc"]],"code",[69,[[2,[0]],[170,[[10,[0]],"code_name"]]]]],{"multi":true,"geo":true}]""")
      }
    }
  }

  "indexDrop operator" should {
    "be accessible on ReqlTable and" should {
      "allow to call it with index name" in {
        r.table("abc").indexDrop("code") shouldBe
          subtypeOf [ReqlIndexDroppingResult] and serializedTo("""[76,[[15,["abc"]],"code"]]""")
      }
    }
  }

  "indexList operator" should {
    "be accessible on ReqlTable and" should {
      "allow to call it without arguments" in {
        r.table("abc").indexList() shouldBe
          subtypeOf [ReqlArray[ReqlString]] and serializedTo("""[77,[[15,["abc"]]]]""")
      }
    }
  }

  "indexStatus operator" should {
    "be accessible on ReqlTable and" should {
      "allow to call it without arguments" in {
        r.table("abc").indexStatus() shouldBe
          subtypeOf [ReqlArray[ReqlIndexStatusResult]] and serializedTo("""[139,[[15,["abc"]]]]""")
      }

      "allow to call it with index name" in {
        r.table("abc").indexStatus("code") shouldBe
          subtypeOf [ReqlArray[ReqlIndexStatusResult]] and serializedTo("""[139,[[15,["abc"]],"code"]]""")
      }

      "allow to call it with multiple index names" in {
        r.table("abc").indexStatus("code", "code1") shouldBe
          subtypeOf [ReqlArray[ReqlIndexStatusResult]] and serializedTo("""[139,[[15,["abc"]],"code","code1"]]""")
      }
    }
  }

  "indexWait operator" should {
    "be accessible on ReqlTable and" should {
      "allow to call it without arguments" in {
        r.table("abc").indexWait() shouldBe
          subtypeOf [ReqlArray[ReqlIndexStatusResult]] and serializedTo("""[140,[[15,["abc"]]]]""")
      }

      "allow to call it with index name" in {
        r.table("abc").indexWait("code") shouldBe
          subtypeOf [ReqlArray[ReqlIndexStatusResult]] and serializedTo("""[140,[[15,["abc"]],"code"]]""")
      }

      "allow to call it with multiple index names" in {
        r.table("abc").indexWait("code", "code1") shouldBe
          subtypeOf [ReqlArray[ReqlIndexStatusResult]] and serializedTo("""[140,[[15,["abc"]],"code","code1"]]""")
      }
    }
  }

  "indexRename operator" should {
    "be accessible on ReqlTable and" should {
      "allow to call it only with old and new index name" in {
        r.table("abc").indexRename("code", "type") shouldBe
          subtypeOf [ReqlIndexRenamingResult] and serializedTo("""[156,[[15,["abc"]],"code","type"]]""")
      }

      "allow to call it with index names and overwrite option" in {
        r.table("abc").indexRename("code", "type", NotOverwrite) shouldBe
          subtypeOf [ReqlIndexRenamingResult] and serializedTo("""[156,[[15,["abc"]],"code","type"]]""")

        r.table("abc").indexRename("code", "type", Overwrite) shouldBe
          subtypeOf [ReqlIndexRenamingResult] and
          serializedTo("""[156,[[15,["abc"]],"code","type"],{"overwrite":true}]""")
      }
    }
  }

}
