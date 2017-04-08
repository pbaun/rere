package rere.ql

import java.util.UUID

import akka.util.ByteString
import io.circe.{Json, JsonObject}
import org.scalatest.Matchers._
import org.scalatest.{Assertion, WordSpec}
import rere.ql.data._
import rere.ql.queries.{Func, Var}
import rere.ql.shapes.ReqlModel
import rere.ql.types._

object ProtoTestKit {
  import org.scalactic.source.Position
  import rasterization.building._

  import scala.reflect.runtime.universe._

  implicit class TestHelper(val query: ReqlExpr) extends AnyVal {
    def =*=(expected: String): Assertion = {
      import rasterization.building._
      query.build().utf8String shouldBe expected
    }

    def =%/%=[Q : Manifest](expected: String): Assertion = {
      import rasterization.building._
      query shouldBe an[Q]
      query.build().utf8String shouldBe expected
    }
  }

  implicit class TestExt[QueryType <: ReqlExpr : TypeTag](val query: QueryType) {
    def =%=[ExpectedType : TypeTag](expected: String)(implicit pos: Position): Assertion = {
      val typeMatches = typeOf[QueryType] <:< typeOf[ExpectedType]
      assert(
        typeMatches,
        s"Type mismatch: inferred type: ${typeOf[QueryType]}; required type: ${typeOf[ExpectedType]}"
      )
      query.build().utf8String shouldBe expected
    }
  }
}

class RasterizationTest extends WordSpec {

  import ProtoTestKit._
  import rere.ql.util.JsonToReql

  trait ShapesData {
    import rere.ql.shapes._

    case class Abc(name: String)

    object AbcShape extends Shape(Abc.apply _, PrimaryKey[String]) with IdeaTypeHint[Abc] {
      implicit val name = field("name", _.name)

      def primaryKey = pk(name)
      def projection = name :-: SNil
    }

    object TestDatabase extends DatabaseShape("test") {
      implicit val abc = table("abc", AbcShape)
    }
  }

  val emptyReqlObject: ReqlObject = queries.values.expr(Map.empty[String, ReqlDatum])

  "JsonToReql" should {

    import io.circe.literal._

    "transform array to make_array" in {
      JsonToReql.transform(json"""[1, 2, 3]""") shouldBe json"""[2, [1, 2, 3]]"""
    }

    "transform nested arrays to make_array" in {
      JsonToReql.transform(json"""[1, [2, 3, [4, 5, 6]], 7]""") shouldBe
        json"""[2, [1, [2, [2, 3, [2, [4, 5, 6]]]], 7]]"""
    }

    "transform array inside objects to make_array" in {
      JsonToReql.transform(json"""{"test": [1, 2], "test2": [3, {"test3": [4, [5, 6]]}]}""") shouldBe
        json"""{"test": [2, [1, 2]], "test2": [2, [3, {"test3": [2, [4, [2, [5, 6]]]]}]]}"""
    }
  }

  "Rasterization" should {
    import options.all._
    import queries.all._

    def jsonTable(tableName: String) = r.table[JsonObject, String](tableName)
    val abcJsonTable = jsonTable("abc")
    val bcdJsonTable = jsonTable("bcd")

    /*"rasterize queries how described in guide" in {

      r.db("blog").build().utf8String shouldBe """[14,["blog"]]"""

      r.db("blog").table("users").build().utf8String shouldBe """[15,[[14,["blog"]],"users"]]"""

      r.db("blog").table("users").filter(
        r.expr(Map(
          "name" -> r.expr("Michel")
        ))
      ).build().utf8String shouldBe """[39,[[15,[[14,["blog"]],"users"]],{"name":"Michel"}]]"""

      r.db("blog").table("users").filter(
        r.expr(JsonObject.fromMap(Map(
          "name" -> Json.fromString("Michel")
        )))
      ) =*= """[39,[[15,[[14,["blog"]],"users"]],{"name":"Michel"}]]"""

      r.db("blog").table("users").filter(
        r.expr(Map(
          "name" -> r.expr("Michel")
        ))
      ).changes().build().utf8String shouldBe """[152,[[39,[[15,[[14,["blog"]],"users"]],{"name":"Michel"}]]]]"""

      r.db("blog").table("users").filter(
        r.expr(Map(
          "name" -> r.expr("Mic\"hel")
        ))
      ).changes().build().utf8String shouldBe """[152,[[39,[[15,[[14,["blog"]],"users"]],{"name":"Mic\"hel"}]]]]"""

      //r.db("blog").table("users").asSequence.changes().build().utf8String shouldBe """[152,[[15,[[14,["blog"]],"users"]]]]"""

      r.db("blog").table("users").filter(
        r.expr(Map(
          "code" -> r.expr(123),
          "name" -> r.expr(Seq(r.expr(1), r.expr(2), r.expr(3)))
        ))
      ) =*= """[39,[[15,[[14,["blog"]],"users"]],{"code":123,"name":[2,[1,2,3]]}]]"""

      r.db("blog").table("users").filter(
        r.expr(JsonObject.fromMap(Map(
          "code" -> Json.fromInt(123),
          "name" -> Json.fromValues(Seq(Json.fromInt(1), Json.fromInt(2), Json.fromInt(3)))
        )))
      ) =*= """[39,[[15,[[14,["blog"]],"users"]],{"code":123,"name":[2,[1,2,3]]}]]"""

      r.db("blog").table("users").filter(
        JsonObject.fromMap(Map(
          "code" -> Json.fromInt(123),
          "name" -> Json.fromValues(Seq(Json.fromInt(1), Json.fromInt(2), Json.fromInt(3)))
        ))
      ) =*= """[39,[[15,[[14,["blog"]],"users"]],{"code":123,"name":[2,[1,2,3]]}]]"""
    }*/

    "var" in {
      val varExpr = new Var(7)

      varExpr =*= "[10,[7]]"
    }

    "js" in {
      r.js("while(true) {}") =%=[ReqlJS] """[11,["while(true) {}"]]"""

      r.js("while(true) {}", WithDefaultJSTimeout) =%=[ReqlJS] """[11,["while(true) {}"]]"""
      r.js("while(true) {}", WithJSTimeout(7)) =%=[ReqlJS] """[11,["while(true) {}"],{"timeout":7}]"""
      r.js("while(true) {}", WithJSTimeout(BigDecimal(1.3))) =%=[ReqlJS] """[11,["while(true) {}"],{"timeout":1.3}]"""

      //complex
      r.js("while(true) {}", WithJSTimeout(r.random(7, FloatValues))) =%=[ReqlJS] """[11,["while(true) {}"],{"timeout":[151,[7],{"float":true}]}]"""
    }

    "uuid" in {
      r.uuid() =%=[ReqlString] "[169,[]]"

      r.uuid("abc") =%=[ReqlString] """[169,["abc"]]"""
    }

    "http" in {
      r.http("https://github.com") =%=[ReqlHttpResult] """[153,["https://github.com"]]"""

      r.http("https://github.com", WithDefaultHttpTimeout) =%=[ReqlHttpResult] """[153,["https://github.com"]]"""
      r.http("https://github.com", WithHttpTimeout(BigDecimal(0.1))) =%=[ReqlHttpResult] """[153,["https://github.com"],{"timeout":0.1}]"""

      r.http("https://github.com", attempts = WithDefaultHttpAttempts) =%=[ReqlHttpResult] """[153,["https://github.com"]]"""
      r.http("https://github.com", attempts = WithHttpAttempts(3)) =%=[ReqlHttpResult] """[153,["https://github.com"],{"attempts":3}]"""

      r.http("https://github.com", redirects = WithDefaultHttpRedirects) =%=[ReqlHttpResult] """[153,["https://github.com"]]"""
      r.http("https://github.com", redirects = WithHttpRedirects(2)) =%=[ReqlHttpResult] """[153,["https://github.com"],{"redirects":2}]"""

      r.http("https://expired.badssl.com", verify = WithDefaultHttpVerify) =%=[ReqlHttpResult] """[153,["https://expired.badssl.com"]]"""
      r.http("https://expired.badssl.com", verify = DoVerify) =%=[ReqlHttpResult] """[153,["https://expired.badssl.com"],{"verify":true}]"""
      r.http("https://expired.badssl.com", verify = DoNotVerify) =%=[ReqlHttpResult] """[153,["https://expired.badssl.com"],{"verify":false}]"""
      r.http("https://expired.badssl.com", verify = WithHttpVerify(true)) =%=[ReqlHttpResult] """[153,["https://expired.badssl.com"],{"verify":true}]"""
      r.http("https://expired.badssl.com", verify = WithHttpVerify(false)) =%=[ReqlHttpResult] """[153,["https://expired.badssl.com"],{"verify":false}]"""

      r.http("https://github.com", resultFormat = WithDefaultHttpResultFormat) =%=[ReqlHttpResult] """[153,["https://github.com"]]"""
      r.http("https://github.com", resultFormat = TextResultFormat) =%=[ReqlHttpResult] """[153,["https://github.com"],{"result_format":"text"}]"""
      r.http("https://github.com", resultFormat = JsonResultFormat) =%=[ReqlHttpResult] """[153,["https://github.com"],{"result_format":"json"}]"""
      r.http("https://github.com", resultFormat = JsonpResultFormat) =%=[ReqlHttpResult] """[153,["https://github.com"],{"result_format":"jsonp"}]"""
      r.http("https://github.com", resultFormat = BinaryResultFormat) =%=[ReqlHttpResult] """[153,["https://github.com"],{"result_format":"binary"}]"""
      r.http("https://github.com", resultFormat = AutoResultFormat) =%=[ReqlHttpResult] """[153,["https://github.com"],{"result_format":"auto"}]"""
      r.http("https://github.com", resultFormat = WithHttpResultFormat("text")) =%=[ReqlHttpResult] """[153,["https://github.com"],{"result_format":"text"}]"""
      r.http("https://github.com", resultFormat = WithHttpResultFormat("auto")) =%=[ReqlHttpResult] """[153,["https://github.com"],{"result_format":"auto"}]"""

      r.http("https://github.com", method = WithDefaultHttpMethod) =%=[ReqlHttpResult] """[153,["https://github.com"]]"""
      r.http("https://github.com", method = HttpGet) =%=[ReqlHttpResult] """[153,["https://github.com"],{"method":"GET"}]"""
      r.http("https://github.com", method = HttpPost) =%=[ReqlHttpResult] """[153,["https://github.com"],{"method":"POST"}]"""
      r.http("https://github.com", method = HttpPut) =%=[ReqlHttpResult] """[153,["https://github.com"],{"method":"PUT"}]"""
      r.http("https://github.com", method = HttpPatch) =%=[ReqlHttpResult] """[153,["https://github.com"],{"method":"PATCH"}]"""
      r.http("https://github.com", method = HttpDelete) =%=[ReqlHttpResult] """[153,["https://github.com"],{"method":"DELETE"}]"""
      r.http("https://github.com", method = HttpHead) =%=[ReqlHttpResult] """[153,["https://github.com"],{"method":"HEAD"}]"""
      r.http("https://github.com", method = WithHttpMethod("GET")) =%=[ReqlHttpResult] """[153,["https://github.com"],{"method":"GET"}]"""
      r.http("https://github.com", method = WithHttpMethod("POST")) =%=[ReqlHttpResult] """[153,["https://github.com"],{"method":"POST"}]"""

      r.http("https://github.com", auth = WithoutHttpAuth) =%=[ReqlHttpResult] """[153,["https://github.com"]]"""
      r.http("https://httpbin.org/basic-auth/user/passwd", auth = DefaultAuth("user", "passwd")) =%=[ReqlHttpResult] """[153,["https://httpbin.org/basic-auth/user/passwd"],{"auth":{"user":"user","pass":"passwd"}}]"""
      r.http("https://httpbin.org/basic-auth/user/passwd", auth = BasicAuth("user", "passwd")) =%=[ReqlHttpResult] """[153,["https://httpbin.org/basic-auth/user/passwd"],{"auth":{"type":"basic","user":"user","pass":"passwd"}}]"""
      r.http("https://httpbin.org/digest-auth/auth/user/passwd", auth = DigestAuth("user", "passwd")) =%=[ReqlHttpResult] """[153,["https://httpbin.org/digest-auth/auth/user/passwd"],{"auth":{"type":"digest","user":"user","pass":"passwd"}}]"""
      r.http("https://httpbin.org/basic-auth/user/passwd", auth = WithHttpAuth("basic", "user", "passwd")) =%=[ReqlHttpResult] """[153,["https://httpbin.org/basic-auth/user/passwd"],{"auth":{"type":"basic","user":"user","pass":"passwd"}}]"""
      r.http("https://httpbin.org/digest-auth/auth/user/passwd", auth = WithHttpAuth("digest", "user", "passwd")) =%=[ReqlHttpResult] """[153,["https://httpbin.org/digest-auth/auth/user/passwd"],{"auth":{"type":"digest","user":"user","pass":"passwd"}}]"""

      r.http("https://httpbin.org/get", params = WithoutHttpParams) =%=[ReqlHttpResult] """[153,["https://httpbin.org/get"]]"""
      r.http("https://httpbin.org/get", params = HttpParams(Seq(
        new HttpQueryNumberParam("abc", 123),
        new HttpQueryStringParam("bcd", "cde")
      ))) =%=[ReqlHttpResult] """[153,["https://httpbin.org/get"],{"params":{"abc":123,"bcd":"cde"}}]"""

      r.http("https://httpbin.org/get", header = WithDefaultHttpHeader) =%=[ReqlHttpResult] """[153,["https://httpbin.org/get"]]"""
      r.http("https://httpbin.org/get", header = WithHttpHeader(
        HttpHeaderFieldsArray(Seq(
          new HttpHeaderStringField("From: webmaster@w3.org"),
          new HttpHeaderStringField("Host: www.w3.org")
        ))
      )) =%=[ReqlHttpResult] """[153,["https://httpbin.org/get"],{"header":[2,["From: webmaster@w3.org","Host: www.w3.org"]]}]"""
      r.http("https://httpbin.org/get", header = WithHttpHeader(
        HttpHeaderFieldsArray(
          "From: webmaster@w3.org" ::
          "Host: www.w3.org" ::
          ReNil
        )
      )) =%=[ReqlHttpResult] """[153,["https://httpbin.org/get"],{"header":[2,["From: webmaster@w3.org","Host: www.w3.org"]]}]"""
      r.http("https://httpbin.org/get", header = WithHttpHeader(
        HttpHeaderFieldsObject(Seq(
          new HttpHeaderPairField("From", "webmaster@w3.org"),
          new HttpHeaderPairField("Host", "www.w3.org")
        ))
      )) =%=[ReqlHttpResult] """[153,["https://httpbin.org/get"],{"header":{"From":"webmaster@w3.org","Host":"www.w3.org"}}]"""
      r.http("https://httpbin.org/get", header = WithHttpHeader(
        HttpHeaderFieldsObject(
          ("From" := "webmaster@w3.org") ~
          ("Host" := "www.w3.org")
        )
      )) =%=[ReqlHttpResult] """[153,["https://httpbin.org/get"],{"header":{"From":"webmaster@w3.org","Host":"www.w3.org"}}]"""

      r.http("https://httpbin.org/post", method = HttpPost, data = WithoutHttpData) =%=[ReqlHttpResult] """[153,["https://httpbin.org/post"],{"method":"POST"}]"""
      r.http("https://httpbin.org/post", method = HttpPost, data = WithHttpData(
        new HttpStringData("string data")
      )) =%=[ReqlHttpResult] """[153,["https://httpbin.org/post"],{"method":"POST","data":"string data"}]"""
      r.http("https://httpbin.org/post", method = HttpPost, data = WithHttpData(
        HttpFormFieldsObject(Seq(
          new HttpNumberFormField("abc", 123),
          new HttpStringFormField("bcd", "cde")
        ))
      )) =%=[ReqlHttpResult] """[153,["https://httpbin.org/post"],{"method":"POST","data":{"abc":123,"bcd":"cde"}}]"""
      r.http("https://httpbin.org/post", method = HttpPost, data = WithHttpData(
        HttpFormFieldsObject(
          ("abc" := 123) ~
          ("bcd" := "cde")
        )
      )) =%=[ReqlHttpResult] """[153,["https://httpbin.org/post"],{"method":"POST","data":{"abc":123,"bcd":"cde"}}]"""

      //from docs
      r.http("http://httpbin.org/post",
        method = HttpPost,
        data = WithHttpData(HttpFormFieldsObject(
          ("player" := "Bob") ~
          ("game" := "tic tac toe")
        ))
      ) =%=[ReqlHttpResult] """[153,["http://httpbin.org/post"],{"method":"POST","data":{"player":"Bob","game":"tic tac toe"}}]"""

      //pagination
      r.http("https://api.github.com/search/code?q=addClass+user:mozilla", pagination = WithoutHttpPagination) =%=[ReqlHttpResult] """[153,["https://api.github.com/search/code?q=addClass+user:mozilla"]]"""
      r.http("https://api.github.com/search/code?q=addClass+user:mozilla", pagination = WithHttpPagination(
        page = LinkNextStrategy,
        pageLimit = RequestsLimit(3)
      )) =%=[ReqlHttpResult] """[153,["https://api.github.com/search/code?q=addClass+user:mozilla"],{"page":"link-next","page_limit":3}]"""

      r.http("https://api.github.com/search/code?q=addClass+user:mozilla", pagination = WithHttpPagination(
        page = PaginationStrategy(info =>
          info("body").asObject("meta").asObject("next").asString.default(r.expr(null).asString)
        ),
        pageLimit = RequestsLimit(5)
      )) =%=[ReqlHttpResult] """[153,["https://api.github.com/search/code?q=addClass+user:mozilla"],{"page":[69,[[2,[0]],[92,[[170,[[170,[[170,[[10,[0]],"body"]],"meta"]],"next"]],null]]]],"page_limit":5}]"""

      r.http("https://api.github.com/search/code?q=addClass+user:mozilla", pagination = WithHttpPagination(
        page = LinkNextStrategy,
        pageLimit = NoLimit
      )) =%=[ReqlHttpResult] """[153,["https://api.github.com/search/code?q=addClass+user:mozilla"],{"page":"link-next","page_limit":-1}]"""

      r.http("https://api.github.com/search/code?q=addClass+user:mozilla", pagination = WithHttpPagination(
        page = LinkNextStrategy,
        pageLimit = NoRequests
      )) =%=[ReqlHttpResult] """[153,["https://api.github.com/search/code?q=addClass+user:mozilla"],{"page":"link-next","page_limit":0}]"""
    }

    "error" in {
      r.error("impossible branch") =%=[ReqlError] """[12,["impossible branch"]]"""
    }

    /*"row" in {
      r.row =*= "[13,[]]"
      r.row("code") =*= """[170,[[13,[]],"code"]]"""
    }*/

    "db" in {
      r.db("test") =%=[ReqlDatabase] """[14,["test"]]"""
    }

    "table" in {
      //on db
      r.db("heroes").table("marvel") =%=[ReqlTable[JsonObject, String]] """[15,[[14,["heroes"]],"marvel"]]"""

      r.db("heroes").table("marvel", DefaultReadMode) =%=[ReqlTable[JsonObject, String]] """[15,[[14,["heroes"]],"marvel"]]"""
      r.db("heroes").table("marvel", Single) =%=[ReqlTable[JsonObject, String]] """[15,[[14,["heroes"]],"marvel"],{"read_mode":"single"}]"""
      r.db("heroes").table("marvel", Majority) =%=[ReqlTable[JsonObject, String]] """[15,[[14,["heroes"]],"marvel"],{"read_mode":"majority"}]"""
      r.db("heroes").table("marvel", Outdated) =%=[ReqlTable[JsonObject, String]] """[15,[[14,["heroes"]],"marvel"],{"read_mode":"outdated"}]"""

      r.db("heroes").table("marvel", identifierFormat = DefaultIdentifierFormat) =%=[ReqlTable[JsonObject, String]] """[15,[[14,["heroes"]],"marvel"]]"""
      r.db("heroes").table("marvel", identifierFormat = NameIdentifier) =%=[ReqlTable[JsonObject, String]] """[15,[[14,["heroes"]],"marvel"],{"identifier_format":"name"}]"""
      r.db("heroes").table("marvel", identifierFormat = UuidIdentifier) =%=[ReqlTable[JsonObject, String]] """[15,[[14,["heroes"]],"marvel"],{"identifier_format":"uuid"}]"""

      r.db("heroes").table("marvel", Majority, UuidIdentifier) =%=[ReqlTable[JsonObject, String]] """[15,[[14,["heroes"]],"marvel"],{"read_mode":"majority","identifier_format":"uuid"}]"""

      //on r
      r.table("marvel") =%=[ReqlTable[JsonObject, String]] """[15,["marvel"]]"""

      r.table("marvel", DefaultReadMode) =%=[ReqlTable[JsonObject, String]] """[15,["marvel"]]"""
      r.table("marvel", Single) =%=[ReqlTable[JsonObject, String]] """[15,["marvel"],{"read_mode":"single"}]"""
      r.table("marvel", Majority) =%=[ReqlTable[JsonObject, String]] """[15,["marvel"],{"read_mode":"majority"}]"""
      r.table("marvel", Outdated) =%=[ReqlTable[JsonObject, String]] """[15,["marvel"],{"read_mode":"outdated"}]"""

      r.table("marvel", identifierFormat = DefaultIdentifierFormat) =%=[ReqlTable[JsonObject, String]] """[15,["marvel"]]"""
      r.table("marvel", identifierFormat = NameIdentifier) =%=[ReqlTable[JsonObject, String]] """[15,["marvel"],{"identifier_format":"name"}]"""
      r.table("marvel", identifierFormat = UuidIdentifier) =%=[ReqlTable[JsonObject, String]] """[15,["marvel"],{"identifier_format":"uuid"}]"""

      r.table("marvel", Majority, UuidIdentifier) =%=[ReqlTable[JsonObject, String]] """[15,["marvel"],{"read_mode":"majority","identifier_format":"uuid"}]"""
    }

    "get" in new ShapesData {
      jsonTable("posts").get("a9849eef-7176-4411-935b-79a6e3c56a74") =%=[ReqlSelectionOfObject[JsonObject, String]] """[16,[[15,["posts"]],"a9849eef-7176-4411-935b-79a6e3c56a74"]]"""

      jsonTable("heroes").get("uuid man") =%=[ReqlSelectionOfObject[JsonObject, String]] """[16,[[15,["heroes"]],"uuid man"]]"""

      // should compile
      r.table[JsonObject, String]("json").get("uuid")

      {
        import TestDatabase.abc

        val q = abc.table().get("uuid")
        val _: ReqlSelectionOfObject[Abc, String] = q
        q =*= """[16,[[15,[[14,["test"]],"abc"]],"uuid"]]"""

        """abc.table().get(123)""".shouldNot(compile)
      }
    }

    "getAll" in {
      r.table[JsonObject, String]("marvel").getAll(Index("code_name"), "man_of_steel") =%=[ReqlSelectionOfStream[JsonObject, String]] """[78,[[15,["marvel"]],"man_of_steel"],{"index":"code_name"}]"""

      r.table[JsonObject, String]("dc").getAll("superman") =%=[ReqlSelectionOfStream[JsonObject, String]] """[78,[[15,["dc"]],"superman"]]"""

      r.table[JsonObject, String]("dc").getAll("superman", "ant man") =%=[ReqlSelectionOfStream[JsonObject, String]] """[78,[[15,["dc"]],"superman","ant man"]]"""

      """r.table("dc").getAll("superman").getAll("ant man")""".shouldNot(compile)

      //with .args
      r.table[JsonObject, String]("abc").getAll(r.args(Seq(
        r.expr("Alice"), r.expr("Bob")
      ))) =%=[ReqlSelectionOfStream[JsonObject, String]] """[78,[[15,["abc"]],[154,[[2,["Alice","Bob"]]]]]]"""

      r.table[JsonObject, String]("abc").getAll(Index("code"), r.args(Seq(
        r.expr("Alice"), r.expr("Bob")
      ))) =%=[ReqlSelectionOfStream[JsonObject, String]] """[78,[[15,["abc"]],[154,[[2,["Alice","Bob"]]]]],{"index":"code"}]"""
    }

    "eq" in {
      r.eq_(Seq(r.expr(123), r.expr(234), r.expr(345))) =%=[ReqlBoolean] "[17,[123,234,345]]"

      r.expr(123).eq_(r.expr(234)) =%=[ReqlBoolean] "[17,[123,234]]"

      r.expr(123).eq_(Seq(r.expr(234), r.expr(345))) =%=[ReqlBoolean] "[17,[123,234,345]]"

      r.expr("a").eq_(Seq(r.expr("b"), r.expr("c"))) =%=[ReqlBoolean] """[17,["a","b","c"]]"""
    }

    "ne" in {
      r.ne_(Seq(r.expr(123), r.expr(234), r.expr(345))) =%=[ReqlBoolean] "[18,[123,234,345]]"

      r.expr(123).ne_(r.expr(234)) =%=[ReqlBoolean] "[18,[123,234]]"

      r.expr(123).ne_(Seq(r.expr(234), r.expr(345))) =%=[ReqlBoolean] "[18,[123,234,345]]"

      r.expr("a").ne_(Seq(r.expr("b"), r.expr("c"))) =%=[ReqlBoolean] """[18,["a","b","c"]]"""
    }

    "lt" in {
      r.lt(Seq(123, r.expr(234), 345)) =%=[ReqlBoolean] "[19,[123,234,345]]"

      r.expr(123).lt(r.expr(234)) =%=[ReqlBoolean] "[19,[123,234]]"

      r.expr(123).lt(Seq(r.expr(234), r.expr(345))) =%=[ReqlBoolean] "[19,[123,234,345]]"

      r.expr("a").lt(Seq(r.expr("b"), r.expr("c"))) =%=[ReqlBoolean] """[19,["a","b","c"]]"""
    }

    "le" in {
      r.le(Seq(123, r.expr(234), 345)) =%=[ReqlBoolean] "[20,[123,234,345]]"

      r.expr(123).le(r.expr(234)) =%=[ReqlBoolean] "[20,[123,234]]"

      r.expr(123).le(Seq(r.expr(234), r.expr(345))) =%=[ReqlBoolean] "[20,[123,234,345]]"

      r.expr("a").le(Seq(r.expr("b"), r.expr("c"))) =%=[ReqlBoolean] """[20,["a","b","c"]]"""
    }

    "gt" in {
      r.gt(Seq(123, r.expr(234), 345)) =%=[ReqlBoolean] "[21,[123,234,345]]"

      r.expr(123).gt(r.expr(234)) =%=[ReqlBoolean] "[21,[123,234]]"

      r.expr(123).gt(Seq(r.expr(234), r.expr(345))) =%=[ReqlBoolean] "[21,[123,234,345]]"

      r.expr("a").gt(Seq(r.expr("b"), r.expr("c"))) =%=[ReqlBoolean] """[21,["a","b","c"]]"""
    }

    "ge" in {
      r.ge(Seq(123, r.expr(234), 345)) =%=[ReqlBoolean] "[22,[123,234,345]]"

      r.expr(123).ge(r.expr(234)) =%=[ReqlBoolean] "[22,[123,234]]"

      r.expr(123).ge(Seq(r.expr(234), r.expr(345))) =%=[ReqlBoolean] "[22,[123,234,345]]"

      r.expr("a").ge(Seq(r.expr("b"), r.expr("c"))) =%=[ReqlBoolean] """[22,["a","b","c"]]"""
    }

    "not" in {
      r.not(false) =%=[ReqlBoolean] "[23,[false]]"
      r.not(true) =%=[ReqlBoolean] "[23,[true]]"

      r.expr(false).not() =%=[ReqlBoolean] "[23,[false]]"
      r.expr(true).not() =%=[ReqlBoolean] "[23,[true]]"

      r.not(false).not() =%=[ReqlBoolean] "[23,[[23,[false]]]]"
      r.expr(false).not().not() =%=[ReqlBoolean] "[23,[[23,[false]]]]"

      r.expr(123).gt(234).not() =%=[ReqlBoolean] "[23,[[21,[123,234]]]]"

      r.eq_(Seq(
        r.expr(123).le(234),
        r.ge(Seq(123, 234)).not()
      )) =%=[ReqlBoolean] "[17,[[20,[123,234]],[23,[[22,[123,234]]]]]]"

      r.not(r.gt(Seq(123, 234))) =%=[ReqlBoolean] "[23,[[21,[123,234]]]]"
    }

    "add" in {
      "r.add()".shouldNot(compile)
      "r.expr(123).add()".should(compile)

      //numbers
      //integer
      r.expr(123).add() =%=[ReqlInteger] "[24,[123]]"
      r.expr(123).add(234) =%=[ReqlInteger] "[24,[123,234]]"
      r.expr(123).add(234, 345) =%=[ReqlInteger] "[24,[123,234,345]]"

      //float
      r.expr(BigDecimal(123.123)).add() =%=[ReqlFloat] "[24,[123.123]]"
      r.expr(BigDecimal(123.123)).add(BigDecimal(234.234)) =%=[ReqlFloat] "[24,[123.123,234.234]]"
      r.expr(BigDecimal(123.123)).add(BigDecimal(234.234), BigDecimal(345.345)) =%=[ReqlFloat] "[24,[123.123,234.234,345.345]]"

      //mixed
      r.expr(123).add(BigDecimal(234.234)) =%=[ReqlFloat] "[24,[123,234.234]]"
      r.expr(123).add(BigDecimal(234.234), BigDecimal(345.345)) =%=[ReqlFloat] "[24,[123,234.234,345.345]]"
      r.expr(123).add(BigDecimal(234.234), 345) =%=[ReqlFloat] "[24,[123,234.234,345]]"
      r.expr(123).add(234, BigDecimal(345.345)) =%=[ReqlFloat] "[24,[123,234,345.345]]"
      r.expr(BigDecimal(123.123)).add(234) =%=[ReqlFloat] "[24,[123.123,234]]"
      r.expr(BigDecimal(123.123)).add(234, 345) =%=[ReqlFloat] "[24,[123.123,234,345]]"
      r.expr(BigDecimal(123.123)).add(234, BigDecimal(345.345)) =%=[ReqlFloat] "[24,[123.123,234,345.345]]"
      r.expr(BigDecimal(123.123)).add(BigDecimal(234.234), 345) =%=[ReqlFloat] "[24,[123.123,234.234,345]]"

      //on r
      //integer
      r.add(123) =%=[ReqlInteger] "[24,[123]]"
      r.add(123, 234) =%=[ReqlInteger] "[24,[123,234]]"
      r.add(123, 234, 345) =%=[ReqlInteger] "[24,[123,234,345]]"

      //float
      r.add(BigDecimal(123.123)) =%=[ReqlFloat] "[24,[123.123]]"
      r.add(BigDecimal(123.123), BigDecimal(234.234)) =%=[ReqlFloat] "[24,[123.123,234.234]]"
      r.add(BigDecimal(123.123), BigDecimal(234.234), BigDecimal(345.345)) =%=[ReqlFloat] "[24,[123.123,234.234,345.345]]"

      //mixed
      r.add(123, BigDecimal(234.234)) =%=[ReqlFloat] "[24,[123,234.234]]"
      r.add(123, BigDecimal(234.234), BigDecimal(345.345)) =%=[ReqlFloat] "[24,[123,234.234,345.345]]"
      r.add(123, BigDecimal(234.234), 345) =%=[ReqlFloat] "[24,[123,234.234,345]]"
      r.add(123, 234, BigDecimal(345.345)) =%=[ReqlFloat] "[24,[123,234,345.345]]"
      r.add(BigDecimal(123.123), 234) =%=[ReqlFloat] "[24,[123.123,234]]"
      r.add(BigDecimal(123.123), 234, 345) =%=[ReqlFloat] "[24,[123.123,234,345]]"
      r.add(BigDecimal(123.123), 234, BigDecimal(345.345)) =%=[ReqlFloat] "[24,[123.123,234,345.345]]"
      r.add(BigDecimal(123.123), BigDecimal(234.234), 345) =%=[ReqlFloat] "[24,[123.123,234.234,345]]"

      //complex
      r.add(123).add(234) =%=[ReqlInteger] "[24,[[24,[123]],234]]"
      r.add(r.add(123)) =%=[ReqlInteger] "[24,[[24,[123]]]]"
      r.expr(123).add(r.add(234, 345)) =%=[ReqlInteger] "[24,[123,[24,[234,345]]]]"

      //strings
      r.expr("abc").add() =%=[ReqlString] """[24,["abc"]]"""
      r.expr("abc").add("bcd") =%=[ReqlString] """[24,["abc","bcd"]]"""
      r.expr("abc").add("bcd", "cde") =%=[ReqlString] """[24,["abc","bcd","cde"]]"""

      r.add("abc") =%=[ReqlString] """[24,["abc"]]"""
      r.add("abc", "bcd") =%=[ReqlString] """[24,["abc","bcd"]]"""
      r.add("abc", "bcd", "cde") =%=[ReqlString] """[24,["abc","bcd","cde"]]"""

      r.add("abc").add("bcd") =%=[ReqlString] """[24,[[24,["abc"]],"bcd"]]"""
      r.add(r.add("abc")) =%=[ReqlString] """[24,[[24,["abc"]]]]"""
      r.expr("abc").add(r.add("bcd", "cde")) =%=[ReqlString] """[24,["abc",[24,["bcd","cde"]]]]"""

      //arrays
      r.expr(Seq(1, 2, 3): Seq[ReqlInteger]).add() =%=[ReqlArray[ReqlInteger]] """[24,[[2,[1,2,3]]]]"""

      r.expr(Seq(1, 2, 3): Seq[ReqlInteger])
        .add(Seq(4, 5, 6): Seq[ReqlInteger]) =%=[ReqlArray[ReqlInteger]] """[24,[[2,[1,2,3]],[2,[4,5,6]]]]"""

      r.expr(Seq(1, 2, 3): Seq[ReqlInteger])
        .add(
          Seq(4, 5, 6): Seq[ReqlInteger],
          Seq(7, 8, 9): Seq[ReqlInteger]
        ) =%=[ReqlArray[ReqlInteger]] """[24,[[2,[1,2,3]],[2,[4,5,6]],[2,[7,8,9]]]]"""

      r.add(Seq(1, 2, 3): Seq[ReqlInteger]) =%=[ReqlArray[ReqlInteger]] """[24,[[2,[1,2,3]]]]"""

      r.add(
        Seq(1, 2, 3): Seq[ReqlInteger],
        Seq(4, 5, 6): Seq[ReqlInteger]
      ) =%=[ReqlArray[ReqlInteger]] """[24,[[2,[1,2,3]],[2,[4,5,6]]]]"""

      r.add(
        Seq(1, 2, 3): Seq[ReqlInteger],
        Seq(4, 5, 6): Seq[ReqlInteger],
        Seq(7, 8, 9): Seq[ReqlInteger]
      ) =%=[ReqlArray[ReqlInteger]] """[24,[[2,[1,2,3]],[2,[4,5,6]],[2,[7,8,9]]]]"""

      r.add(Seq(1, 2, 3): Seq[ReqlInteger])
        .add(Seq(4, 5, 6): Seq[ReqlInteger]) =%=[ReqlArray[ReqlInteger]] """[24,[[24,[[2,[1,2,3]]]],[2,[4,5,6]]]]"""

      r.add(r.add(Seq(1, 2, 3): Seq[ReqlInteger])) =%=[ReqlArray[ReqlInteger]] """[24,[[24,[[2,[1,2,3]]]]]]"""

      r.expr(Seq(1, 2, 3): Seq[ReqlInteger])
        .add(r.add(
          Seq(4, 5, 6): Seq[ReqlInteger],
          Seq(7, 8, 9): Seq[ReqlInteger])
        ) =%=[ReqlArray[ReqlInteger]] """[24,[[2,[1,2,3]],[24,[[2,[4,5,6]],[2,[7,8,9]]]]]]"""

      //time
      r.now().add() =%=[ReqlTime] "[24,[[103,[]]]]"
      r.now().add(123) =%=[ReqlTime] "[24,[[103,[]],123]]"
      r.now().add(123, 234) =%=[ReqlTime] "[24,[[103,[]],123,234]]"

      r.add(r.now()) =%=[ReqlTime] "[24,[[103,[]]]]"
      r.add(r.now(), 123) =%=[ReqlTime] "[24,[[103,[]],123]]"
      r.add(r.now(), 123, 234) =%=[ReqlTime] "[24,[[103,[]],123,234]]"

      "r.add(r.now()).add(r.now())".shouldNot(compile)
      r.add(r.now()).add(123) =%=[ReqlTime] """[24,[[24,[[103,[]]]],123]]"""
      r.add(r.add(r.now())) =%=[ReqlTime] """[24,[[24,[[103,[]]]]]]"""
      r.now().add(r.add(123, 234)) =%=[ReqlTime] """[24,[[103,[]],[24,[123,234]]]]"""
    }

    "sub" in {
      "r.sub()".shouldNot(compile)
      "r.expr(123).sub()".should(compile)

      //numbers
      //integer
      r.expr(123).sub() =%=[ReqlInteger] "[25,[123]]"
      r.expr(123).sub(234) =%=[ReqlInteger] "[25,[123,234]]"
      r.expr(123).sub(234, 345) =%=[ReqlInteger] "[25,[123,234,345]]"

      //float
      r.expr(BigDecimal(123.123)).sub() =%=[ReqlFloat] "[25,[123.123]]"
      r.expr(BigDecimal(123.123)).sub(BigDecimal(234.234)) =%=[ReqlFloat] "[25,[123.123,234.234]]"
      r.expr(BigDecimal(123.123)).sub(BigDecimal(234.234), BigDecimal(345.345)) =%=[ReqlFloat] "[25,[123.123,234.234,345.345]]"

      //mixed
      r.expr(123).sub(BigDecimal(234.234)) =%=[ReqlFloat] "[25,[123,234.234]]"
      r.expr(123).sub(BigDecimal(234.234), BigDecimal(345.345)) =%=[ReqlFloat] "[25,[123,234.234,345.345]]"
      r.expr(123).sub(BigDecimal(234.234), 345) =%=[ReqlFloat] "[25,[123,234.234,345]]"
      r.expr(123).sub(234, BigDecimal(345.345)) =%=[ReqlFloat] "[25,[123,234,345.345]]"
      r.expr(BigDecimal(123.123)).sub(234) =%=[ReqlFloat] "[25,[123.123,234]]"
      r.expr(BigDecimal(123.123)).sub(234, 345) =%=[ReqlFloat] "[25,[123.123,234,345]]"
      r.expr(BigDecimal(123.123)).sub(234, BigDecimal(345.345)) =%=[ReqlFloat] "[25,[123.123,234,345.345]]"
      r.expr(BigDecimal(123.123)).sub(BigDecimal(234.234), 345) =%=[ReqlFloat] "[25,[123.123,234.234,345]]"

      //on r
      //integer
      r.sub(123) =%=[ReqlInteger] "[25,[123]]"
      r.sub(123, 234) =%=[ReqlInteger] "[25,[123,234]]"
      r.sub(123, 234, 345) =%=[ReqlInteger] "[25,[123,234,345]]"

      //float
      r.sub(BigDecimal(123.123)) =%=[ReqlFloat] "[25,[123.123]]"
      r.sub(BigDecimal(123.123), BigDecimal(234.234)) =%=[ReqlFloat] "[25,[123.123,234.234]]"
      r.sub(BigDecimal(123.123), BigDecimal(234.234), BigDecimal(345.345)) =%=[ReqlFloat] "[25,[123.123,234.234,345.345]]"

      //mixed
      r.sub(123, BigDecimal(234.234)) =%=[ReqlFloat] "[25,[123,234.234]]"
      r.sub(123, BigDecimal(234.234), BigDecimal(345.345)) =%=[ReqlFloat] "[25,[123,234.234,345.345]]"
      r.sub(123, BigDecimal(234.234), 345) =%=[ReqlFloat] "[25,[123,234.234,345]]"
      r.sub(123, 234, BigDecimal(345.345)) =%=[ReqlFloat] "[25,[123,234,345.345]]"
      r.sub(BigDecimal(123.123), 234) =%=[ReqlFloat] "[25,[123.123,234]]"
      r.sub(BigDecimal(123.123), 234, 345) =%=[ReqlFloat] "[25,[123.123,234,345]]"
      r.sub(BigDecimal(123.123), 234, BigDecimal(345.345)) =%=[ReqlFloat] "[25,[123.123,234,345.345]]"
      r.sub(BigDecimal(123.123), BigDecimal(234.234), 345) =%=[ReqlFloat] "[25,[123.123,234.234,345]]"

      //complex
      r.sub(123).sub(234) =%=[ReqlInteger] "[25,[[25,[123]],234]]"
      r.sub(r.sub(123)) =%=[ReqlInteger] "[25,[[25,[123]]]]"
      r.expr(123).sub(r.sub(234, 345)) =%=[ReqlInteger] "[25,[123,[25,[234,345]]]]"

      //time
      r.now().sub() =%=[ReqlTime] "[25,[[103,[]]]]"
      r.now().sub(1) =%=[ReqlTime] "[25,[[103,[]],1]]"
      r.now().sub(1, 2) =%=[ReqlTime] "[25,[[103,[]],1,2]]"

      r.sub(r.now()) =%=[ReqlTime] "[25,[[103,[]]]]"
      r.sub(r.now(), 1) =%=[ReqlTime] "[25,[[103,[]],1]]"
      r.sub(r.now(), 1, 2) =%=[ReqlTime] "[25,[[103,[]],1,2]]"

      //complex time
      r.now().sub(r.now()) =%=[ReqlFloat] "[25,[[103,[]],[103,[]]]]"
      "r.now().sub(r.now(), 1)".shouldNot(compile)
      r.now().sub(r.now()).sub(1, 2) =%=[ReqlFloat] "[25,[[25,[[103,[]],[103,[]]]],1,2]]"

      r.sub(r.now(), r.now()) =%=[ReqlFloat] "[25,[[103,[]],[103,[]]]]"
      "r.sub(r.now(), r.now(), r.now())".shouldNot(compile)
    }

    "mul" in {
      //number
      //integer
      r.expr(123).mul() =%=[ReqlInteger] "[26,[123]]"
      r.expr(123).mul(234) =%=[ReqlInteger] "[26,[123,234]]"
      r.expr(123).mul(234, 345) =%=[ReqlInteger] "[26,[123,234,345]]"

      //float
      r.expr(BigDecimal(123.123)).mul() =%=[ReqlFloat] "[26,[123.123]]"
      r.expr(BigDecimal(123.123)).mul(BigDecimal(234.234)) =%=[ReqlFloat] "[26,[123.123,234.234]]"
      r.expr(BigDecimal(123.123)).mul(BigDecimal(234.234), BigDecimal(345.345)) =%=[ReqlFloat] "[26,[123.123,234.234,345.345]]"

      //mixed
      r.expr(123).mul(BigDecimal(234.234)) =%=[ReqlFloat] "[26,[123,234.234]]"
      r.expr(123).mul(BigDecimal(234.234), BigDecimal(345.345)) =%=[ReqlFloat] "[26,[123,234.234,345.345]]"
      r.expr(123).mul(BigDecimal(234.234), 345) =%=[ReqlFloat] "[26,[123,234.234,345]]"
      r.expr(123).mul(234, BigDecimal(345.345)) =%=[ReqlFloat] "[26,[123,234,345.345]]"
      r.expr(BigDecimal(123.123)).mul(234) =%=[ReqlFloat] "[26,[123.123,234]]"
      r.expr(BigDecimal(123.123)).mul(234, 345) =%=[ReqlFloat] "[26,[123.123,234,345]]"
      r.expr(BigDecimal(123.123)).mul(234, BigDecimal(345.345)) =%=[ReqlFloat] "[26,[123.123,234,345.345]]"
      r.expr(BigDecimal(123.123)).mul(BigDecimal(234.234), 345) =%=[ReqlFloat] "[26,[123.123,234.234,345]]"

      //array
      r.expr(Seq(r.expr(123), r.expr(234), r.expr(345))).mul() =%=[ReqlArray[ReqlInteger]] "[26,[[2,[123,234,345]]]]"
      r.expr(Seq(r.expr(123), r.expr(234), r.expr(345))).mul(6) =%=[ReqlArray[ReqlInteger]] "[26,[[2,[123,234,345]],6]]"
      r.expr(Seq(r.expr(123), r.expr(234), r.expr(345))).mul(6, 7) =%=[ReqlArray[ReqlInteger]] "[26,[[2,[123,234,345]],6,7]]"

      "r.expr(Seq(r.expr(123), r.expr(234), r.expr(345))).mul(BigDecimal(2.5))".shouldNot(compile)
    }

    "div" in {
      //integer
      r.expr(123).div() =%=[ReqlFloat] "[27,[123]]"
      r.expr(123).div(234) =%=[ReqlFloat] "[27,[123,234]]"
      r.expr(123).div(234, 345) =%=[ReqlFloat] "[27,[123,234,345]]"

      //float
      r.expr(BigDecimal(123.123)).div() =%=[ReqlFloat] "[27,[123.123]]"
      r.expr(BigDecimal(123.123)).div(BigDecimal(234.234)) =%=[ReqlFloat] "[27,[123.123,234.234]]"
      r.expr(BigDecimal(123.123)).div(BigDecimal(234.234), BigDecimal(345.345)) =%=[ReqlFloat] "[27,[123.123,234.234,345.345]]"

      //mixed
      r.expr(123).div(BigDecimal(234.234)) =%=[ReqlFloat] "[27,[123,234.234]]"
      r.expr(123).div(BigDecimal(234.234), BigDecimal(345.345)) =%=[ReqlFloat] "[27,[123,234.234,345.345]]"
      r.expr(123).div(BigDecimal(234.234), 345) =%=[ReqlFloat] "[27,[123,234.234,345]]"
      r.expr(123).div(234, BigDecimal(345.345)) =%=[ReqlFloat] "[27,[123,234,345.345]]"
      r.expr(BigDecimal(123.123)).div(234) =%=[ReqlFloat] "[27,[123.123,234]]"
      r.expr(BigDecimal(123.123)).div(234, 345) =%=[ReqlFloat] "[27,[123.123,234,345]]"
      r.expr(BigDecimal(123.123)).div(234, BigDecimal(345.345)) =%=[ReqlFloat] "[27,[123.123,234,345.345]]"
      r.expr(BigDecimal(123.123)).div(BigDecimal(234.234), 345) =%=[ReqlFloat] "[27,[123.123,234.234,345]]"
    }

    "mod" in {
      "r.expr(123).mod()".shouldNot(compile)
      r.expr(123).mod(7) =%=[ReqlInteger] "[28,[123,7]]"
      "r.expr(123).mod(7, 4)".shouldNot(compile)
      "r.expr(123).mod(4.3)".shouldNot(compile)
      "r.expr(123.2).mod(4)".shouldNot(compile)
    }

    "floor" in {
      r.floor(123) =%=[ReqlInteger] "[183,[123]]"
      r.expr(123).floor() =%=[ReqlInteger] "[183,[123]]"
      "r.expr(123).floor(234)".shouldNot(compile)
    }

    "ceil" in {
      r.ceil(123) =%=[ReqlInteger] "[184,[123]]"
      r.expr(123).ceil() =%=[ReqlInteger] "[184,[123]]"
      "r.expr(123).ceil(234)".shouldNot(compile)
    }

    "round" in {
      r.round(BigDecimal(12.345)) =%=[ReqlInteger] "[185,[12.345]]"
      r.expr(BigDecimal(-12.345)).round() =%=[ReqlInteger] "[185,[-12.345]]"
      "r.expr(123).round(234)".shouldNot(compile)
    }

    "append" in {
      r.expr(Seq(r.expr(123), r.expr(234))).append(r.expr(345)) =%=[ReqlArray[ReqlInteger]] "[29,[[2,[123,234]],345]]"
      r.expr(Seq(r.expr("test"))).append(r.now()) =%=[ReqlArray[ReqlDatum]] """[29,[[2,["test"]],[103,[]]]]"""
      "r.expr(Seq(r.expr(123))).append(234, 345)".shouldNot(compile)
    }

    "prepend" in {
      r.expr(Seq(r.expr(123), r.expr(234))).prepend(r.expr(345)) =%=[ReqlArray[ReqlInteger]] "[80,[[2,[123,234]],345]]"
      r.expr(Seq(r.expr("test"))).prepend(r.now()) =%=[ReqlArray[ReqlDatum]] """[80,[[2,["test"]],[103,[]]]]"""
      "r.expr(Seq(r.expr(123))).prepend(234, 345)".shouldNot(compile)
    }

    "difference" in {
      r.expr(Seq(r.expr(123), r.expr(234), r.expr(345)))
        .difference(r.expr(Seq(r.expr(234)))) =%=[ReqlArray[ReqlInteger]] "[95,[[2,[123,234,345]],[2,[234]]]]"
      "r.expr(Seq(r.expr(123), r.expr(234), r.expr(345))).difference(123)".shouldNot(compile)
    }

    "setInsert" in {
      r.expr(Seq(r.expr(123), r.expr(234))).setInsert(123) =%=[ReqlArray[ReqlInteger]] "[88,[[2,[123,234]],123]]"
      "r.expr(Seq(r.expr(123), r.expr(234))).setInsert(123,234)".shouldNot(compile)
    }

    "setIntersection" in {
      r.expr(Seq(r.expr(123), r.expr(234), r.expr(345)))
        .setIntersection(r.expr(Seq(r.expr(234), r.expr(456)))) =%=[ReqlArray[ReqlInteger]] "[89,[[2,[123,234,345]],[2,[234,456]]]]"
      "r.expr(Seq(r.expr(123), r.expr(234))).setIntersection(123)".shouldNot(compile)
    }

    "setUnion" in {
      r.expr(Seq(r.expr(123), r.expr(234), r.expr(345)))
        .setUnion(r.expr(Seq(r.expr(234), r.expr(456)))) =%=[ReqlArray[ReqlInteger]] "[90,[[2,[123,234,345]],[2,[234,456]]]]"
      "r.expr(Seq(r.expr(123), r.expr(234))).setUnion(123)".shouldNot(compile)
    }

    "setDifference" in {
      r.expr(Seq(r.expr(123), r.expr(234), r.expr(345)))
        .setDifference(r.expr(Seq(r.expr(234), r.expr(456)))) =%=[ReqlArray[ReqlInteger]] "[91,[[2,[123,234,345]],[2,[234,456]]]]"
      "r.expr(Seq(r.expr(123), r.expr(234))).setDifference(123)".shouldNot(compile)
    }

    "slice" in {

      //TODO: make options visible without additional imports

      //on array
      r.expr(Seq(r.expr(123), r.expr(234))).slice(1) =%=[ReqlArray[ReqlInteger]] "[30,[[2,[123,234]],1]]"
      r.expr(Seq(r.expr(123), r.expr(234))).slice(1, 2) =%=[ReqlArray[ReqlInteger]] "[30,[[2,[123,234]],1,2]]"
      r.expr(Seq(r.expr(123), r.expr(234))).slice(1, Bounds(DefaultBound, DefaultBound)) =%=[ReqlArray[ReqlInteger]] "[30,[[2,[123,234]],1]]"
      r.expr(Seq(r.expr(123), r.expr(234))).slice(1, 2, Bounds(DefaultBound, DefaultBound)) =%=[ReqlArray[ReqlInteger]] "[30,[[2,[123,234]],1,2]]"

      "r.expr(Seq(r.expr(123), r.expr(234))).slice(1.1)".shouldNot(compile)
      "r.expr(Seq(r.expr(123), r.expr(234))).slice(1, 2.1)".shouldNot(compile)

      //bounds tests
      r.expr(Seq(r.expr(123), r.expr(234)))
        .slice(1, Bounds(DefaultBound, DefaultBound)) =%=[ReqlArray[ReqlInteger]] "[30,[[2,[123,234]],1]]"

      r.expr(Seq(r.expr(123), r.expr(234)))
        .slice(1, Bounds(OpenBound, DefaultBound)) =%=[ReqlArray[ReqlInteger]] """[30,[[2,[123,234]],1],{"left_bound":"open"}]"""

      r.expr(Seq(r.expr(123), r.expr(234)))
        .slice(1, Bounds(ClosedBound, DefaultBound)) =%=[ReqlArray[ReqlInteger]] """[30,[[2,[123,234]],1],{"left_bound":"closed"}]"""

      r.expr(Seq(r.expr(123), r.expr(234)))
        .slice(1, Bounds(DefaultBound, OpenBound)) =%=[ReqlArray[ReqlInteger]] """[30,[[2,[123,234]],1],{"right_bound":"open"}]"""

      r.expr(Seq(r.expr(123), r.expr(234)))
        .slice(1, Bounds(DefaultBound, ClosedBound)) =%=[ReqlArray[ReqlInteger]] """[30,[[2,[123,234]],1],{"right_bound":"closed"}]"""

      r.expr(Seq(r.expr(123), r.expr(234)))
        .slice(1, Bounds(OpenBound, ClosedBound)) =%=[ReqlArray[ReqlInteger]] """[30,[[2,[123,234]],1],{"left_bound":"open","right_bound":"closed"}]"""

      r.expr(Seq(r.expr(123), r.expr(234)))
        .slice(1, Bounds(ClosedBound, OpenBound)) =%=[ReqlArray[ReqlInteger]] """[30,[[2,[123,234]],1],{"left_bound":"closed","right_bound":"open"}]"""

      //TODO: tests for other types
    }

    "skip" in {
      //table
      abcJsonTable.skip(10) =%=[ReqlSelectionOfStream[JsonObject, String]] """[70,[[15,["abc"]],10]]"""

      //tableSlice
      abcJsonTable.orderBy(OrderedIndex(r.asc("code"))) shouldBe an[ReqlTableSlice[JsonObject, String]]
      abcJsonTable.orderBy(OrderedIndex(r.asc("code"))).skip(10) =%=[ReqlSelectionOfStream[JsonObject, String]] """[70,[[41,[[15,["abc"]]],{"index":[73,["code"]]}],10]]"""

      //selectionOfArray
      abcJsonTable.orderBy(r.asc("code")) shouldBe an[ReqlSelectionOfArray[JsonObject, String]]
      abcJsonTable.orderBy(r.asc("code")).skip(10) =%=[ReqlSelectionOfArray[JsonObject, String]] """[70,[[41,[[15,["abc"]],[73,["code"]]]],10]]"""

      //selectionOfStream
      abcJsonTable.skip(10).skip(20) =%=[ReqlSelectionOfStream[JsonObject, String]] """[70,[[70,[[15,["abc"]],10]],20]]"""

      //infiniteStream
      r.range().skip(5) =%=[ReqlInfiniteStream[ReqlInteger]] """[70,[[173,[]],5]]"""

      //finiteStream
      r.range(10).skip(5) =%=[ReqlFiniteStream[ReqlInteger]] """[70,[[173,[10]],5]]"""

      //changefeed
      abcJsonTable.get("uuid").changes() shouldBe an[ReqlInfiniteStream[ReqlChangefeedNotification[JsonObject]]]
      abcJsonTable.get("uuid").changes().skip(10) =%=[ReqlInfiniteStream[ReqlChangefeedNotification[JsonObject]]] """[70,[[152,[[16,[[15,["abc"]],"uuid"]]]],10]]"""

      //array
      r.expr(Seq(r.expr(123), r.expr(234))).skip(10) =%=[ReqlArray[ReqlInteger]] "[70,[[2,[123,234]],10]]"
    }

    "limit" in {
      //table
      abcJsonTable.limit(10) =%=[ReqlSelectionOfStream[JsonObject, String]] """[71,[[15,["abc"]],10]]"""

      //tableSlice
      abcJsonTable.orderBy(OrderedIndex(r.asc("code"))).limit(10) =%=[ReqlSelectionOfStream[JsonObject, String]] """[71,[[41,[[15,["abc"]]],{"index":[73,["code"]]}],10]]"""

      //selectionOfArray
      abcJsonTable.orderBy(r.asc("code")).limit(10) =%=[ReqlSelectionOfArray[JsonObject, String]] """[71,[[41,[[15,["abc"]],[73,["code"]]]],10]]"""

      //selectionOfStream
      abcJsonTable.skip(10).limit(20) =%=[ReqlSelectionOfStream[JsonObject, String]] """[71,[[70,[[15,["abc"]],10]],20]]"""

      //infiniteStream
      r.range().limit(10) =%=[ReqlInfiniteStream[ReqlInteger]] """[71,[[173,[]],10]]"""

      //finiteStream
      r.range(20).limit(10) =%=[ReqlFiniteStream[ReqlInteger]] """[71,[[173,[20]],10]]"""

      //changefeed
      abcJsonTable.get("uuid").changes()
        .limit(10) =%=[ReqlInfiniteStream[ReqlChangefeedNotification[JsonObject]]] """[71,[[152,[[16,[[15,["abc"]],"uuid"]]]],10]]"""

      //array
      r.expr(Seq(r.expr(123), r.expr(234))).limit(10) =%=[ReqlArray[ReqlInteger]] "[71,[[2,[123,234]],10]]"
    }

    "offsets_of" in {
      //table
      r.table[JsonObject, String]("abc").offsetsOf(JsonObject.empty) =%=[ReqlFiniteStream[ReqlInteger]] """[87,[[15,["abc"]],{}]]"""
      r.table[JsonObject, String]("abc").offsetsOf(_ => true) =%=[ReqlFiniteStream[ReqlInteger]] """[87,[[15,["abc"]],[69,[[2,[0]],true]]]]"""

      //tableSlice
      r.table[JsonObject, String]("abc").orderBy(OrderedIndex(r.asc("code"))).offsetsOf(JsonObject.empty) =%=[ReqlFiniteStream[ReqlInteger]] """[87,[[41,[[15,["abc"]]],{"index":[73,["code"]]}],{}]]"""
      r.table[JsonObject, String]("abc").orderBy(OrderedIndex(r.asc("code"))).offsetsOf(_ => true) =%=[ReqlFiniteStream[ReqlInteger]] """[87,[[41,[[15,["abc"]]],{"index":[73,["code"]]}],[69,[[2,[0]],true]]]]"""

      //selectionOfArray
      r.table[JsonObject, String]("abc").orderBy(r.asc("code")).offsetsOf(JsonObject.empty) =%=[ReqlArray[ReqlInteger]] """[87,[[41,[[15,["abc"]],[73,["code"]]]],{}]]"""
      r.table[JsonObject, String]("abc").orderBy(r.asc("code")).offsetsOf(_ => true) =%=[ReqlArray[ReqlInteger]] """[87,[[41,[[15,["abc"]],[73,["code"]]]],[69,[[2,[0]],true]]]]"""

      //selectionOfStream
      r.table[JsonObject, String]("abc").skip(10).offsetsOf(JsonObject.empty) =%=[ReqlFiniteStream[ReqlInteger]] """[87,[[70,[[15,["abc"]],10]],{}]]"""
      r.table[JsonObject, String]("abc").skip(10).offsetsOf(_ => true) =%=[ReqlFiniteStream[ReqlInteger]] """[87,[[70,[[15,["abc"]],10]],[69,[[2,[0]],true]]]]"""

      //infiniteStream
      r.range().offsetsOf(20) =%=[ReqlInfiniteStream[ReqlInteger]] """[87,[[173,[]],20]]"""
      r.range().offsetsOf(_ => true) =%=[ReqlInfiniteStream[ReqlInteger]] """[87,[[173,[]],[69,[[2,[0]],true]]]]"""

      //finiteStream
      r.range(50).offsetsOf(20) =%=[ReqlFiniteStream[ReqlInteger]] """[87,[[173,[50]],20]]"""
      r.range(50).offsetsOf(_ => true) =%=[ReqlFiniteStream[ReqlInteger]] """[87,[[173,[50]],[69,[[2,[0]],true]]]]"""

      //changefeed
      //r.table[ReqlObject]("abc").get("uuid").changes().offsetsOf(JsonObject.empty) =%=[ReqlInfiniteStream[ReqlInteger]] """[87,[[152,[[16,[[15,["abc"]],"uuid"]]]],{}]]"""
      //TODO: maybe it should be easier
      val notification: ReqlChangefeedNotification[JsonObject] = ChangefeedNotification(
        Some(JsonObject.fromMap(Map("code" -> Json.fromInt(1)))),
        Some(JsonObject.fromMap(Map("code" -> Json.fromInt(2))))
      )
      abcJsonTable.get("uuid").changes().offsetsOf(notification) =%=[ReqlInfiniteStream[ReqlInteger]] """[87,[[152,[[16,[[15,["abc"]],"uuid"]]]],{"old_val":{"code":1},"new_val":{"code":2}}]]"""

      //array
      r.expr(Seq(r.expr(123), r.expr(234))).offsetsOf(10) =%=[ReqlArray[ReqlInteger]] """[87,[[2,[123,234]],10]]"""
      r.expr(Seq(r.expr(123), r.expr(234))).offsetsOf(_ => true) =%=[ReqlArray[ReqlInteger]] """[87,[[2,[123,234]],[69,[[2,[0]],true]]]]"""

      //complex predicate
      r.table[JsonObject, String]("abc").offsetsOf(_("name").eq_("value")) =%=[ReqlFiniteStream[ReqlInteger]] """[87,[[15,["abc"]],[69,[[2,[0]],[17,[[170,[[10,[0]],"name"]],"value"]]]]]]"""
    }

    "contains" in {
      //table
      abcJsonTable.contains(
        r.pred(JsonObject.fromMap(Map("code" -> Json.fromInt(123))): ReqlModel[JsonObject, String]),
        r.pred(JsonObject.fromMap(Map("code" -> Json.fromInt(234))): ReqlModel[JsonObject, String])
      ) =%=[ReqlBoolean] """[93,[[15,["abc"]],{"code":123},{"code":234}]]"""

      abcJsonTable.contains(r.pred(_.asObject("name").eq_("value"))) =%=[ReqlBoolean] """[93,[[15,["abc"]],[69,[[2,[0]],[17,[[170,[[10,[0]],"name"]],"value"]]]]]]"""

      abcJsonTable.contains(
        r.pred(JsonObject.fromMap(Map("code" -> Json.fromInt(123))): ReqlModel[JsonObject, String]),
        r.pred(_.asObject("name").eq_("value")),
        r.pred(JsonObject.fromMap(Map("code" -> Json.fromInt(234))): ReqlModel[JsonObject, String])
      ) =%=[ReqlBoolean] """[93,[[15,["abc"]],{"code":123},[69,[[2,[0]],[17,[[170,[[10,[0]],"name"]],"value"]]]],{"code":234}]]"""

      //tableSlice
      abcJsonTable.orderBy(OrderedIndex(r.asc("code"))).contains(
        r.pred(JsonObject.fromMap(Map("code" -> Json.fromInt(123))): ReqlModel[JsonObject, String]),
        r.pred(JsonObject.fromMap(Map("code" -> Json.fromInt(234))): ReqlModel[JsonObject, String])
      ) =%=[ReqlBoolean] """[93,[[41,[[15,["abc"]]],{"index":[73,["code"]]}],{"code":123},{"code":234}]]"""

      //selectionOfArray
      abcJsonTable.orderBy(r.asc("code")).contains(
        r.pred(JsonObject.fromMap(Map("code" -> Json.fromInt(123))): ReqlModel[JsonObject, String])
      ) =%=[ReqlBoolean] """[93,[[41,[[15,["abc"]],[73,["code"]]]],{"code":123}]]"""

      //selectionOfStream
      abcJsonTable.skip(10).contains(
        r.pred(JsonObject.fromMap(Map("code" -> Json.fromInt(123))): ReqlModel[JsonObject, String])
      ) =%=[ReqlBoolean] """[93,[[70,[[15,["abc"]],10]],{"code":123}]]"""

      //stream (Cannot call a terminal (`reduce`, `count`, etc.) on an infinite stream (such as a changefeed))
      abcJsonTable.get("uuid").changes() shouldBe an[ReqlInfiniteStream[ReqlChangefeedNotification[ReqlModel[JsonObject, String]]]]
      """r.table[ReqlObject]("abc").get("uuid").changes().contains(pred("code" := 123))""".shouldNot(compile)

      //array
      r.expr(Seq(r.expr(123), r.expr(234))).contains(r.expr(123)) =%=[ReqlBoolean] "[93,[[2,[123,234]],123]]"
    }

    "get_field" in {
      //table
      r.table("abc").getField("name") =%=[ReqlFiniteStream[ReqlDatum]] """[31,[[15,["abc"]],"name"]]"""

      //tableSlice
      r.table("abc").orderBy(OrderedIndex(r.asc("code"))).getField("name") =%=[ReqlFiniteStream[ReqlDatum]] """[31,[[41,[[15,["abc"]]],{"index":[73,["code"]]}],"name"]]"""

      //selectionOfArray
      r.table("abc").orderBy(r.asc("code")).getField("name") =%=[ReqlArray[ReqlInteger]] """[31,[[41,[[15,["abc"]],[73,["code"]]]],"name"]]"""

      //selectionOfStream
      r.table("abc").skip(10).getField("name") =%=[ReqlFiniteStream[ReqlDatum]] """[31,[[70,[[15,["abc"]],10]],"name"]]"""

      //infiniteStream
      //TODO: rewrite this example to .map
      r.range().getField("name") =%=[ReqlInfiniteStream[ReqlDatum]] """[31,[[173,[]],"name"]]"""

      //finiteStream
      r.range(5).getField("name") =%=[ReqlFiniteStream[ReqlDatum]] """[31,[[173,[5]],"name"]]"""

      //changefeed
      abcJsonTable.get("uuid").changes().getField("name") =%=[ReqlInfiniteStream[ReqlDatum]] """[31,[[152,[[16,[[15,["abc"]],"uuid"]]]],"name"]]"""

      //stream chain
      r.table("abc").getField("name").getField("first") =%=[ReqlFiniteStream[ReqlDatum]] """[31,[[31,[[15,["abc"]],"name"]],"first"]]"""

      //singleSelection
      abcJsonTable.get("uuid").getField("name") =%=[ReqlDatum] """[31,[[16,[[15,["abc"]],"uuid"]],"name"]]"""
      """abcJsonTable.get("uuid").getField("name").getField("first")""".shouldNot(compile)

      //object
      r.expr(Map("name" -> r.expr(123))).getField("name") =%=[ReqlDatum] """[31,[{"name":123},"name"]]"""
      """r.expr(Map("name" -> r.expr(123))).getField("name").getField("first")""".shouldNot(compile)
    }

    "keys" in {
      //singleSelection
      abcJsonTable.get("uuid").keys() =%=[ReqlArray[ReqlString]] """[94,[[16,[[15,["abc"]],"uuid"]]]]"""

      //object
      r.expr(Map("name" -> r.expr(123))).keys() =%=[ReqlArray[ReqlString]] """[94,[{"name":123}]]"""
    }

    "values" in {
      //singleSelection
      abcJsonTable.get("uuid").values() =%=[ReqlArray[ReqlDatum]] """[186,[[16,[[15,["abc"]],"uuid"]]]]"""

      //object
      r.expr(Map("name" -> r.expr(123))).values() =%=[ReqlArray[ReqlDatum]] """[186,[{"name":123}]]"""
    }

    "object" in {
      r.obj(
        r.expr("name") -> r.expr(123),
        r.expr("code") -> r.expr("abc")
      ) =%=[ReqlObject] """[143,["name",123,"code","abc"]]"""

      r.obj(
        r.expr("name") -> r.expr(123),
        r.expr("time") -> r.now()
      ) =%=[ReqlObject] """[143,["name",123,"time",[103,[]]]]"""

      /*r.obj(
        "name" -*> 123,
        "code" -*> "abc"
      ) =*= """[143,["name",123,"code","abc"]]"""*/
    }

    "has_fields" in {
      //table
      abcJsonTable.hasFields("name") =%=[ReqlSelectionOfStream[JsonObject, String]] """[32,[[15,["abc"]],"name"]]"""

      //table with .args
      abcJsonTable.hasFields(r.args(Seq(
        r.expr("name")
      ))) =%=[ReqlSelectionOfStream[JsonObject, String]] """[32,[[15,["abc"]],[154,[[2,["name"]]]]]]"""

      //tableSlice
      abcJsonTable.orderBy(OrderedIndex(r.asc("code"))).hasFields("name") =%=[ReqlSelectionOfStream[JsonObject, String]] """[32,[[41,[[15,["abc"]]],{"index":[73,["code"]]}],"name"]]"""

      //tableSlice with .args
      abcJsonTable.orderBy(OrderedIndex(r.asc("code"))).hasFields(r.args(Seq(
        r.expr("name")
      ))) =%=[ReqlSelectionOfStream[JsonObject, String]] """[32,[[41,[[15,["abc"]]],{"index":[73,["code"]]}],[154,[[2,["name"]]]]]]"""

      //selectionOfArray
      abcJsonTable.orderBy(r.asc("code")).hasFields("name") =%=[ReqlSelectionOfArray[JsonObject, String]] """[32,[[41,[[15,["abc"]],[73,["code"]]]],"name"]]"""

      //selectionOfArray with .args
      abcJsonTable.orderBy(r.asc("code")).hasFields(r.args(Seq(
        r.expr("name")
      ))) =%=[ReqlSelectionOfArray[JsonObject, String]] """[32,[[41,[[15,["abc"]],[73,["code"]]]],[154,[[2,["name"]]]]]]"""

      //selectionOfStream
      abcJsonTable.skip(10).hasFields("name") =%=[ReqlSelectionOfStream[JsonObject, String]] """[32,[[70,[[15,["abc"]],10]],"name"]]"""

      //selectionOfStream with .args
      abcJsonTable.skip(10).hasFields(r.args(Seq(
        r.expr("name")
      ))) =%=[ReqlSelectionOfStream[JsonObject, String]] """[32,[[70,[[15,["abc"]],10]],[154,[[2,["name"]]]]]]"""

      //infiniteStream
      r.range().hasFields("code") =%=[ReqlInfiniteStream[ReqlDatum]] """[32,[[173,[]],"code"]]"""

      //finiteStream
      //TODO: it should not compile
      r.range(10).hasFields("code") =%=[ReqlFiniteStream[ReqlInteger]] """[32,[[173,[10]],"code"]]"""

      //changefeed
      abcJsonTable.get("uuid").changes().hasFields("name") =%=[ReqlInfiniteStream[ReqlObject]] """[32,[[152,[[16,[[15,["abc"]],"uuid"]]]],"name"]]"""

      //stream with .args
      abcJsonTable.get("uuid").changes().hasFields(r.args(Seq(
        r.expr("name")
      ))) =%=[ReqlInfiniteStream[ReqlObject]] """[32,[[152,[[16,[[15,["abc"]],"uuid"]]]],[154,[[2,["name"]]]]]]"""

      //array
      r.expr(Seq(
        r.expr(JsonObject.empty), r.expr(JsonObject.empty)
      )).hasFields("code") =%=[ReqlArray[ReqlDatum]] """[32,[[2,[{},{}]],"code"]]"""

      //array with .args
      r.expr(Seq(
        r.expr(JsonObject.empty), r.expr(JsonObject.empty)
      )).hasFields(r.args(Seq(
        r.expr("name")
      ))) =%=[ReqlArray[ReqlDatum]] """[32,[[2,[{},{}]],[154,[[2,["name"]]]]]]"""

      //singleSelection
      abcJsonTable.get("uuid").hasFields("code") =%=[ReqlBoolean] """[32,[[16,[[15,["abc"]],"uuid"]],"code"]]"""

      //singleSelection with .args
      abcJsonTable.get("uuid").hasFields(r.args(Seq(
        r.expr("name")
      ))) =%=[ReqlBoolean] """[32,[[16,[[15,["abc"]],"uuid"]],[154,[[2,["name"]]]]]]"""

      //object
      r.expr(Map("code" -> r.expr(123))).hasFields("code") =%=[ReqlBoolean] """[32,[{"code":123},"code"]]"""

      //object with .args
      r.expr(Map("code" -> r.expr(123))).hasFields(r.args(Seq(
        r.expr("name")
      ))) =%=[ReqlBoolean] """[32,[{"code":123},[154,[[2,["name"]]]]]]"""
    }

    "with_fields" in {
      //table
      r.table("abc").withFields("name") =%=[ReqlFiniteStream[ReqlObject]] """[96,[[15,["abc"]],"name"]]"""

      //tableSlice
      r.table("abc").orderBy(OrderedIndex(r.asc("code"))).withFields("name") =%=[ReqlFiniteStream[ReqlObject]] """[96,[[41,[[15,["abc"]]],{"index":[73,["code"]]}],"name"]]"""

      //selectionOfArray
      r.table("abc").orderBy(r.asc("code")).withFields("name") =%=[ReqlArray[ReqlDatum]] """[96,[[41,[[15,["abc"]],[73,["code"]]]],"name"]]"""

      //selectionOfStream
      r.table("abc").skip(10).withFields("name") =%=[ReqlFiniteStream[ReqlObject]] """[96,[[70,[[15,["abc"]],10]],"name"]]"""

      //infiniteStream
      //TODO: it should not compile
      r.range().withFields("name") =%=[ReqlInfiniteStream[ReqlInteger]] """[96,[[173,[]],"name"]]"""

      //finiteStream
      //TODO: it should not compile
      r.range(10).withFields("name") =%=[ReqlFiniteStream[ReqlInteger]] """[96,[[173,[10]],"name"]]"""

      //changefeed
      abcJsonTable.get("uuid").changes().withFields("name") =%=[ReqlInfiniteStream[ReqlObject]] """[96,[[152,[[16,[[15,["abc"]],"uuid"]]]],"name"]]"""

      //array
      r.expr(Seq(
        r.expr(JsonObject.empty), r.expr(JsonObject.empty)
      )).withFields("code") =%=[ReqlArray[ReqlDatum]] """[96,[[2,[{},{}]],"code"]]"""
    }

    "pluck" in {
      //table
      r.table("abc").pluck("name", "code") =%=[ReqlFiniteStream[ReqlObject]] """[33,[[15,["abc"]],"name","code"]]"""

      //tableSlice
      r.table("abc").orderBy(OrderedIndex(r.asc("code"))).pluck("name", "code") =%=[ReqlFiniteStream[ReqlObject]] """[33,[[41,[[15,["abc"]]],{"index":[73,["code"]]}],"name","code"]]"""

      //selectionOfArray
      r.table("abc").orderBy(r.asc("code")).pluck("name", "code") =%=[ReqlArray[ReqlDatum]] """[33,[[41,[[15,["abc"]],[73,["code"]]]],"name","code"]]"""

      //selectionOfStream
      r.table("abc").skip(10).pluck("name", "code") =%=[ReqlFiniteStream[ReqlObject]] """[33,[[70,[[15,["abc"]],10]],"name","code"]]"""

      //infiniteStream
      //TODO: it should not compile
      r.range().pluck("name", "code") =%=[ReqlInfiniteStream[ReqlInteger]] """[33,[[173,[]],"name","code"]]"""

      //finiteStream
      //TODO: it should not compile
      r.range(10).pluck("name", "code") =%=[ReqlFiniteStream[ReqlInteger]] """[33,[[173,[10]],"name","code"]]"""

      //changefeed
      abcJsonTable.get("uuid").changes().pluck("name", "code") =%=[ReqlInfiniteStream[ReqlObject]] """[33,[[152,[[16,[[15,["abc"]],"uuid"]]]],"name","code"]]"""

      //array
      r.expr(Seq(
        r.expr(JsonObject.empty), r.expr(JsonObject.empty)
      )).pluck("name", "code") =%=[ReqlArray[ReqlDatum]] """[33,[[2,[{},{}]],"name","code"]]"""

      //singleSelection
      abcJsonTable.get("uuid").pluck("code") =%=[ReqlObject] """[33,[[16,[[15,["abc"]],"uuid"]],"code"]]"""

      //object
      r.expr(Map("code" -> r.expr(123))).pluck("code") =%=[ReqlObject] """[33,[{"code":123},"code"]]"""
    }

    "without" in {
      //table
      r.table("abc").without("name", "code") =%=[ReqlFiniteStream[ReqlObject]] """[34,[[15,["abc"]],"name","code"]]"""

      //tableSlice
      r.table("abc").orderBy(OrderedIndex(r.asc("code"))).without("name", "code") =%=[ReqlFiniteStream[ReqlObject]] """[34,[[41,[[15,["abc"]]],{"index":[73,["code"]]}],"name","code"]]"""

      //selectionOfArray
      r.table("abc").orderBy(r.asc("code")).without("name", "code") =%=[ReqlArray[ReqlObject]] """[34,[[41,[[15,["abc"]],[73,["code"]]]],"name","code"]]"""

      //selectionOfStream
      r.table("abc").skip(10).without("name", "code") =%=[ReqlFiniteStream[ReqlObject]] """[34,[[70,[[15,["abc"]],10]],"name","code"]]"""

      //infiniteStream
      //TODO: it should not compile
      r.range().without("name", "code") =%=[ReqlInfiniteStream[ReqlInteger]] """[34,[[173,[]],"name","code"]]"""

      //finiteStream
      //TODO: it should not compile
      r.range(10).without("name", "code") =%=[ReqlFiniteStream[ReqlInteger]] """[34,[[173,[10]],"name","code"]]"""

      //changefeed
      abcJsonTable.get("uuid").changes().without("name", "code") =%=[ReqlInfiniteStream[ReqlObject]] """[34,[[152,[[16,[[15,["abc"]],"uuid"]]]],"name","code"]]"""

      //array
      r.expr(Seq(
        r.expr(JsonObject.empty), r.expr(JsonObject.empty)
      )).without("code") =%=[ReqlArray[ReqlDatum]] """[34,[[2,[{},{}]],"code"]]"""

      //singleSelection
      abcJsonTable.get("uuid").without("code") =%=[ReqlObject] """[34,[[16,[[15,["abc"]],"uuid"]],"code"]]"""

      //object
      r.expr(Map("code" -> r.expr(123))).without("code") =%=[ReqlObject] """[34,[{"code":123},"code"]]"""
    }

    "merge" in {
      //table
      abcJsonTable.merge(r.expr(JsonObject.empty)) =%=[ReqlFiniteStream[ReqlObject]] """[35,[[15,["abc"]],{}]]"""

      //tableSlice
      abcJsonTable.orderBy(OrderedIndex(r.asc("code"))).merge(r.expr(JsonObject.empty)) =%=[ReqlFiniteStream[ReqlObject]] """[35,[[41,[[15,["abc"]]],{"index":[73,["code"]]}],{}]]"""

      //selectionOfArray
      abcJsonTable.orderBy(r.asc("code")).merge(r.expr(JsonObject.empty)) =%=[ReqlArray[ReqlObject]] """[35,[[41,[[15,["abc"]],[73,["code"]]]],{}]]"""

      //selectionOfStream
      abcJsonTable.skip(10).merge(r.expr(JsonObject.empty)) =%=[ReqlFiniteStream[ReqlObject]] """[35,[[70,[[15,["abc"]],10]],{}]]"""

      //infiniteStream
      """r.range().merge(r.expr(JsonObject.empty))""".stripMargin.shouldNot(compile)
      //r.range().merge(r.expr(JsonObject.empty)) =%=[ReqlInfiniteStream[ReqlObject]] """[35,[[173,[]],{}]]"""

      //finiteStream
      """r.range(10).merge(r.expr(JsonObject.empty))""".stripMargin.shouldNot(compile)
      //r.range(10).merge(r.expr(JsonObject.empty)) =%=[ReqlFiniteStream[ReqlObject]] """[35,[[173,[10]],{}]]"""

      //changefeed
      abcJsonTable.get("uuid").changes().merge(r.expr(JsonObject.empty)) =%=[ReqlInfiniteStream[ReqlObject]] """[35,[[152,[[16,[[15,["abc"]],"uuid"]]]],{}]]"""

      //array
      r.expr(Seq(r.expr(JsonObject.empty), r.expr(JsonObject.empty)))
        .merge({obj: ReqlObject => r.expr(JsonObject.empty)}) =%=[ReqlArray[ReqlObject]] """[35,[[2,[{},{}]],[69,[[2,[0]],{}]]]]"""

      //singleSelection
      abcJsonTable.get("uuid")
        .merge(r.expr(JsonObject.empty)) =%=[ReqlObject] """[35,[[16,[[15,["abc"]],"uuid"]],{}]]"""
      abcJsonTable.get("uuid")
        .merge(r.expr(JsonObject.empty), {obj: ReqlObject => r.expr(JsonObject.empty)}) =%=[ReqlObject] """[35,[[16,[[15,["abc"]],"uuid"]],{},[69,[[2,[0]],{}]]]]"""

      //object
      r.expr(JsonObject.empty).merge({obj: ReqlObject => r.expr(JsonObject.empty)}) =%=[ReqlObject] """[35,[{},[69,[[2,[0]],{}]]]]"""
      r.expr(JsonObject.empty)
        .merge(r.expr(JsonObject.empty), r.expr(JsonObject.empty)) =%=[ReqlObject] """[35,[{},{},{}]]"""
      r.expr(JsonObject.empty)
        .merge({obj: ReqlObject => r.expr(JsonObject.empty)}, r.expr(JsonObject.empty)) =%=[ReqlObject] """[35,[{},[69,[[2,[0]],{}]],{}]]"""
    }

    "between" in {
      //table
      abcJsonTable.between(1, 10) =%=[ReqlTableSlice[JsonObject, String]] """[182,[[15,["abc"]],1,10]]"""

      abcJsonTable.between(r.maxval, r.minval) =%=[ReqlTableSlice[JsonObject, String]] """[182,[[15,["abc"]],[181,[]],[180,[]]]]"""

      abcJsonTable
        .between(1, 10, indexOptions = Index("code_index")) =%=[ReqlTableSlice[JsonObject, String]] """[182,[[15,["abc"]],1,10],{"index":"code_index"}]"""
      abcJsonTable
        .between(1, 10, Bounds(OpenBound, DefaultBound), Index("code_index")) =%=[ReqlTableSlice[JsonObject, String]] """[182,[[15,["abc"]],1,10],{"left_bound":"open","index":"code_index"}]"""

      //tableSlice
      //TODO: .between not allowed on same table twice - signature in docs says it's allowed; db says - not allowed (it's works with order: r.table("tv_shows").orderBy({index: "code"}).between(1,4))
      abcJsonTable.orderBy(OrderedIndex(r.asc("code"))) shouldBe an[ReqlTableSlice[JsonObject, String]]
      abcJsonTable.orderBy(OrderedIndex(r.asc("code"))).between(1, 10) =%=[ReqlTableSlice[JsonObject, String]] """[182,[[41,[[15,["abc"]]],{"index":[73,["code"]]}],1,10]]"""
    }

    "reduce" in {
      //table
      r.table[JsonObject, String]("abc").reduce {
        (left: ReqlModel[JsonObject, String], right: ReqlModel[JsonObject, String]) =>
          left.merge(right)
      } =%=[ReqlModel[JsonObject, String]] """[37,[[15,["abc"]],[69,[[2,[0,1]],[35,[[10,[0]],[10,[1]]]]]]]]"""

      //tableSlice
      r.table[JsonObject, String]("abc").orderBy(OrderedIndex(r.asc("code"))).reduce {
        (left: ReqlModel[JsonObject, String], right: ReqlModel[JsonObject, String]) =>
          left.merge(right)
      } =%=[ReqlModel[JsonObject, String]] """[37,[[41,[[15,["abc"]]],{"index":[73,["code"]]}],[69,[[2,[0,1]],[35,[[10,[0]],[10,[1]]]]]]]]"""

      //selectionOfArray
      r.table[JsonObject, String]("abc").orderBy(r.asc("code")).reduce {
        (left: ReqlModel[JsonObject, String], right: ReqlModel[JsonObject, String]) =>
          left.merge(right)
      } =%=[ReqlModel[JsonObject, String]] """[37,[[41,[[15,["abc"]],[73,["code"]]]],[69,[[2,[0,1]],[35,[[10,[0]],[10,[1]]]]]]]]"""

      //selectionOfStream
      r.table[JsonObject, String]("abc").skip(10).reduce {
        (left: ReqlModel[JsonObject, String], right: ReqlModel[JsonObject, String]) =>
          left.merge(right)
      } =%=[ReqlModel[JsonObject, String]] """[37,[[70,[[15,["abc"]],10]],[69,[[2,[0,1]],[35,[[10,[0]],[10,[1]]]]]]]]"""

      //stream
      """
        |r.table("abc").get("uuid").changes().reduce {
        |  (left: ReqlObject, right: ReqlObject) =>
        |    left.merge(right)
        |}
      """.stripMargin.shouldNot(compile)

      //array
      r.expr(Seq(emptyReqlObject, emptyReqlObject)).reduce {
        (left: ReqlObject, right: ReqlObject) =>
          left.merge(right)
      } =%=[ReqlObject] """[37,[[2,[{},{}]],[69,[[2,[0,1]],[35,[[10,[0]],[10,[1]]]]]]]]"""
    }

    "map" in {
      //table
      abcJsonTable.map { x => x.asNumber.mul(2) } =%=[ReqlFiniteStream[ReqlFloat]] """[38,[[15,["abc"]],[69,[[2,[0]],[26,[[10,[0]],2]]]]]]"""

      abcJsonTable.map(bcdJsonTable, (x, y: ReqlModel[JsonObject, String]) => x.merge(y)) =%=[ReqlFiniteStream[ReqlObject]] """[38,[[15,["abc"]],[15,["bcd"]],[69,[[2,[0,1]],[35,[[10,[0]],[10,[1]]]]]]]]"""

      //tableSlice
      abcJsonTable.orderBy(OrderedIndex(r.asc("code"))).map { x => x.asNumber.mul(2) } =%=[ReqlFiniteStream[ReqlNumber]] """[38,[[41,[[15,["abc"]]],{"index":[73,["code"]]}],[69,[[2,[0]],[26,[[10,[0]],2]]]]]]"""

      //selectionOfArray
      abcJsonTable.orderBy(r.asc("code")).map { x => x.asNumber.mul(2) } =%=[ReqlArray[ReqlNumber]] """[38,[[41,[[15,["abc"]],[73,["code"]]]],[69,[[2,[0]],[26,[[10,[0]],2]]]]]]"""

      //selectionOfStream
      //TODO: it should not work - should not allow to convert object to number?
      abcJsonTable.skip(10).map { x => x.asNumber.mul(2) } =%=[ReqlFiniteStream[ReqlNumber]] """[38,[[70,[[15,["abc"]],10]],[69,[[2,[0]],[26,[[10,[0]],2]]]]]]"""

      //infiniteStream
      r.range().map(_.mul(2)) =%=[ReqlInfiniteStream[ReqlInteger]] """[38,[[173,[]],[69,[[2,[0]],[26,[[10,[0]],2]]]]]]"""

      //finiteStream
      r.range(10).map(_.mul(2)) =%=[ReqlFiniteStream[ReqlInteger]] """[38,[[173,[10]],[69,[[2,[0]],[26,[[10,[0]],2]]]]]]"""

      //changefeed
      abcJsonTable.get("uuid").changes().map(_.merge(JsonObject.empty)) =%=[ReqlInfiniteStream[ReqlObject]] """[38,[[152,[[16,[[15,["abc"]],"uuid"]]]],[69,[[2,[0]],[35,[[10,[0]],{}]]]]]]"""

      //array
      r.expr(Seq(r.expr(123), r.expr(234))).map(_.add(r.expr(1))) =%=[ReqlArray[ReqlInteger]] """[38,[[2,[123,234]],[69,[[2,[0]],[24,[[10,[0]],1]]]]]]"""

      r.expr(Seq(r.expr(123), r.expr(234))).map[ReqlInteger, ReqlInteger](r.expr(Seq(r.expr(345), r.expr(456))), {
        case (el0: ReqlInteger, el1: ReqlInteger) =>
          el0.mul(el1): ReqlInteger
      }) =%=[ReqlArray[ReqlInteger]] """[38,[[2,[123,234]],[2,[345,456]],[69,[[2,[0,1]],[26,[[10,[0]],[10,[1]]]]]]]]"""

      r.expr(Seq(r.expr(123), r.expr(234))).map[ReqlInteger, ReqlInteger, ReqlInteger](
        r.expr(Seq(r.expr(345), r.expr(456))),
        r.expr(Seq(r.expr(567), r.expr(678))),
        {
          case (el0: ReqlInteger, el1: ReqlInteger, el2: ReqlInteger) =>
            el0.mul(el1, el2)
        }
      ) =%=[ReqlArray[ReqlInteger]] """[38,[[2,[123,234]],[2,[345,456]],[2,[567,678]],[69,[[2,[0,1,2]],[26,[[10,[0]],[10,[1]],[10,[2]]]]]]]]"""

      //complex example
      abcJsonTable.getField[ReqlNumber]("code").map(x => x.mul(x)) =%=[ReqlFiniteStream[ReqlFloat]] """[38,[[31,[[15,["abc"]],"code"]],[69,[[2,[0]],[26,[[10,[0]],[10,[0]]]]]]]]"""

      abcJsonTable.getField[ReqlNumber]("code").map[ReqlNumber, ReqlNumber](abcJsonTable.getField[ReqlNumber]("num"), {
        case (code: ReqlNumber, num: ReqlNumber) =>
          code.asNumber.add(num.asNumber)
      }) =%=[ReqlFiniteStream[ReqlNumber]] """[38,[[31,[[15,["abc"]],"code"]],[31,[[15,["abc"]],"num"]],[69,[[2,[0,1]],[24,[[10,[0]],[10,[1]]]]]]]]"""
    }

    "fold" in {
      //table
      r.table[JsonObject, String]("abc").fold[ReqlModel[JsonObject, String]](JsonObject.empty)((x, y) => x.merge(y)) =%=[ReqlModel[JsonObject, String]] """[187,[[15,["abc"]],{},[69,[[2,[0,1]],[35,[[10,[0]],[10,[1]]]]]]]]"""

      """
        |r.table("abc").fold(r.expr(JsonObject.empty))((x, y) => r.now())
      """.stripMargin.shouldNot(compile)
      r.table[JsonObject, String]("abc").fold[ReqlObject](JsonObject.empty)((x, y) => x.merge(y)) =%=[ReqlObject] """[187,[[15,["abc"]],{},[69,[[2,[0,1]],[35,[[10,[0]],[10,[1]]]]]]]]"""

      r.table[JsonObject, String]("abc").foldAndEmit[ReqlTime](r.now())(
        (x, y) => x.add(y.keys().count()),
        Emit[ReqlTime, ReqlModel[JsonObject, String], ReqlDatum](
          (acc, row, newAcc) =>
            r.expr(Seq(acc, row, newAcc))
        )
      ) =%=[ReqlFiniteStream[ReqlTime]] """[187,[[15,["abc"]],[103,[]],[69,[[2,[0,1]],[24,[[10,[0]],[43,[[94,[[10,[1]]]]]]]]]]],{"emit":[69,[[2,[0,1,2]],[2,[[10,[0]],[10,[1]],[10,[2]]]]]]}]"""

      r.table[JsonObject, String]("abc").foldAndEmit[ReqlTime](r.now())(
        (x, y) => x.add(y.keys().count()),
        Emit[ReqlTime, ReqlModel[JsonObject, String], ReqlDatum](
          (acc, row, newAcc) =>
            r.expr(Seq(acc, row, newAcc))
        ).withFinalEmit(
          last =>
            r.expr(Seq(r.expr("finish"), last, r.expr("!")))
        )
      ) =%=[ReqlFiniteStream[ReqlTime]] """[187,[[15,["abc"]],[103,[]],[69,[[2,[0,1]],[24,[[10,[0]],[43,[[94,[[10,[1]]]]]]]]]]],{"emit":[69,[[2,[0,1,2]],[2,[[10,[0]],[10,[1]],[10,[2]]]]]],"final_emit":[69,[[2,[0]],[2,["finish",[10,[0]],"!"]]]]}]"""

      r.table[JsonObject, String]("abc").foldAndEmit[ReqlTime](r.now())(
        (x, y) => x.add(y.keys().count()),
        EmitWithFinalEmit[ReqlTime, ReqlModel[JsonObject, String], ReqlDatum](
          (acc, row, newAcc) =>
            r.expr(Seq(acc, row, newAcc)),
          last =>
            r.expr(Seq(r.expr("finish"), last, r.expr("!")))
        )
      ) =%=[ReqlFiniteStream[ReqlTime]] """[187,[[15,["abc"]],[103,[]],[69,[[2,[0,1]],[24,[[10,[0]],[43,[[94,[[10,[1]]]]]]]]]]],{"emit":[69,[[2,[0,1,2]],[2,[[10,[0]],[10,[1]],[10,[2]]]]]],"final_emit":[69,[[2,[0]],[2,["finish",[10,[0]],"!"]]]]}]"""

      //tableSlice
      r.table[JsonObject, String]("abc").orderBy(OrderedIndex(r.asc("code"))).
        fold(emptyReqlObject)((x, y) => x.merge(y)) =%=[ReqlDatum] """[187,[[41,[[15,["abc"]]],{"index":[73,["code"]]}],{},[69,[[2,[0,1]],[35,[[10,[0]],[10,[1]]]]]]]]"""

      r.table[JsonObject, String]("abc").orderBy(OrderedIndex(r.asc("code"))).
        foldAndEmit[ReqlTime](r.now())(
          (x, y) => x.add(y.keys().count()),
          EmitWithFinalEmit[ReqlTime, ReqlModel[JsonObject, String], ReqlDatum](
            (acc, row, newAcc) =>
              r.expr(Seq(acc, row, newAcc)),
            last =>
              r.expr(Seq(r.expr("finish"), last, r.expr("!")))
          )
        ) =%=[ReqlFiniteStream[ReqlTime]] """[187,[[41,[[15,["abc"]]],{"index":[73,["code"]]}],[103,[]],[69,[[2,[0,1]],[24,[[10,[0]],[43,[[94,[[10,[1]]]]]]]]]]],{"emit":[69,[[2,[0,1,2]],[2,[[10,[0]],[10,[1]],[10,[2]]]]]],"final_emit":[69,[[2,[0]],[2,["finish",[10,[0]],"!"]]]]}]"""

      //selectionOfArray
      r.table[JsonObject, String]("abc").orderBy(r.asc("code")).
        fold(emptyReqlObject)((x, y) => x.merge(y)) =%=[ReqlDatum] """[187,[[41,[[15,["abc"]],[73,["code"]]]],{},[69,[[2,[0,1]],[35,[[10,[0]],[10,[1]]]]]]]]"""

      r.table[JsonObject, String]("abc").orderBy(r.asc("code")).
        foldAndEmit[ReqlTime](r.now())(
          (x, y) => x.add(y.keys().count()),
          EmitWithFinalEmit[ReqlTime, ReqlModel[JsonObject, String], ReqlDatum](
            (acc, row, newAcc) =>
              r.expr(Seq(acc, row, newAcc)),
            last =>
              r.expr(Seq(r.expr("finish"), last, r.expr("!")))
          )
        ) =%=[ReqlArray[ReqlDatum]] """[187,[[41,[[15,["abc"]],[73,["code"]]]],[103,[]],[69,[[2,[0,1]],[24,[[10,[0]],[43,[[94,[[10,[1]]]]]]]]]]],{"emit":[69,[[2,[0,1,2]],[2,[[10,[0]],[10,[1]],[10,[2]]]]]],"final_emit":[69,[[2,[0]],[2,["finish",[10,[0]],"!"]]]]}]"""

      //selectionOfStream
      r.table[JsonObject, String]("abc").skip(10) shouldBe an[ReqlSelectionOfStream[JsonObject, String]]

      r.table[JsonObject, String]("abc").skip(10).
        fold(emptyReqlObject)((x, y) => x.merge(y)) =%=[ReqlDatum] """[187,[[70,[[15,["abc"]],10]],{},[69,[[2,[0,1]],[35,[[10,[0]],[10,[1]]]]]]]]"""

      r.table[JsonObject, String]("abc").skip(10).
        foldAndEmit[ReqlTime](r.now())(
          (x, y) => x.add(y.keys().count()),
          EmitWithFinalEmit[ReqlTime, ReqlModel[JsonObject, String], ReqlDatum](
            (acc, row, newAcc) =>
              r.expr(Seq(acc, row, newAcc)),
            last =>
              r.expr(Seq(r.expr("finish"), last, r.expr("!")))
          )
        ) =%=[ReqlFiniteStream[ReqlTime]] """[187,[[70,[[15,["abc"]],10]],[103,[]],[69,[[2,[0,1]],[24,[[10,[0]],[43,[[94,[[10,[1]]]]]]]]]]],{"emit":[69,[[2,[0,1,2]],[2,[[10,[0]],[10,[1]],[10,[2]]]]]],"final_emit":[69,[[2,[0]],[2,["finish",[10,[0]],"!"]]]]}]"""

      //finiteStream
      r.range(1, 10).fold[ReqlTime](r.now())((acc, value) => acc.add(value)) =%=[ReqlDatum] """[187,[[173,[1,10]],[103,[]],[69,[[2,[0,1]],[24,[[10,[0]],[10,[1]]]]]]]]"""

      r.range(1, 10).
        foldAndEmit[ReqlTime](r.now())(
          (x, y) => x.add(y),
          EmitWithFinalEmit[ReqlTime, ReqlDatum, ReqlDatum](
            (acc, row, newAcc) =>
              r.expr(Seq(acc, row, newAcc)),
            last =>
              r.expr(Seq(r.expr("finish"), last, r.expr("!")))
          )
        ) =%=[ReqlFiniteStream[ReqlTime]] """[187,[[173,[1,10]],[103,[]],[69,[[2,[0,1]],[24,[[10,[0]],[10,[1]]]]]]],{"emit":[69,[[2,[0,1,2]],[2,[[10,[0]],[10,[1]],[10,[2]]]]]],"final_emit":[69,[[2,[0]],[2,["finish",[10,[0]],"!"]]]]}]"""

      //infiniteStream
      """
        |r.range().fold(r.now())((acc, value) => acc.add(value.asNumber))
      """.stripMargin.shouldNot(compile)

      r.range().
        foldAndEmit[ReqlTime](r.now())(
          (x, y) => x.add(y),
          EmitWithFinalEmit[ReqlTime, ReqlInteger, ReqlDatum](
            (acc, row, newAcc) =>
              r.expr(Seq(acc, row, newAcc)),
            last =>
              r.expr(Seq(r.expr("finish"), last, r.expr("!")))
          )
        ) =%=[ReqlInfiniteStream[ReqlDatum]] """[187,[[173,[]],[103,[]],[69,[[2,[0,1]],[24,[[10,[0]],[10,[1]]]]]]],{"emit":[69,[[2,[0,1,2]],[2,[[10,[0]],[10,[1]],[10,[2]]]]]],"final_emit":[69,[[2,[0]],[2,["finish",[10,[0]],"!"]]]]}]"""

      //array
      r.expr(Seq(r.expr(1), r.expr(2), r.expr(3))).
        fold[ReqlTime](r.now())((acc, value) => acc.add(value)) =%=[ReqlDatum] """[187,[[2,[1,2,3]],[103,[]],[69,[[2,[0,1]],[24,[[10,[0]],[10,[1]]]]]]]]"""

      r.expr(Seq(r.expr(1), r.expr(2), r.expr(3))).
        foldAndEmit[ReqlTime](r.now())(
          (x, y) => x.add(y),
          EmitWithFinalEmit[ReqlTime, ReqlDatum, ReqlDatum](
            (acc, row, newAcc) =>
              r.expr(Seq(acc, row, newAcc)),
            last =>
              r.expr(Seq(r.expr("finish"), last, r.expr("!")))
          )
        ) =%=[ReqlArray[ReqlDatum]] """[187,[[2,[1,2,3]],[103,[]],[69,[[2,[0,1]],[24,[[10,[0]],[10,[1]]]]]]],{"emit":[69,[[2,[0,1,2]],[2,[[10,[0]],[10,[1]],[10,[2]]]]]],"final_emit":[69,[[2,[0]],[2,["finish",[10,[0]],"!"]]]]}]"""
    }

    "filter" in {
      //filter should not work on single selection - only sequence
      """
        |r.table("abc").get("uuid").filter(r.expr(JsonObject.fromMap(Map(
        |  "code" -> Json.fromInt(123)
        |))))
      """.stripMargin.shouldNot(compile)

      //filter should not filter by datum (string, number, etc)
      """
        |r.table("abc").filter(r.expr("code"))
      """.stripMargin.shouldNot(compile)

      """
        |r.table("abc").filter(r.expr(123))
      """.stripMargin.shouldNot(compile)

      //table
      abcJsonTable.filter(JsonObject.fromMap(Map(
        "code" -> Json.fromInt(123)
      ))) =%=[ReqlSelectionOfStream[JsonObject, String]] """[39,[[15,["abc"]],{"code":123}]]"""

      abcJsonTable.filter(JsonObject.fromMap(Map(
        "code" -> Json.fromInt(123)
      )), Skip) =%=[ReqlSelectionOfStream[JsonObject, String]] """[39,[[15,["abc"]],{"code":123}]]"""

      abcJsonTable.filter(JsonObject.fromMap(Map(
        "code" -> Json.fromInt(123)
      )), NoSkip) =%=[ReqlSelectionOfStream[JsonObject, String]] """[39,[[15,["abc"]],{"code":123}],{"default":true}]"""

      abcJsonTable.filter(JsonObject.fromMap(Map(
        "code" -> Json.fromInt(123)
      )), RethrowError) =%=[ReqlSelectionOfStream[JsonObject, String]] """[39,[[15,["abc"]],{"code":123}],{"default":[12,[]]}]"""

      abcJsonTable.filter(_.asObject.getField("code").eq_(5)) =%=[ReqlSelectionOfStream[JsonObject, String]] """[39,[[15,["abc"]],[69,[[2,[0]],[17,[[31,[[10,[0]],"code"]],5]]]]]]"""

      //tableSlice
      abcJsonTable.orderBy(OrderedIndex(r.asc("code"))).filter(JsonObject.fromMap(Map(
        "code" -> Json.fromInt(123)
      ))) =%=[ReqlSelectionOfStream[JsonObject, String]] """[39,[[41,[[15,["abc"]]],{"index":[73,["code"]]}],{"code":123}]]"""

      //selectionOfArray
      abcJsonTable.orderBy(r.asc("code")).filter(JsonObject.fromMap(Map(
        "code" -> Json.fromInt(123)
      ))) =%=[ReqlSelectionOfArray[JsonObject, String]] """[39,[[41,[[15,["abc"]],[73,["code"]]]],{"code":123}]]"""

      //selectionOfStream
      abcJsonTable.skip(10).filter(JsonObject.fromMap(Map(
        "code" -> Json.fromInt(123)
      ))) =%=[ReqlSelectionOfStream[JsonObject, String]] """[39,[[70,[[15,["abc"]],10]],{"code":123}]]"""

      // infiniteStream
      r.range().filter(_ => true) =%=[ReqlInfiniteStream[ReqlInteger]] """[39,[[173,[]],[69,[[2,[0]],true]]]]"""

      // finiteStream
      r.range(10).filter(_ => true) =%=[ReqlFiniteStream[ReqlInteger]] """[39,[[173,[10]],[69,[[2,[0]],true]]]]"""

      // changefeed
      val notification: ReqlChangefeedNotification[JsonObject] = ChangefeedNotification(
        Some(JsonObject.fromMap(Map("code" -> Json.fromInt(1)))),
        Some(JsonObject.fromMap(Map("code" -> Json.fromInt(2))))
      )
      abcJsonTable.get("uuid").changes()
        .filter(notification) =%=[ReqlInfiniteStream[ReqlChangefeedNotification[JsonObject]]] """[39,[[152,[[16,[[15,["abc"]],"uuid"]]]],{"old_val":{"code":1},"new_val":{"code":2}}]]"""

      // array
      """
        |r.expr(Seq(r.expr(123), r.expr(234))).filter(r.expr(123))
      """.stripMargin.shouldNot(compile)

      r.expr(Seq(r.expr(123), r.expr(234))).filter(_ => true) =%=[ReqlArray[ReqlInteger]] """[39,[[2,[123,234]],[69,[[2,[0]],true]]]]"""
    }

    "concat_map" in {
      abcJsonTable shouldBe an[ReqlTable[ReqlObject, UUID]]
      abcJsonTable shouldBe an[ReqlFiniteSequence[ReqlObject]]

      abcJsonTable.orderBy(OrderedIndex(r.asc("code"))) shouldBe an[ReqlTableSlice[ReqlObject, UUID]]
      abcJsonTable.orderBy(OrderedIndex(r.asc("code"))) shouldBe an[ReqlFiniteSequence[ReqlObject]]

      abcJsonTable.orderBy(r.asc("code")) shouldBe an[ReqlSelectionOfArray[ReqlObject, UUID]]
      abcJsonTable.orderBy(r.asc("code")) shouldBe an[ReqlFiniteSequence[ReqlObject]]

      abcJsonTable.skip(10) shouldBe an[ReqlSelectionOfStream[ReqlObject, UUID]]
      abcJsonTable.skip(10) shouldBe an[ReqlFiniteSequence[ReqlObject]]

      r.range() shouldBe an[ReqlInfiniteStream[ReqlInteger]]
      r.range() should not be an[ReqlFiniteSequence[ReqlInteger]]

      r.range(10) shouldBe an[ReqlFiniteStream[ReqlInteger]]
      r.range(10) shouldBe an[ReqlFiniteSequence[ReqlInteger]]

      abcJsonTable.get("uuid").changes() shouldBe an[ReqlInfiniteStream[ReqlChangefeedNotification[ReqlModel[JsonObject, String]]]]
      abcJsonTable.get("uuid").changes() should not be an[ReqlFiniteSequence[ReqlChangefeedNotification[ReqlModel[JsonObject, String]]]]

      r.expr(Seq(r.expr(123), r.expr(234))) shouldBe an[ReqlArray[ReqlInteger]]
      r.expr(Seq(r.expr(123), r.expr(234))) shouldBe an[ReqlFiniteSequence[ReqlInteger]]

      //table
      r.table[JsonObject, String]("abc").concatMap(_ => r.table("bcd")) =%=[ReqlFiniteStream[ReqlObject]] """[40,[[15,["abc"]],[69,[[2,[0]],[15,["bcd"]]]]]]"""

      //tableSlice
      r.table[JsonObject, String]("abc").orderBy(OrderedIndex(r.asc("code"))).concatMap(_ => r.table("bcd")) =%=[ReqlFiniteStream[ReqlObject]] """[40,[[41,[[15,["abc"]]],{"index":[73,["code"]]}],[69,[[2,[0]],[15,["bcd"]]]]]]"""

      //selectionOfArray
      r.table[JsonObject, String]("abc").orderBy(r.asc("code")).concatMap(_ => r.table("bcd")) =%=[ReqlArray[ReqlObject]] """[40,[[41,[[15,["abc"]],[73,["code"]]]],[69,[[2,[0]],[15,["bcd"]]]]]]"""

      //selectionOfStream
      r.table[JsonObject, String]("abc").skip(10).concatMap(_ => r.table("bcd")) =%=[ReqlFiniteStream[ReqlObject]] """[40,[[70,[[15,["abc"]],10]],[69,[[2,[0]],[15,["bcd"]]]]]]"""

      //infiniteStream
      r.range().concatMap(_ => r.range(10)) =%=[ReqlInfiniteStream[ReqlInteger]] """[40,[[173,[]],[69,[[2,[0]],[173,[10]]]]]]"""
      """
        |r.range().concatMap(_ => r.range())
      """.stripMargin.shouldNot(compile)

      //finiteStream
      r.range(10).concatMap(_ => r.range(10)) =%=[ReqlFiniteStream[ReqlInteger]] """[40,[[173,[10]],[69,[[2,[0]],[173,[10]]]]]]"""
      """
        |r.range(10).concatMap(_ => r.range())
      """.stripMargin.shouldNot(compile)

      //changefeed
      abcJsonTable.get("uuid").changes().concatMap(_ => r.table("bcd")) =%=[ReqlInfiniteStream[ReqlObject]] """[40,[[152,[[16,[[15,["abc"]],"uuid"]]]],[69,[[2,[0]],[15,["bcd"]]]]]]"""

      //array
      """
        |r.expr(Seq(r.expr(123), r.expr(234))).concatMap(_.asNumber.add(r.expr(1)))
      """.stripMargin.shouldNot(compile)

      r.expr(Seq(r.expr(123), r.expr(234))).concatMap(x => Seq(x.asNumber.add(r.expr(1)))) =%=[ReqlArray[ReqlFloat]] """[40,[[2,[123,234]],[69,[[2,[0]],[2,[[24,[[10,[0]],1]]]]]]]]"""

      r.expr(Seq(r.expr(1), r.expr(2), r.expr(3))).concatMap(
        _ => r.table("abc")
      ) =%=[ReqlArray[ReqlDatum]] """[40,[[2,[1,2,3]],[69,[[2,[0]],[15,["abc"]]]]]]"""

      //other cases
      """
        |r.table("abc").getField("code").
        |  concatMap(x => x.asNumber.mul(x.asNumber))
      """.stripMargin.shouldNot(compile)

      r.table("abc").getField[ReqlNumber]("code").
        concatMap[ReqlNumber](x => Seq(x.asNumber.mul(x.asNumber))) =%=[ReqlFiniteStream[ReqlNumber]] """[40,[[31,[[15,["abc"]],"code"]],[69,[[2,[0]],[2,[[26,[[10,[0]],[10,[0]]]]]]]]]]"""

      r.table("abc").getField[ReqlNumber]("code").
        concatMap(_ => r.table("def")) =%=[ReqlFiniteStream[ReqlObject]] """[40,[[31,[[15,["abc"]],"code"]],[69,[[2,[0]],[15,["def"]]]]]]"""

      //complex example
      r.table[JsonObject, String]("abc").
        concatMap(_ =>
          r.table[JsonObject, String]("abc").
            concatMap(_ =>
              r.table[JsonObject, String]("abc")
            )
        ).
        count() =%=[ReqlInteger] """[43,[[40,[[15,["abc"]],[69,[[2,[0]],[40,[[15,["abc"]],[69,[[2,[0]],[15,["abc"]]]]]]]]]]]]"""
    }

    "order_by" in {
      """
        |OrderedIndex(r.desc(_.asNumber).add(5))
      """.stripMargin.shouldNot(compile)

      abcJsonTable.orderBy(r.asc("code")) shouldBe an[ReqlSelectionOfArray[ReqlObject, UUID]]
      abcJsonTable.orderBy(OrderedIndex(r.desc("id"))) shouldBe an[ReqlTableSlice[ReqlObject, UUID]]

      //r.table("tv_shows").filter({}).orderBy({index:"code"}).typeOf() -> not compile
      //r.table("tv_shows").filter({}).orderBy("code").typeOf() -> "SELECTION<ARRAY>"
      //r.table("tv_shows").filter({}).typeOf() -> "SELECTION<STREAM>"
      """
        |r.table("tv_shows").filter(JsonObject.empty).orderBy(OrderedIndex(r.desc("id")))
      """.stripMargin.shouldNot(compile)


      //table
      abcJsonTable.orderBy(OrderedIndex(r.desc("id"))) =%=[ReqlTableSlice[JsonObject, String]] """[41,[[15,["abc"]]],{"index":[74,["id"]]}]"""

      abcJsonTable.orderBy(r.asc("code")) =%=[ReqlSelectionOfArray[JsonObject, String]] """[41,[[15,["abc"]],[73,["code"]]]]"""

      abcJsonTable.orderBy(r.asc("code"), r.desc(_.asNumber.add(5))) =%=[ReqlSelectionOfArray[JsonObject, String]] """[41,[[15,["abc"]],[73,["code"]],[74,[[69,[[2,[0]],[24,[[10,[0]],5]]]]]]]]"""

      abcJsonTable.orderBy(OrderedIndex(r.desc("id")), r.asc("code"), r.desc(_.asNumber.add(5))) =%=[ReqlSelectionOfStream[JsonObject, String]] """[41,[[15,["abc"]],[73,["code"]],[74,[[69,[[2,[0]],[24,[[10,[0]],5]]]]]]],{"index":[74,["id"]]}]"""


      //tableSlice
      abcJsonTable.between(1, 7, indexOptions = Index("code")) shouldBe an[ReqlTableSlice[JsonObject, String]]
      abcJsonTable.between(1, 7, indexOptions = Index("code")).orderBy(r.asc("name")) =%=[ReqlSelectionOfArray[JsonObject, String]] """[41,[[182,[[15,["abc"]],1,7],{"index":"code"}],[73,["name"]]]]"""

      //r.table("tv_shows").orderBy({index: "code"}).between(1, 7).typeOf() -> "TABLE_SLICE"   (works same as orderBy({index:code}).between(1,7,{index:code}) and like .between(1,7,{index:code}).orderBy({index:code})  )
      abcJsonTable.orderBy(OrderedIndex(r.asc("code"))).between(1, 7) shouldBe an[ReqlTableSlice[ReqlObject, UUID]]
      // db can handle it but our query model can't
      """
        |r.table("abc").orderBy(OrderedIndex(r.asc("code"))).between(1, 7, indexOptions = Index("code"))
      """.stripMargin.shouldNot(compile)

      // db can handle it but our query model can't
      //r.table("tv_shows").between(1, 7, {index: "code"}).orderBy({index: "name"}).typeOf() -> Not work        (can't change index)        //we don't need to support this - it can be done by .orderBy(index).between(x,y)
      //r.table("tv_shows").between(1, 7, {index: "code"}).orderBy({index: "code"}).typeOf() -> "TABLE_SLICE"   (same index is allowed)     //we don't need to support this - it can be done by .orderBy(index).between(x,y)
      """
        |r.table("abc").between(1, 7, indexOptions = Index("code")).orderBy(OrderedIndex(r.desc("code")))
      """.stripMargin.shouldNot(compile)


      //selectionOfArray
      abcJsonTable.orderBy(r.asc("code"), r.asc("name")) shouldBe an[ReqlSelectionOfArray[JsonObject, String]]
      abcJsonTable.orderBy(r.asc("code"), r.asc("name")).orderBy(r.asc("id")) =%=[ReqlSelectionOfArray[JsonObject, String]] """[41,[[41,[[15,["abc"]],[73,["code"]],[73,["name"]]]],[73,["id"]]]]"""

      """
        |r.table("abc").orderBy(r.asc("code"), r.asc("name")).orderBy(OrderedIndex(r.desc("code")))
      """.stripMargin.shouldNot(compile)


      //selectionOfStream
      abcJsonTable.filter(JsonObject.empty) shouldBe an[ReqlSelectionOfStream[JsonObject, String]]
      abcJsonTable.filter(JsonObject.empty).orderBy(r.asc("code")) =%=[ReqlSelectionOfArray[JsonObject, String]] """[41,[[39,[[15,["abc"]],{}]],[73,["code"]]]]"""
      abcJsonTable.filter(JsonObject.empty).
        orderBy(r.asc("major"), r.desc("minor")) =%=[ReqlSelectionOfArray[JsonObject, String]] """[41,[[39,[[15,["abc"]],{}]],[73,["major"]],[74,["minor"]]]]"""


      //should not work on stream
      abcJsonTable.get("uuid").changes() shouldBe an[ReqlInfiniteStream[ReqlChangefeedNotification[ReqlModel[JsonObject, String]]]]
      """
        |r.table("abc").get("uuid").changes().orderBy(r.asc("code"))
      """.stripMargin.shouldNot(compile)


      //array
      r.expr(Seq(r.expr(JsonObject.empty), r.expr(JsonObject.empty), r.expr(JsonObject.empty))).
        orderBy(r.asc("major"), r.desc("minor")) =%=[ReqlArray[ReqlDatum]] """[41,[[2,[{},{},{}]],[73,["major"]],[74,["minor"]]]]"""


      //should not work on singleSelection
      """
        |r.table("abc").get("uuid").orderBy(r.desc("code"))
      """.stripMargin.shouldNot(compile)


      //complex example from docs
      abcJsonTable.
        between(80, 160, indexOptions = Index("code")).
        orderBy(r.desc("name")) =%=[ReqlSelectionOfArray[JsonObject, String]] """[41,[[182,[[15,["abc"]],80,160],{"index":"code"}],[74,["name"]]]]"""
    }

    "distinct" in {
      //table
      r.table("abc").distinct() =%=[ReqlFiniteStream[ReqlObject]] """[42,[[15,["abc"]]]]"""
      r.table("abc").distinct(Index("code")) =%=[ReqlFiniteStream[ReqlObject]] """[42,[[15,["abc"]]],{"index":"code"}]"""

      //tableSlice
      /*r.table("abc").orderBy(OrderedIndex(r.asc("code"))) shouldBe an[ReqlTableSlice]
      r.table("abc").orderBy(OrderedIndex(r.asc("code"))).distinct() =%=[ReqlArray] """[42,[[41,[[15,["abc"]]],{"index":[73,["code"]]}]]]"""

      r.table("abc").between(1, 7, indexOptions = Index("code")) shouldBe an[ReqlTableSlice]
      r.table("abc").between(1, 7, indexOptions = Index("code")).distinct() =%=[ReqlArray] """[42,[[182,[[15,["abc"]],1,7],{"index":"code"}]]]"""*/

      //selectionOfArray
      r.table("abc").orderBy(r.asc("code")).distinct() =%=[ReqlArray[ReqlDatum]] """[42,[[41,[[15,["abc"]],[73,["code"]]]]]]"""

      //selectionOfStream
      r.table("abc").skip(10).distinct() =%=[ReqlArray[ReqlDatum]] """[42,[[70,[[15,["abc"]],10]]]]"""

      //stream
      //Cannot call a terminal (`reduce`, `count`, etc.) on an infinite stream (such as a changefeed)
      """
        |r.table("abc").get("uuid").changes().distinct()
      """.stripMargin.shouldNot(compile)

      //array
      r.expr(Seq(r.expr(123), r.expr(234))).distinct() =%=[ReqlArray[ReqlInteger]] """[42,[[2,[123,234]]]]"""

      //complex cases
      //r.table("abc").getField("code").distinct() =*= """[42,[[31,[[15,["abc"]],"code"]]]]"""
      //r.table("abc").between(1,10).distinct() =*= """[42,[[182,[[15,["abc"]],1,10]]]]"""
    }

    "count" in {
      //table
      r.table[JsonObject, String]("abc").count() =%=[ReqlInteger] """[43,[[15,["abc"]]]]"""
      r.table[JsonObject, String]("abc").count(JsonObject.empty) =%=[ReqlInteger] """[43,[[15,["abc"]],{}]]"""
      r.table[JsonObject, String]("abc").count(_ => true) =%=[ReqlInteger] """[43,[[15,["abc"]],[69,[[2,[0]],true]]]]"""

      //TODO: fixit?
      //r.table("abc").count(r.table("abc").get("uuid")) =%=[ReqlInteger] """[43,[[15,["abc"]],[16,[[15,["abc"]],"uuid"]]]]"""
      //r.table("abc").count(r.now()) =%=[ReqlInteger] """[43,[[15,["abc"]],[103,[]]]]"""
      r.table[JsonObject, String]("abc").count(_.getField("code").gt(5)) =%=[ReqlInteger] """[43,[[15,["abc"]],[69,[[2,[0]],[21,[[31,[[10,[0]],"code"]],5]]]]]]"""
      r.table[JsonObject, String]("abc").count(_("code").ge(123)) =%=[ReqlInteger] """[43,[[15,["abc"]],[69,[[2,[0]],[22,[[170,[[10,[0]],"code"]],123]]]]]]"""
      r.table[JsonObject, String]("abc").count(JsonObject.empty) =%=[ReqlInteger] """[43,[[15,["abc"]],{}]]"""
      r.table[JsonObject, String]("abc").apply[ReqlInteger]("code").count(r.expr(123)) =*= """[43,[[170,[[15,["abc"]],"code"]],123]]"""


      //tableSlice
      r.table[JsonObject, String]("abc").orderBy(OrderedIndex(r.asc("code"))) shouldBe an[ReqlTableSlice[ReqlObject, UUID]]
      r.table[JsonObject, String]("abc").orderBy(OrderedIndex(r.asc("code"))).count() =%=[ReqlInteger] """[43,[[41,[[15,["abc"]]],{"index":[73,["code"]]}]]]"""
      r.table[JsonObject, String]("abc").orderBy(OrderedIndex(r.asc("code"))).count(JsonObject.empty) =%=[ReqlInteger] """[43,[[41,[[15,["abc"]]],{"index":[73,["code"]]}],{}]]"""
      r.table[JsonObject, String]("abc").orderBy(OrderedIndex(r.asc("code"))).count(_.getField("code").gt(5)) =%=[ReqlInteger] """[43,[[41,[[15,["abc"]]],{"index":[73,["code"]]}],[69,[[2,[0]],[21,[[31,[[10,[0]],"code"]],5]]]]]]"""


      //selectionOfArray
      r.table[JsonObject, String]("abc").orderBy(r.asc("code")) shouldBe an[ReqlSelectionOfArray[ReqlObject, UUID]]
      r.table[JsonObject, String]("abc").orderBy(r.asc("code")).count() =%=[ReqlInteger] """[43,[[41,[[15,["abc"]],[73,["code"]]]]]]"""
      r.table[JsonObject, String]("abc").orderBy(r.asc("code")).count(JsonObject.empty) =%=[ReqlInteger] """[43,[[41,[[15,["abc"]],[73,["code"]]]],{}]]"""
      r.table[JsonObject, String]("abc").orderBy(r.asc("code")).count(_.getField("code").gt(5)) =%=[ReqlInteger] """[43,[[41,[[15,["abc"]],[73,["code"]]]],[69,[[2,[0]],[21,[[31,[[10,[0]],"code"]],5]]]]]]"""


      //selectionOfStream
      r.table[JsonObject, String]("abc").skip(10) shouldBe an[ReqlSelectionOfStream[ReqlObject, UUID]]
      r.table[JsonObject, String]("abc").skip(10).count() =%=[ReqlInteger] """[43,[[70,[[15,["abc"]],10]]]]"""
      r.table[JsonObject, String]("abc").skip(10).count(JsonObject.empty) =%=[ReqlInteger] """[43,[[70,[[15,["abc"]],10]],{}]]"""
      r.table[JsonObject, String]("abc").skip(10).count(_.getField("code").gt(5)) =%=[ReqlInteger] """[43,[[70,[[15,["abc"]],10]],[69,[[2,[0]],[21,[[31,[[10,[0]],"code"]],5]]]]]]"""


      //infinite stream
      """
        |r.range().count()
      """.stripMargin.shouldNot(compile)


      //finite stream
      r.range(10).count() =%=[ReqlInteger] """[43,[[173,[10]]]]"""
      r.range(10).count(5) =%=[ReqlInteger] """[43,[[173,[10]],5]]"""
      r.range(10).count(_.gt(5)) =%=[ReqlInteger] """[43,[[173,[10]],[69,[[2,[0]],[21,[[10,[0]],5]]]]]]"""
      r.range(10).count(r.expr(5)) =%=[ReqlInteger] """[43,[[173,[10]],5]]"""


      //should not work on changefeed
      """
        |r.table[ReqlObject]("abc").get("uuid").changes().count()
      """.stripMargin.shouldNot(compile)


      //array
      r.expr(Seq(r.expr(JsonObject.empty), r.expr(JsonObject.empty), r.expr(JsonObject.empty)))
        .count() =%=[ReqlInteger] """[43,[[2,[{},{},{}]]]]"""
      r.expr(Seq(r.expr(JsonObject.empty), r.expr(JsonObject.empty), r.expr(JsonObject.empty)))
        .count(JsonObject.empty) =%=[ReqlInteger] """[43,[[2,[{},{},{}]],{}]]"""
      r.expr(Seq(r.expr(JsonObject.empty), r.expr(JsonObject.empty), r.expr(JsonObject.empty)))
        .count(_.getField("code").gt(5)) =%=[ReqlInteger] """[43,[[2,[{},{},{}]],[69,[[2,[0]],[21,[[31,[[10,[0]],"code"]],5]]]]]]"""
      r.expr(Seq(r.expr(JsonObject.empty), r.expr(JsonObject.empty), r.expr(JsonObject.empty)))
        .count(r.expr(JsonObject.empty)) =%=[ReqlInteger] """[43,[[2,[{},{},{}]],{}]]"""

      //binary
      r.binary(ByteString(1, 2, 3, 4)).count() =%=[ReqlInteger] """[43,[{"$reql_type$":"BINARY","data":"AQIDBA=="}]]"""

      //string
      r.expr("test").count() =%=[ReqlInteger] """[43,["test"]]"""

      //object
      r.expr(JsonObject.empty).count() =%=[ReqlInteger] """[43,[{}]]"""

      //complex test on return type
      r.random(r.expr("test").count()) =%=[ReqlInteger] """[151,[[43,["test"]]]]"""

      //groupedStream
      //TODO: fixit
      /*r.table[ReqlObject]("abc").group("code") shouldBe an[ReqlGroupedStream]
      r.table[ReqlObject]("abc").group("code").count() =%=[ReqlGroupedData] """[43,[[144,[[15,["abc"]],"code"]]]]"""
      r.table[ReqlObject]("abc").group("code").count(JsonObject.empty) =%=[ReqlGroupedData] """[43,[[144,[[15,["abc"]],"code"]],{}]]"""
      r.table[ReqlObject]("abc").group("code").count(_.getField("code").gt(5)) =%=[ReqlGroupedData] """[43,[[144,[[15,["abc"]],"code"]],[69,[[2,[0]],[21,[[31,[[10,[0]],"code"]],5]]]]]]"""
      r.table[ReqlObject]("abc").group("code").count(pred(r.expr(JsonObject.empty))) =%=[ReqlGroupedData] """[43,[[144,[[15,["abc"]],"code"]],{}]]"""*/
    }

    "is_empty" in {
      //table
      r.table("abc").isEmpty() =%=[ReqlBoolean] """[86,[[15,["abc"]]]]"""

      //tableSlice
      r.table("abc").orderBy(OrderedIndex(r.asc("code"))).isEmpty() =%=[ReqlBoolean] """[86,[[41,[[15,["abc"]]],{"index":[73,["code"]]}]]]"""

      //selectionOfArray
      r.table("abc").orderBy(r.asc("code")).isEmpty() =%=[ReqlBoolean] """[86,[[41,[[15,["abc"]],[73,["code"]]]]]]"""

      //selectionOfStream
      r.table("abc").skip(10).isEmpty() =%=[ReqlBoolean] """[86,[[70,[[15,["abc"]],10]]]]"""

      //infiniteStream
      r.range().isEmpty() =%=[ReqlBoolean] """[86,[[173,[]]]]"""

      //finiteStream
      r.range(10).isEmpty() =%=[ReqlBoolean] """[86,[[173,[10]]]]"""

      //changefeed
      abcJsonTable.get("uuid").changes() shouldBe an[ReqlInfiniteStream[ReqlChangefeedNotification[ReqlModel[JsonObject, String]]]]
      abcJsonTable.get("uuid").changes().isEmpty() =%=[ReqlBoolean] """[86,[[152,[[16,[[15,["abc"]],"uuid"]]]]]]"""
      r.table("abc").distinct() shouldBe an[ReqlFiniteStream[ReqlObject]]
      r.table("abc").distinct().isEmpty() =%=[ReqlBoolean] """[86,[[42,[[15,["abc"]]]]]]"""

      //array
      r.expr(Seq(r.expr(1), r.expr(2), r.expr(3))).isEmpty() =%=[ReqlBoolean] "[86,[[2,[1,2,3]]]]"

      //TODO: FIXIT after fixing of destict after between
      /*r.table("abc").
        between(0, 6, indexOptions = Index("code")).
        distinct().
        isEmpty() =*= """[86,[[42,[[182,[[15,["abc"]],0,6],{"index":"code"}]]]]]"""*/
    }

    "union" in {
      //table
      r.table[JsonObject, String]("abc")
        .union(Nil: Seq[ReqlArray[ReqlModel[JsonObject, String]]]) =%=[ReqlFiniteStream[ReqlObject]] """[44,[[15,["abc"]]]]"""

      r.table[JsonObject, String]("abc")
        .union(Seq(r.table[JsonObject, String]("bcd"))) =%=[ReqlFiniteStream[ReqlObject]] """[44,[[15,["abc"]],[15,["bcd"]]]]"""

      r.table[JsonObject, String]("abc")
        .union(Seq(
          r.expr(Seq(JsonObject.empty: ReqlModel[JsonObject, String]))
        )) =%=[ReqlFiniteStream[ReqlObject]] """[44,[[15,["abc"]],[2,[{}]]]]"""

      r.table[JsonObject, String]("abc")
        .union(Seq(
          r.table[JsonObject, String]("bcd"),
          r.expr(Seq(JsonObject.empty: ReqlModel[JsonObject, String]))
        )) =%=[ReqlFiniteStream[ReqlObject]] """[44,[[15,["abc"]],[15,["bcd"]],[2,[{}]]]]"""

      r.table[JsonObject, String]("abc")
        .union(Seq(
          r.range().map(_ => JsonObject.empty: ReqlModel[JsonObject, String])
        )) =%=[ReqlInfiniteStream[ReqlObject]] """[44,[[15,["abc"]],[38,[[173,[]],[69,[[2,[0]],{}]]]]]]"""

      r.table[JsonObject, String]("abc")
        .union(Seq(
          r.table[JsonObject, String]("bcd"),
          r.expr(Seq(JsonObject.empty: ReqlModel[JsonObject, String]))
        ), MergeSort("code")) =%=[ReqlFiniteStream[ReqlObject]] """[44,[[15,["abc"]],[15,["bcd"]],[2,[{}]]],{"interleave":"code"}]"""


      //tableSlice
      abcJsonTable.orderBy(OrderedIndex(r.asc("code")))
        .union(Nil: Seq[ReqlArray[ReqlModel[JsonObject, String]]]) =%=[ReqlFiniteStream[ReqlObject]] """[44,[[41,[[15,["abc"]]],{"index":[73,["code"]]}]]]"""

      abcJsonTable.orderBy(OrderedIndex(r.asc("code")))
        .union(Seq(
          bcdJsonTable,
          r.expr(Seq(JsonObject.empty: ReqlModel[JsonObject, String]))
        )) =%=[ReqlFiniteStream[ReqlObject]] """[44,[[41,[[15,["abc"]]],{"index":[73,["code"]]}],[15,["bcd"]],[2,[{}]]]]"""

      abcJsonTable.orderBy(OrderedIndex(r.asc("code")))
        .union(Seq(
          r.range().map(_ => JsonObject.empty: ReqlModel[JsonObject, String])
        )) =%=[ReqlInfiniteStream[ReqlObject]] """[44,[[41,[[15,["abc"]]],{"index":[73,["code"]]}],[38,[[173,[]],[69,[[2,[0]],{}]]]]]]"""

      abcJsonTable.orderBy(OrderedIndex(r.asc("code")))
        .union(Seq(
          bcdJsonTable,
          r.expr(Seq(JsonObject.empty: ReqlModel[JsonObject, String]))
        ), MergeSort("code")) =%=[ReqlFiniteStream[ReqlObject]] """[44,[[41,[[15,["abc"]]],{"index":[73,["code"]]}],[15,["bcd"]],[2,[{}]]],{"interleave":"code"}]"""


      //selectionOfStream
      r.table[JsonObject, String]("abc").skip(10).union(Nil: Seq[ReqlArray[ReqlModel[JsonObject, String]]])

      r.table[JsonObject, String]("abc").skip(10)
        .union(Seq(
          r.table[JsonObject, String]("bcd"),
          r.expr(Seq(JsonObject.empty: ReqlModel[JsonObject, String]))
        )) =%=[ReqlFiniteStream[ReqlObject]] """[44,[[70,[[15,["abc"]],10]],[15,["bcd"]],[2,[{}]]]]"""

      r.table[JsonObject, String]("abc").skip(10)
        .union(Seq(
          r.range().map(_ => JsonObject.empty: ReqlModel[JsonObject, String])
        )) =%=[ReqlInfiniteStream[ReqlObject]] """[44,[[70,[[15,["abc"]],10]],[38,[[173,[]],[69,[[2,[0]],{}]]]]]]"""

      r.table[JsonObject, String]("abc").skip(10)
        .union(Seq(
          r.table[JsonObject, String]("bcd"),
          r.expr(Seq(JsonObject.empty: ReqlModel[JsonObject, String]))
        ), MergeSort("code")) =%=[ReqlFiniteStream[ReqlObject]] """[44,[[70,[[15,["abc"]],10]],[15,["bcd"]],[2,[{}]]],{"interleave":"code"}]"""


      //selectionOfArray
      r.table[JsonObject, String]("abc").orderBy(r.asc("code"))
        .union(Nil: Seq[ReqlArray[ReqlModel[JsonObject, String]]]) =%=[ReqlArray[ReqlObject]] """[44,[[41,[[15,["abc"]],[73,["code"]]]]]]"""

      r.table[JsonObject, String]("abc").orderBy(r.asc("code"))
        .union(Seq(
          r.expr(Seq(JsonObject.empty: ReqlModel[JsonObject, String]))
        )) =%=[ReqlArray[ReqlObject]] """[44,[[41,[[15,["abc"]],[73,["code"]]]],[2,[{}]]]]"""

      r.table[JsonObject, String]("abc").orderBy(r.asc("code"))
        .union(Seq(
          r.table[JsonObject, String]("bcd"),
          r.expr(Seq(JsonObject.empty: ReqlModel[JsonObject, String]))
        )) =%=[ReqlFiniteStream[ReqlObject]] """[44,[[41,[[15,["abc"]],[73,["code"]]]],[15,["bcd"]],[2,[{}]]]]"""

      r.table[JsonObject, String]("abc").orderBy(r.asc("code"))
        .union(Seq(
          r.range().map(_ => JsonObject.empty: ReqlModel[JsonObject, String])
        )) =%=[ReqlInfiniteStream[ReqlObject]] """[44,[[41,[[15,["abc"]],[73,["code"]]]],[38,[[173,[]],[69,[[2,[0]],{}]]]]]]"""

      r.table[JsonObject, String]("abc").orderBy(r.asc("code"))
        .union(Seq(
          r.table[JsonObject, String]("bcd"),
          r.expr(Seq(JsonObject.empty: ReqlModel[JsonObject, String]))
        ), MergeSort("code")) =%=[ReqlFiniteStream[ReqlObject]] """[44,[[41,[[15,["abc"]],[73,["code"]]]],[15,["bcd"]],[2,[{}]]],{"interleave":"code"}]"""


      //finiteStream
      r.range(1, 4).map(_ => r.expr(JsonObject.empty))
        .union(Nil: Seq[ReqlArray[ReqlJsonObject]]) =%=[ReqlFiniteStream[ReqlObject]] """[44,[[38,[[173,[1,4]],[69,[[2,[0]],{}]]]]]]"""

      r.range(1, 4).map(_ => emptyReqlObject)
        .union(Seq(
          bcdJsonTable,
          r.expr(Seq(emptyReqlObject))
        )) =%=[ReqlFiniteStream[ReqlObject]] """[44,[[38,[[173,[1,4]],[69,[[2,[0]],{}]]]],[15,["bcd"]],[2,[{}]]]]"""

      r.range(1, 4).map(_ => r.expr(JsonObject.empty))
        .union(Seq(
          r.range().map(_ => r.expr(JsonObject.empty))
        )) =%=[ReqlInfiniteStream[ReqlJsonObject]] """[44,[[38,[[173,[1,4]],[69,[[2,[0]],{}]]]],[38,[[173,[]],[69,[[2,[0]],{}]]]]]]"""

      r.range(1, 4).map(_ => JsonObject.empty: ReqlModel[JsonObject, String])
        .union(Seq(
          r.table[JsonObject, String]("bcd"),
          r.expr(Seq(JsonObject.empty: ReqlModel[JsonObject, String]))
        ), MergeSort("code")) =%=[ReqlFiniteStream[ReqlModel[JsonObject, String]]] """[44,[[38,[[173,[1,4]],[69,[[2,[0]],{}]]]],[15,["bcd"]],[2,[{}]]],{"interleave":"code"}]"""


      //infiniteStream
      r.range().map(_ => r.expr(JsonObject.empty))
        .union(Nil: Seq[ReqlArray[ReqlJsonObject]]) =%=[ReqlInfiniteStream[ReqlObject]] """[44,[[38,[[173,[]],[69,[[2,[0]],{}]]]]]]"""

      r.range().map(_ => emptyReqlObject)
        .union(Seq(
          r.expr(Seq(emptyReqlObject))
        )) =%=[ReqlInfiniteStream[ReqlObject]] """[44,[[38,[[173,[]],[69,[[2,[0]],{}]]]],[2,[{}]]]]"""

      r.range().map(_ => r.expr(JsonObject.empty))
        .union(Seq(
          r.range().map(_ => r.expr(JsonObject.empty))
        )) =%=[ReqlInfiniteStream[ReqlJsonObject]] """[44,[[38,[[173,[]],[69,[[2,[0]],{}]]]],[38,[[173,[]],[69,[[2,[0]],{}]]]]]]"""

      r.range().map(_ => JsonObject.empty: ReqlModel[JsonObject, String])
        .union(Seq(
          bcdJsonTable,
          r.expr(Seq(JsonObject.empty: ReqlModel[JsonObject, String]))
        )) =%=[ReqlInfiniteStream[ReqlModel[JsonObject, String]]] """[44,[[38,[[173,[]],[69,[[2,[0]],{}]]]],[15,["bcd"]],[2,[{}]]]]"""

      r.range().map(_ => JsonObject.empty: ReqlModel[JsonObject, String])
        .union(Seq(
          bcdJsonTable,
          r.expr(Seq(JsonObject.empty: ReqlModel[JsonObject, String]))
        ), MergeSort("code")) =%=[ReqlInfiniteStream[ReqlModel[JsonObject, String]]] """[44,[[38,[[173,[]],[69,[[2,[0]],{}]]]],[15,["bcd"]],[2,[{}]]],{"interleave":"code"}]"""


      //array
      r.expr(Seq(r.expr(JsonObject.empty)))
        .union(Nil: Seq[ReqlArray[ReqlJsonObject]]) =%=[ReqlArray[ReqlObject]] """[44,[[2,[{}]]]]"""

      r.expr(Seq(r.expr(123)))
        .union(Seq(
          r.expr(Seq(r.expr(234), r.expr(345)))
        )) =%=[ReqlArray[ReqlInteger]] """[44,[[2,[123]],[2,[234,345]]]]"""

      r.expr(Seq(JsonObject.empty: ReqlModel[JsonObject, String]))
        .union(Seq(
          abcJsonTable.orderBy(r.asc("code")),
          r.expr(Seq(JsonObject.empty: ReqlModel[JsonObject, String]))
        )) =%=[ReqlArray[ReqlModel[JsonObject, String]]] """[44,[[2,[{}]],[41,[[15,["abc"]],[73,["code"]]]],[2,[{}]]]]"""

      r.expr(Seq(JsonObject.empty: ReqlModel[JsonObject, String]))
        .union(Seq(
          bcdJsonTable,
          r.expr(Seq(JsonObject.empty: ReqlModel[JsonObject, String]))
        )) =%=[ReqlFiniteStream[ReqlModel[JsonObject, String]]] """[44,[[2,[{}]],[15,["bcd"]],[2,[{}]]]]"""

      r.expr(Seq(JsonObject.empty: ReqlModel[JsonObject, String]))
        .union(Seq(
          r.range().map(_ => JsonObject.empty: ReqlModel[JsonObject, String]),
          r.expr(Seq(JsonObject.empty: ReqlModel[JsonObject, String]))
        )) =%=[ReqlInfiniteStream[ReqlModel[JsonObject, String]]] """[44,[[2,[{}]],[38,[[173,[]],[69,[[2,[0]],{}]]]],[2,[{}]]]]"""

      r.expr(Seq(JsonObject.empty: ReqlModel[JsonObject, String]))
        .union(Seq(
          bcdJsonTable,
          r.expr(Seq(JsonObject.empty: ReqlModel[JsonObject, String]))
        ), MergeSort("code")) =%=[ReqlFiniteStream[ReqlModel[JsonObject, String]]] """[44,[[2,[{}]],[15,["bcd"]],[2,[{}]]],{"interleave":"code"}]"""


      //options
      r.expr(Seq(r.expr(JsonObject.empty)))
        .union(Seq(
          r.expr(Seq(r.expr(JsonObject.empty)))
        )) =%=[ReqlArray[ReqlObject]] """[44,[[2,[{}]],[2,[{}]]]]"""

      r.expr(Seq(r.expr(JsonObject.empty)))
        .union(Seq(
          r.expr(Seq(r.expr(JsonObject.empty)))
        ), Mix) =%=[ReqlArray[ReqlObject]] """[44,[[2,[{}]],[2,[{}]]]]"""

      r.expr(Seq(r.expr(JsonObject.empty)))
        .union(Seq(
          r.expr(Seq(r.expr(JsonObject.empty)))
        ), PreventMixing) =%=[ReqlArray[ReqlObject]] """[44,[[2,[{}]],[2,[{}]]],{"interleave":false}]"""

      r.expr(Seq(r.expr(JsonObject.empty)))
        .union(Seq(
          r.expr(Seq(r.expr(JsonObject.empty)))
        ), MergeSort("code")) =%=[ReqlArray[ReqlObject]] """[44,[[2,[{}]],[2,[{}]]],{"interleave":"code"}]"""
    }

    "nth" in {
      //table
      abcJsonTable.nth(3) =%=[ReqlSelectionOfObject[JsonObject, String]] """[45,[[15,["abc"]],3]]"""

      //tableSlice
      abcJsonTable.orderBy(OrderedIndex(r.asc("code"))).nth(3) =%=[ReqlSelectionOfObject[JsonObject, String]] """[45,[[41,[[15,["abc"]]],{"index":[73,["code"]]}],3]]"""

      //selectionOfArray
      abcJsonTable.orderBy(r.asc("code")).nth(3) =%=[ReqlSelectionOfObject[JsonObject, String]] """[45,[[41,[[15,["abc"]],[73,["code"]]]],3]]"""

      //selectionOfStream
      abcJsonTable.skip(10).nth(3) =%=[ReqlSelectionOfObject[JsonObject, String]] """[45,[[70,[[15,["abc"]],10]],3]]"""

      //should not work on stream
      """r.table("abc").get("uuid").changes().nth(3)""".shouldNot(compile)

      //array
      r.expr(Seq(r.expr(1), r.expr(2), r.expr(3), r.now(), r.expr(JsonObject.empty))).nth(3) =%=[ReqlDatum] """[45,[[2,[1,2,3,[103,[]],{}]],3]]"""
    }

    "bracket" in {
      //table
      r.table[JsonObject, String]("table").apply("field") =%=[ReqlFiniteStream[ReqlModel[JsonObject, String]]] """[170,[[15,["table"]],"field"]]"""

      //tableSlice
      r.table("abc").orderBy(OrderedIndex(r.asc("code")))("field") =%=[ReqlFiniteStream[ReqlDatum]] """[170,[[41,[[15,["abc"]]],{"index":[73,["code"]]}],"field"]]"""

      //selectionOfArray
      r.table("abc").orderBy(r.asc("code"))("field") =%=[ReqlArray[ReqlDatum]] """[170,[[41,[[15,["abc"]],[73,["code"]]]],"field"]]"""
      abcJsonTable.orderBy(r.asc("code"))(3) =%=[ReqlSelectionOfObject[JsonObject, String]] """[170,[[41,[[15,["abc"]],[73,["code"]]]],3]]"""

      //selectionOfStream
      r.table("abc").skip(10)("field") =%=[ReqlFiniteStream[ReqlDatum]] """[170,[[70,[[15,["abc"]],10]],"field"]]"""
      abcJsonTable.skip(10)(3) =%=[ReqlSelectionOfObject[JsonObject, String]] """[170,[[70,[[15,["abc"]],10]],3]]"""

      //infiniteStream
      r.range().map(x => r.expr(Map("code" -> x)))("code") =%=[ReqlInfiniteStream[ReqlDatum]] """[170,[[38,[[173,[]],[69,[[2,[0]],{"code":[10,[0]]}]]]],"code"]]"""

      //finiteStream
      r.range(10).map(x => r.expr(Map("code" -> x)))("code") =%=[ReqlFiniteStream[ReqlDatum]] """[170,[[38,[[173,[10]],[69,[[2,[0]],{"code":[10,[0]]}]]]],"code"]]"""
      r.range(10)(7) =%=[ReqlDatum] """[170,[[173,[10]],7]]"""
      r.table("table").apply("abc")("def") =%=[ReqlFiniteStream[ReqlDatum]] """[170,[[170,[[15,["table"]],"abc"]],"def"]]"""
      /*r.range(4).map { x =>
        r.polygon(
          r.point(r.expr(0), r.expr(0)),
          r.point(r.expr(0), r.expr(10)),
          r.point(r.expr(x), r.expr(x))
        )
      }*/

      //changefeed
      r.table("abc").changes()("new_val") =%=[ReqlInfiniteStream[ReqlDatum]] """[170,[[152,[[15,["abc"]]]],"new_val"]]"""
      """r.table("abc").get("uuid").changes()(3)""".shouldNot(compile)

      //array
      r.expr(Seq(
        r.expr(1), r.expr(2), r.expr(3)
      ))(2) =%=[ReqlDatum] """[170,[[2,[1,2,3]],2]]"""

      //selectionOfObject
      jsonTable("table").get("uuid")("field") =%=[ReqlDatum] """[170,[[16,[[15,["table"]],"uuid"]],"field"]]"""

      //object
      r.expr(Map(
        "test" -> r.expr(123)
      ))("test") =%=[ReqlDatum] """[170,[{"test":123},"test"]]"""
    }

    "inner_join" in {
      //table
      abcJsonTable.innerJoin(
        r.expr(Seq(r.expr(4), r.expr(5), r.expr(6))),
        (x: ReqlModel[JsonObject, String], y: ReqlInteger) => r.expr(true)
      ) =%=[ReqlFiniteStream[ReqlObject]] """[48,[[15,["abc"]],[2,[4,5,6]],[69,[[2,[0,1]],true]]]]"""

      // Cannot call a terminal (`reduce`, `count`, etc.) on an infinite stream (such as a changefeed)
      """
        |r.table("abc").innerJoin(
        |  r.table("bcd").changes(),
        |  (x, y) => r.expr(true)
        |)
      """.stripMargin.shouldNot(compile)


      //tableSlice
      abcJsonTable.orderBy(OrderedIndex(r.asc("code"))).innerJoin(
        r.expr(Seq(r.expr(4), r.expr(5), r.expr(6))),
        (x: ReqlModel[JsonObject, String], y: ReqlInteger) => r.expr(true)
      ) =%=[ReqlFiniteStream[ReqlObject]] """[48,[[41,[[15,["abc"]]],{"index":[73,["code"]]}],[2,[4,5,6]],[69,[[2,[0,1]],true]]]]"""

      // Cannot call a terminal (`reduce`, `count`, etc.) on an infinite stream (such as a changefeed)
      """
        |r.table("abc").orderBy(OrderedIndex(r.asc("code"))).innerJoin(
        |  r.table("bcd").changes(),
        |  (x, y) => r.expr(true)
        |)
      """.stripMargin.shouldNot(compile)


      //selectionOfStream
      abcJsonTable.skip(10).innerJoin(
        r.expr(Seq(r.expr(4), r.expr(5), r.expr(6))),
        (x: ReqlModel[JsonObject, String], y: ReqlInteger) => r.expr(true)
      ) =%=[ReqlFiniteStream[ReqlObject]] """[48,[[70,[[15,["abc"]],10]],[2,[4,5,6]],[69,[[2,[0,1]],true]]]]"""

      // Cannot call a terminal (`reduce`, `count`, etc.) on an infinite stream (such as a changefeed)
      """
        |r.table("abc").skip(10).innerJoin(
        |  r.table("bcd").changes(),
        |  (x, y) => r.expr(true)
        |)
      """.stripMargin.shouldNot(compile)


      //infiniteStream
      r.range().innerJoin(r.range(2), (x, y: ReqlInteger) => true) =%=[ReqlInfiniteStream[ReqlDatum]] """[48,[[173,[]],[173,[2]],[69,[[2,[0,1]],true]]]]"""
      """
        |r.range().innerJoin(r.range(), (x, y) => true)
      """.stripMargin.shouldNot(compile)

      //finiteStream
      r.range(10).innerJoin(r.range(2), (x, y: ReqlInteger) => true) =%=[ReqlFiniteStream[ReqlObject]] """[48,[[173,[10]],[173,[2]],[69,[[2,[0,1]],true]]]]"""
      """
        |r.range(10).innerJoin(r.range(), (x, y) => true)
      """.stripMargin.shouldNot(compile)


      //changefeed
      abcJsonTable.get("uuid").changes().innerJoin(
        r.expr(Seq(r.expr(4), r.expr(5), r.expr(6))),
        (x: ReqlObject, y: ReqlInteger) => r.expr(true)
      ) =%=[ReqlInfiniteStream[ReqlDatum]] """[48,[[152,[[16,[[15,["abc"]],"uuid"]]]],[2,[4,5,6]],[69,[[2,[0,1]],true]]]]"""

      // Cannot call a terminal (`reduce`, `count`, etc.) on an infinite stream (such as a changefeed)
      """
        |r.table("abc").get("uuid").changes().innerJoin(
        |  r.table("bcd").changes(),
        |  (x, y) => r.expr(true)
        |)
      """.stripMargin.shouldNot(compile)


      //selectionOfArray
      abcJsonTable.orderBy(r.asc("code")).innerJoin(
        r.expr(Seq(r.expr(4), r.expr(5), r.expr(6))),
        (x: ReqlModel[JsonObject, String], y: ReqlInteger) => r.expr(true)
      ) =%=[ReqlArray[ReqlDatum]] """[48,[[41,[[15,["abc"]],[73,["code"]]]],[2,[4,5,6]],[69,[[2,[0,1]],true]]]]"""

      abcJsonTable.orderBy(r.asc("code")).innerJoin[ReqlObject](
        r.table("abc").orderBy(r.asc("code")),
        (x: ReqlModel[JsonObject, String], y: ReqlObject) => r.expr(true)
      ) =%=[ReqlArray[ReqlDatum]] """[48,[[41,[[15,["abc"]],[73,["code"]]]],[41,[[15,["abc"]],[73,["code"]]]],[69,[[2,[0,1]],true]]]]"""

      abcJsonTable.orderBy(r.asc("code")).innerJoin[ReqlObject](
        r.table("bcd"),
        (x: ReqlModel[JsonObject, String], y: ReqlObject) => r.expr(true)
      ) =%=[ReqlArray[ReqlDatum]] """[48,[[41,[[15,["abc"]],[73,["code"]]]],[15,["bcd"]],[69,[[2,[0,1]],true]]]]"""

      // Cannot call a terminal (`reduce`, `count`, etc.) on an infinite stream (such as a changefeed)
      """
        |r.table("abc").orderBy(r.asc("code")).innerJoin(
        |  r.table("bcd").changes(),
        |  (x, y) => r.expr(true)
        |)
      """.stripMargin.shouldNot(compile)


      //array
      r.expr(Seq(r.expr(1), r.expr(2), r.expr(3))).innerJoin(
        r.expr(Seq(r.expr(4), r.expr(5), r.expr(6))),
        (x, y: ReqlInteger) => r.expr(true)
      ) =%=[ReqlArray[ReqlJoinResult[ReqlInteger,ReqlInteger]]] """[48,[[2,[1,2,3]],[2,[4,5,6]],[69,[[2,[0,1]],true]]]]"""

      r.expr(Seq(r.expr(1), r.expr(2), r.expr(3))).innerJoin[ReqlObject](
        r.table("abc").orderBy(r.asc("code")),
        (x, y: ReqlObject) => r.expr(true)
      ) =%=[ReqlArray[ReqlDatum]] """[48,[[2,[1,2,3]],[41,[[15,["abc"]],[73,["code"]]]],[69,[[2,[0,1]],true]]]]"""

      // Cannot call a terminal (`reduce`, `count`, etc.) on an infinite stream (such as a changefeed)
      """
        |r.expr(Seq(r.expr(1), r.expr(2), r.expr(3))).innerJoin(
        |  r.table("bcd").changes(),
        |  (x, y) => r.expr(true)
        |)
      """.stripMargin.shouldNot(compile)
    }

    "outer_join" in {
      //table
      abcJsonTable.outerJoin[ReqlInteger](
        r.expr(Seq(r.expr(4), r.expr(5), r.expr(6))),
        (x: ReqlModel[JsonObject, String], y: ReqlInteger) => r.expr(true)
      ) =%=[ReqlFiniteStream[ReqlObject]] """[49,[[15,["abc"]],[2,[4,5,6]],[69,[[2,[0,1]],true]]]]"""

      // Cannot call a terminal (`reduce`, `count`, etc.) on an infinite stream (such as a changefeed)
      """
        |r.table("abc").outerJoin(
        |  r.table("bcd").changes(),
        |  (x, y) => r.expr(true)
        |)
      """.stripMargin.shouldNot(compile)


      //tableSlice
      abcJsonTable.orderBy(OrderedIndex(r.asc("code"))).outerJoin[ReqlInteger](
        r.expr(Seq(r.expr(4), r.expr(5), r.expr(6))),
        (x, y) => r.expr(true)
      ) =%=[ReqlFiniteStream[ReqlObject]] """[49,[[41,[[15,["abc"]]],{"index":[73,["code"]]}],[2,[4,5,6]],[69,[[2,[0,1]],true]]]]"""

      // Cannot call a terminal (`reduce`, `count`, etc.) on an infinite stream (such as a changefeed)
      """
        |r.table("abc").orderBy(OrderedIndex(r.asc("code"))).outerJoin(
        |  r.table("bcd").changes(),
        |  (x, y) => r.expr(true)
        |)
      """.stripMargin.shouldNot(compile)


      //selectionOfStream
      abcJsonTable.skip(10).outerJoin[ReqlInteger](
        r.expr(Seq(r.expr(4), r.expr(5), r.expr(6))),
        (x, y) => r.expr(true)
      ) =%=[ReqlFiniteStream[ReqlObject]] """[49,[[70,[[15,["abc"]],10]],[2,[4,5,6]],[69,[[2,[0,1]],true]]]]"""

      // Cannot call a terminal (`reduce`, `count`, etc.) on an infinite stream (such as a changefeed)
      """
        |r.table("abc").skip(10).outerJoin(
        |  r.table("bcd").changes(),
        |  (x, y) => r.expr(true)
        |)
      """.stripMargin.shouldNot(compile)


      //infiniteStream
      r.range().outerJoin[ReqlInteger](r.range(10), (x, y) => r.expr(true)) =%=[ReqlInfiniteStream[ReqlJoinResult[ReqlInteger, ReqlInteger]]] """[49,[[173,[]],[173,[10]],[69,[[2,[0,1]],true]]]]"""
      """
        |r.range().outerJoin(r.range(), (x, y) => r.expr(true))
      """.stripMargin.shouldNot(compile)


      //finiteStream
      r.range(10).outerJoin[ReqlInteger](r.range(10), (x, y) => r.expr(true)) =%=[ReqlFiniteStream[ReqlObject]] """[49,[[173,[10]],[173,[10]],[69,[[2,[0,1]],true]]]]"""
      """
        |r.range(10).outerJoin(r.range(), (x, y) => r.expr(true))
      """.stripMargin.shouldNot(compile)


      //changefeed
      abcJsonTable.get("uuid").changes().outerJoin[ReqlInteger](
        r.expr(Seq(r.expr(4), r.expr(5), r.expr(6))),
        (x, y) => r.expr(true)
      ) =%=[ReqlInfiniteStream[ReqlDatum]] """[49,[[152,[[16,[[15,["abc"]],"uuid"]]]],[2,[4,5,6]],[69,[[2,[0,1]],true]]]]"""

      // Cannot call a terminal (`reduce`, `count`, etc.) on an infinite stream (such as a changefeed)
      """
        |r.table("abc").get("uuid").changes().outerJoin(
        |  r.table("bcd").changes(),
        |  (x, y) => r.expr(true)
        |)
      """.stripMargin.shouldNot(compile)


      //selectionOfArray
      abcJsonTable.orderBy(r.asc("code")).outerJoin[ReqlInteger](
        r.expr(Seq(r.expr(4), r.expr(5), r.expr(6))),
        (x, y) => r.expr(true)
      ) =%=[ReqlArray[ReqlJoinResult[ReqlModel[JsonObject, String], ReqlInteger]]] """[49,[[41,[[15,["abc"]],[73,["code"]]]],[2,[4,5,6]],[69,[[2,[0,1]],true]]]]"""

      abcJsonTable.orderBy(r.asc("code")).outerJoin[ReqlModel[JsonObject, String]](
        abcJsonTable.orderBy(r.asc("code")),
        (x, y) => r.expr(true)
      ) =%=[ReqlArray[ReqlJoinResult[ReqlModel[JsonObject, String], ReqlModel[JsonObject, String]]]] """[49,[[41,[[15,["abc"]],[73,["code"]]]],[41,[[15,["abc"]],[73,["code"]]]],[69,[[2,[0,1]],true]]]]"""

      abcJsonTable.orderBy(r.asc("code")).outerJoin[ReqlModel[JsonObject, String]](
        bcdJsonTable,
        (x, y) => r.expr(true)
      ) =%=[ReqlArray[ReqlJoinResult[ReqlModel[JsonObject, String], ReqlModel[JsonObject, String]]]] """[49,[[41,[[15,["abc"]],[73,["code"]]]],[15,["bcd"]],[69,[[2,[0,1]],true]]]]"""

      // Cannot call a terminal (`reduce`, `count`, etc.) on an infinite stream (such as a changefeed)
      """
        |r.table("abc").orderBy(r.asc("code")).outerJoin(
        |  r.table("bcd").changes(),
        |  (x, y) => r.expr(true)
        |)
      """.stripMargin.shouldNot(compile)


      //array
      r.expr(Seq(r.expr(1), r.expr(2), r.expr(3))).outerJoin[ReqlInteger](
        r.expr(Seq(r.expr(4), r.expr(5), r.expr(6))),
        (x, y) => r.expr(true)
      ) =%=[ReqlArray[ReqlJoinResult[ReqlInteger, ReqlInteger]]] """[49,[[2,[1,2,3]],[2,[4,5,6]],[69,[[2,[0,1]],true]]]]"""

      r.expr(Seq(r.expr(1), r.expr(2), r.expr(3))).outerJoin[ReqlObject](
        r.table("abc").orderBy(r.asc("code")),
        (x, y) => r.expr(true)
      ) =%=[ReqlArray[ReqlJoinResult[ReqlInteger, ReqlObject]]] """[49,[[2,[1,2,3]],[41,[[15,["abc"]],[73,["code"]]]],[69,[[2,[0,1]],true]]]]"""

      // Cannot call a terminal (`reduce`, `count`, etc.) on an infinite stream (such as a changefeed)
      """
        |r.expr(Seq(r.expr(1), r.expr(2), r.expr(3))).outerJoin(
        |  r.table("bcd").changes(),
        |  (x, y) => r.expr(true)
        |)
      """.stripMargin.shouldNot(compile)
    }

    "eq_join" in {
      //table
      abcJsonTable.eqJoin("code", bcdJsonTable) =%=[ReqlFiniteStream[ReqlObject]] """[50,[[15,["abc"]],"code",[15,["bcd"]]]]"""
      abcJsonTable.eqJoin("code", bcdJsonTable, ordered = Ordered) =%=[ReqlFiniteStream[ReqlObject]] """[50,[[15,["abc"]],"code",[15,["bcd"]]],{"ordered":true}]"""
      abcJsonTable.eqJoin("code", bcdJsonTable, Index("code")) =%=[ReqlFiniteStream[ReqlObject]] """[50,[[15,["abc"]],"code",[15,["bcd"]]],{"index":"code"}]"""
      abcJsonTable.eqJoin("code", bcdJsonTable, Index("code"), Ordered) =%=[ReqlFiniteStream[ReqlObject]] """[50,[[15,["abc"]],"code",[15,["bcd"]]],{"index":"code","ordered":true}]"""

      abcJsonTable.eqJoin({x: ReqlModel[JsonObject, String] => r.expr(123)}, bcdJsonTable) =%=[ReqlFiniteStream[ReqlObject]] """[50,[[15,["abc"]],[69,[[2,[0]],123]],[15,["bcd"]]]]"""
      abcJsonTable.eqJoin({x: ReqlModel[JsonObject, String] => r.expr(123)}, bcdJsonTable, ordered = Ordered) =%=[ReqlFiniteStream[ReqlObject]] """[50,[[15,["abc"]],[69,[[2,[0]],123]],[15,["bcd"]]],{"ordered":true}]"""
      abcJsonTable.eqJoin({x: ReqlModel[JsonObject, String] => r.expr(123)}, bcdJsonTable, Index("code")) =%=[ReqlFiniteStream[ReqlObject]] """[50,[[15,["abc"]],[69,[[2,[0]],123]],[15,["bcd"]]],{"index":"code"}]"""
      abcJsonTable.eqJoin({x: ReqlModel[JsonObject, String] => r.expr(123)}, bcdJsonTable, Index("code"), Ordered) =%=[ReqlFiniteStream[ReqlObject]] """[50,[[15,["abc"]],[69,[[2,[0]],123]],[15,["bcd"]]],{"index":"code","ordered":true}]"""


      //tableSlice
      abcJsonTable.orderBy(OrderedIndex(r.asc("code"))).
        eqJoin("code", bcdJsonTable, Index("code")) =%=[ReqlFiniteStream[ReqlObject]] """[50,[[41,[[15,["abc"]]],{"index":[73,["code"]]}],"code",[15,["bcd"]]],{"index":"code"}]"""

      abcJsonTable.orderBy(OrderedIndex(r.asc("code"))).
        eqJoin({x: ReqlModel[JsonObject, String] => r.expr(123)}, bcdJsonTable, Index("code")) =%=[ReqlFiniteStream[ReqlObject]] """[50,[[41,[[15,["abc"]]],{"index":[73,["code"]]}],[69,[[2,[0]],123]],[15,["bcd"]]],{"index":"code"}]"""


      //selectionOfStream
      abcJsonTable.skip(10).
        eqJoin("code", bcdJsonTable, Index("code")) =%=[ReqlFiniteStream[ReqlObject]] """[50,[[70,[[15,["abc"]],10]],"code",[15,["bcd"]]],{"index":"code"}]"""

      abcJsonTable.skip(10).
        eqJoin({x: ReqlModel[JsonObject, String] => r.expr(123)}, bcdJsonTable, Index("code")) =%=[ReqlFiniteStream[ReqlObject]] """[50,[[70,[[15,["abc"]],10]],[69,[[2,[0]],123]],[15,["bcd"]]],{"index":"code"}]"""


      //infiniteStream
      r.range().map(x => r.expr(Map("code" -> x))).
        eqJoin("code", r.table("bcd"), Index("code")) =%=[ReqlInfiniteStream[ReqlObject]] """[50,[[38,[[173,[]],[69,[[2,[0]],{"code":[10,[0]]}]]]],"code",[15,["bcd"]]],{"index":"code"}]"""


      //finiteStream
      r.range(10).map(x => r.expr(Map("code" -> x))).
        eqJoin("code", r.table("bcd"), Index("code")) =%=[ReqlFiniteStream[ReqlObject]] """[50,[[38,[[173,[10]],[69,[[2,[0]],{"code":[10,[0]]}]]]],"code",[15,["bcd"]]],{"index":"code"}]"""


      //changefeed
      abcJsonTable.get("uuid").changes().
        eqJoin("code", r.table("bcd"), Index("code")) =%=[ReqlInfiniteStream[ReqlDatum]] """[50,[[152,[[16,[[15,["abc"]],"uuid"]]]],"code",[15,["bcd"]]],{"index":"code"}]"""

      abcJsonTable.get("uuid").changes().
        eqJoin({x: ReqlObject => r.expr(123)}, r.table("bcd"), Index("code")) =%=[ReqlInfiniteStream[ReqlDatum]] """[50,[[152,[[16,[[15,["abc"]],"uuid"]]]],[69,[[2,[0]],123]],[15,["bcd"]]],{"index":"code"}]"""


      //selectionOfArray
      r.table[JsonObject, String]("abc").orderBy(r.asc("code")).
        eqJoin("code", r.table[JsonObject, String]("bcd"), Index("code")) =%=[ReqlArray[ReqlDatum]] """[50,[[41,[[15,["abc"]],[73,["code"]]]],"code",[15,["bcd"]]],{"index":"code"}]"""

      r.table[JsonObject, String]("abc").orderBy(r.asc("code")).
        eqJoin({x: ReqlModel[JsonObject, String] => r.expr(123)}, r.table[JsonObject, String]("bcd"), Index("code")) =%=[ReqlArray[ReqlDatum]] """[50,[[41,[[15,["abc"]],[73,["code"]]]],[69,[[2,[0]],123]],[15,["bcd"]]],{"index":"code"}]"""


      //array
      r.expr(Seq(
        r.expr(Map("code" -> r.expr(123))),
        r.expr(Map("code" -> r.expr(234)))
      )).eqJoin("code", r.table("abc")) =%=[ReqlArray[ReqlDatum]] """[50,[[2,[{"code":123},{"code":234}]],"code",[15,["abc"]]]]"""

      r.expr(Seq(
        r.expr(Map("code" -> r.expr(123))),
        r.expr(Map("code" -> r.expr(234)))
      )).eqJoin("code", r.table("abc"), ordered = Ordered) =%=[ReqlArray[ReqlDatum]] """[50,[[2,[{"code":123},{"code":234}]],"code",[15,["abc"]]],{"ordered":true}]"""

      r.expr(Seq(
        r.expr(Map("code" -> r.expr(123))),
        r.expr(Map("code" -> r.expr(234)))
      )).eqJoin("code", r.table("abc"), Index("code")) =%=[ReqlArray[ReqlDatum]] """[50,[[2,[{"code":123},{"code":234}]],"code",[15,["abc"]]],{"index":"code"}]"""

      r.expr(Seq(
        r.expr(Map("code" -> r.expr(123))),
        r.expr(Map("code" -> r.expr(234)))
      )).eqJoin("code", r.table("abc"), Index("code"), Ordered) =%=[ReqlArray[ReqlDatum]] """[50,[[2,[{"code":123},{"code":234}]],"code",[15,["abc"]]],{"index":"code","ordered":true}]"""
    }

    "zip" in {
      //infiniteStream
      r.range().innerJoin(r.range(2), (x: ReqlInteger, y: ReqlInteger) => true).zip() =%=[ReqlInfiniteStream[ReqlDatum]] """[72,[[48,[[173,[]],[173,[2]],[69,[[2,[0,1]],true]]]]]]"""

      //finiteStream
      r.table[JsonObject, String]("abc").eqJoin("code", r.table[JsonObject, String]("bcd")) shouldBe an[ReqlFiniteStream[ReqlDatum]]
      r.table[JsonObject, String]("abc").eqJoin("code", r.table[JsonObject, String]("bcd")).zip() =%=[ReqlFiniteStream[ReqlDatum]] """[72,[[50,[[15,["abc"]],"code",[15,["bcd"]]]]]]"""

      //array
      r.expr(Seq(
        JsonObject.fromMap(Map("code" -> Json.fromInt(123))): ReqlModel[JsonObject, String],
        JsonObject.fromMap(Map("code" -> Json.fromInt(234))): ReqlModel[JsonObject, String]
      )).eqJoin("code", r.table[JsonObject, String]("abc")).zip() =%=[ReqlArray[ReqlDatum]] """[72,[[50,[[2,[{"code":123},{"code":234}]],"code",[15,["abc"]]]]]]"""

      r.expr(Seq(
        JoinResult[JsonObject, JsonObject](
          left = JsonObject.fromMap(Map("test" -> Json.fromInt(123))),
          right = JsonObject.fromMap(Map("code" -> Json.fromInt(234)))
        ): ReqlJoinResult[JsonObject, JsonObject]
      )).zip() =%=[ReqlArray[ReqlDatum]] """[72,[[2,[{"left":{"test":123},"right":{"code":234}}]]]]"""

      //by docs it should not work on other sequence types
      //table
      """r.table("abc").zip()""".shouldNot(compile)

      //tableSlice
      """r.table("abc").orderBy(OrderedIndex(r.asc("code"))).zip()""".shouldNot(compile)

      //selectionOfStream
      """r.table("abc").skip(10).zip()""".shouldNot(compile)

      //selectionOfArray
      """r.table("abc").orderBy(r.asc("code")).zip()""".shouldNot(compile)
    }

    "range" in {
      //infinite
      r.range() shouldBe an[ReqlInfiniteStream[ReqlInteger]]
      r.range() =%=[ReqlInfiniteStream[ReqlInteger]] """[173,[]]"""

      //finite with end only
      r.range(4) shouldBe an[ReqlFiniteStream[ReqlInteger]]
      r.range(4) =%=[ReqlFiniteStream[ReqlInteger]] """[173,[4]]"""

      //finite with start and end
      r.range(-5, 6) shouldBe an[ReqlFiniteStream[ReqlInteger]]
      r.range(-5, 6) =%=[ReqlFiniteStream[ReqlInteger]] """[173,[-5,6]]"""

      //should work only with integers
      """r.range(r.expr(BigDecimal(1.2)))""".shouldNot(compile)

      //example from documentation
      r.range().limit(4) =%=[ReqlInfiniteStream[ReqlInteger]] """[71,[[173,[]],4]]"""
    }

    "insert_at" in {
      //array only
      val a: ReqlArray[ReqlDatum] = r.expr(Seq(r.expr(123), r.expr("test"))).insertAt(1, r.now())
      r.expr(Seq(r.expr(123), r.expr("test"))).insertAt(1, r.now()) =%=[ReqlArray[ReqlDatum]] """[82,[[2,[123,"test"]],1,[103,[]]]]"""

      //should not work on selectionOfArray
      abcJsonTable.orderBy(r.asc("code")) shouldBe an[ReqlSelectionOfArray[ReqlObject, UUID]]
      """r.table("abc").orderBy(r.asc("code")).insertAt(1, r.now())""".shouldNot(compile)
    }

    "delete_at" in {
      //array only
      r.expr(Seq(r.expr(123), r.expr("test"))).deleteAt(1) =%=[ReqlArray[ReqlValue]] """[83,[[2,[123,"test"]],1]]"""
      r.expr(Seq(r.expr(123), r.expr("test"), r.expr(234), r.expr(345))).deleteAt(1, 3) =%=[ReqlArray[ReqlValue]] """[83,[[2,[123,"test",234,345]],1,3]]"""
      r.expr(Seq(r.expr(123), r.expr("test"), r.expr(234), r.expr(345))).deleteAt(-2) =%=[ReqlArray[ReqlValue]] """[83,[[2,[123,"test",234,345]],-2]]"""

      //should not work on selectionOfArray
      abcJsonTable.orderBy(r.asc("code")) shouldBe an[ReqlSelectionOfArray[ReqlObject, UUID]]
      """r.table("abc").orderBy(r.asc("code")).deleteAt(1)""".shouldNot(compile)
    }

    "change_at" in {
      //array only
      val a: ReqlArray[ReqlDatum] = r.expr(Seq(r.expr(123), r.expr("test"))).changeAt(1, r.now())
      r.expr(Seq(r.expr(123), r.expr("test"))).changeAt(1, r.now()) =%=[ReqlArray[ReqlDatum]] """[84,[[2,[123,"test"]],1,[103,[]]]]"""

      //should not work on selectionOfArray
      abcJsonTable.orderBy(r.asc("code")) shouldBe an[ReqlSelectionOfArray[ReqlObject, UUID]]
      """r.table("abc").orderBy(r.asc("code")).changeAt(1, r.now())""".shouldNot(compile)
    }

    "splice_at" in {
      //array only
      val a: ReqlArray[ReqlValue] = r.expr(Seq(r.expr(123), r.expr("test")))
      val b: ReqlArray[ReqlDatum] = r.expr(Seq(r.now(), r.expr("code")))
      val c: ReqlArray[ReqlDatum] = a.spliceAt(1, b)
      val d: ReqlArray[ReqlDatum] = b.spliceAt(1, a)
      r.expr(Seq(r.expr(123), r.expr("test"))).spliceAt(1, r.expr(Seq(r.now(), r.expr("code")))) =%=[ReqlArray[ReqlDatum]] """[85,[[2,[123,"test"]],1,[2,[[103,[]],"code"]]]]"""

      //should not work on selectionOfArray
      abcJsonTable.orderBy(r.asc("code")) shouldBe an[ReqlSelectionOfArray[ReqlObject, UUID]]
      """r.table("abc").orderBy(r.asc("code")).spliceAt(1, r.expr(Seq(r.now(), r.expr("code"))))""".shouldNot(compile)
    }

    "coerce_to" in {
      //string to float
      r.expr("123").coerceTo(`float`) =%=[ReqlFloat] """[51,["123","number"]]"""

      //to string
      r.expr(null).coerceTo(`string`) =%=[ReqlString] """[51,[null,"string"]]"""
      r.expr(true).coerceTo(`string`) =%=[ReqlString] """[51,[true,"string"]]"""
      r.expr(123).coerceTo(`string`) =%=[ReqlString] """[51,[123,"string"]]"""
      r.expr(BigDecimal(123.456)).coerceTo(`string`) =%=[ReqlString] """[51,[123.456,"string"]]"""
      r.expr("abc").coerceTo(`string`) =%=[ReqlString] """[51,["abc","string"]]"""
      (false :: 123 :: "abc" :: ReNil).coerceTo(`string`) =%=[ReqlString] """[51,[[2,[false,123,"abc"]],"string"]]"""
      (("abc" := 123) ~ ("bcd" := "cde")).coerceTo(`string`) =%=[ReqlString] """[51,[{"abc":123,"bcd":"cde"},"string"]]"""
      r.binary(ByteString(10, 20, 30, 40)).coerceTo(`string`) =%=[ReqlString] """[51,[{"$reql_type$":"BINARY","data":"ChQeKA=="},"string"]]"""
      abcJsonTable.get("uuid").coerceTo(`string`) =%=[ReqlString] """[51,[[16,[[15,["abc"]],"uuid"]],"string"]]"""

      //to array
      r.table("abc").coerceTo(`array`) =%=[ReqlArray[ReqlDatum]] """[51,[[15,["abc"]],"array"]]"""
      r.table("abc").orderBy(OrderedIndex(r.asc("code"))).coerceTo(`array`) =%=[ReqlArray[ReqlDatum]] """[51,[[41,[[15,["abc"]]],{"index":[73,["code"]]}],"array"]]"""
      r.table("abc").orderBy(r.asc("code")).coerceTo(`array`) =%=[ReqlArray[ReqlDatum]] """[51,[[41,[[15,["abc"]],[73,["code"]]]],"array"]]"""
      r.table("abc").skip(1).coerceTo(`array`) =%=[ReqlArray[ReqlDatum]] """[51,[[70,[[15,["abc"]],1]],"array"]]"""
      r.range(4).coerceTo(`array`) =%=[ReqlArray[ReqlDatum]] """[51,[[173,[4]],"array"]]"""
      """r.range().coerceTo(`array`)""".shouldNot(compile)
      (false :: 123 :: "abc" :: ReNil).coerceTo(`array`) =%=[ReqlArray[ReqlDatum]] """[51,[[2,[false,123,"abc"]],"array"]]"""
      (("abc" := 123) ~ ("bcd" := "cde")).coerceTo(`array`) =%=[ReqlArray[ReqlDatum]] """[51,[{"abc":123,"bcd":"cde"},"array"]]"""
      abcJsonTable.get("uuid").coerceTo(`array`) =%=[ReqlArray[ReqlDatum]] """[51,[[16,[[15,["abc"]],"uuid"]],"array"]]"""

      //to object
      r.range(4).coerceTo(`object`) =%=[ReqlObject] """[51,[[173,[4]],"object"]]"""
      (("abc" :: 123 :: ReNil) :: ("bcd" :: "cde" :: ReNil) :: ReNil).coerceTo(`object`) =%=[ReqlObject] """[51,[[2,[[2,["abc",123]],[2,["bcd","cde"]]]],"object"]]"""
      (("abc" := 123) ~ ("bcd" := "cde")).coerceTo(`object`) =%=[ReqlObject] """[51,[{"abc":123,"bcd":"cde"},"object"]]"""
      abcJsonTable.get("uuid").coerceTo(`object`) =%=[ReqlObject] """[51,[[16,[[15,["abc"]],"uuid"]],"object"]]"""

      //to binary
      r.expr("abc").coerceTo(`binary`) =%=[ReqlBinary] """[51,["abc","binary"]]"""
    }

    "type_of" in {
      //TODO: cover all cases by documentation - it should be useful for integration tasting
      //array
      r.expr(Seq(r.expr(123), r.expr("test"))).typeOf() =%=[ReqlString] """[52,[[2,[123,"test"]]]]""" // -> ARRAY

      //bool
      r.expr(true).typeOf() =%=[ReqlString] """[52,[true]]""" // -> BOOL

      //maxval
      r.maxval.typeOf() =%=[ReqlString] """[52,[[181,[]]]]""" // -> MAXVAL

      //minval
      r.minval.typeOf() =%=[ReqlString] """[52,[[180,[]]]]""" // -> MINVAL

      //null
      r.expr(null).typeOf() =%=[ReqlString] """[52,[null]]""" // -> NULL
    }

    "update" in {
      //table
      r.table[JsonObject, String]("abc").update(r.expr(null)) =%=[ReqlModificationResult[JsonObject, String]] """[53,[[15,["abc"]],null]]"""
      r.table[JsonObject, String]("abc").update(JsonObject.empty) =%=[ReqlModificationResult[JsonObject, String]] """[53,[[15,["abc"]],{}]]"""
      r.table[JsonObject, String]("abc").update(JsonObject.empty) =%=[ReqlModificationResult[JsonObject, String]] """[53,[[15,["abc"]],{}]]"""
      r.table[JsonObject, String]("abc").
        update({x: ReqlModel[JsonObject, String] => JsonObject.empty: ReqlModel[JsonObject, String]}) =%=[ReqlModificationResult[JsonObject, String]] """[53,[[15,["abc"]],[69,[[2,[0]],{}]]]]"""

      abcJsonTable.update(
        JsonObject.empty,
        returnChanges = DoReturnChanges,
        nonAtomic = NonAtomicUpdate) =%=[ReqlModificationResult[JsonObject, String]] """[53,[[15,["abc"]],{}],{"return_changes":true,"non_atomic":true}]"""

      abcJsonTable.update(
        JsonObject.empty,
        durability = Hard,
        returnChanges = AlwaysReturnChanges,
        nonAtomic = NonAtomicUpdate) =%=[ReqlModificationResult[JsonObject, String]] """[53,[[15,["abc"]],{}],{"durability":"hard","return_changes":"always","non_atomic":true}]"""

      abcJsonTable.update(
        JsonObject.empty,
        durability = Soft,
        returnChanges = DoNotReturnChanges,
        nonAtomic = AtomicUpdate) =%=[ReqlModificationResult[JsonObject, String]] """[53,[[15,["abc"]],{}],{"durability":"soft"}]"""

      r.table[JsonObject, String]("abc").update(
        { x: ReqlModel[JsonObject, String] => JsonObject.empty: ReqlModel[JsonObject, String] },
        durability = Hard,
        returnChanges = AlwaysReturnChanges,
        nonAtomic = NonAtomicUpdate
      ) =%=[ReqlModificationResult[JsonObject, String]] """[53,[[15,["abc"]],[69,[[2,[0]],{}]]],{"durability":"hard","return_changes":"always","non_atomic":true}]"""

      //tableSlice
      abcJsonTable.orderBy(OrderedIndex(r.asc("code"))) shouldBe an[ReqlTableSlice[JsonObject, String]]
      abcJsonTable.orderBy(OrderedIndex(r.asc("code"))).update(
        JsonObject.empty,
        nonAtomic = NonAtomicUpdate
      ) =%=[ReqlModificationResult[JsonObject, String]] """[53,[[41,[[15,["abc"]]],{"index":[73,["code"]]}],{}],{"non_atomic":true}]"""

      //selectionOfStream
      abcJsonTable.skip(10) shouldBe an[ReqlSelectionOfStream[JsonObject, String]]
      abcJsonTable.skip(10).update(
        JsonObject.empty,
        nonAtomic = NonAtomicUpdate
      ) =%=[ReqlModificationResult[JsonObject, String]] """[53,[[70,[[15,["abc"]],10]],{}],{"non_atomic":true}]"""

      //selectionOfArray
      abcJsonTable.orderBy(r.asc("code")) shouldBe an[ReqlSelectionOfArray[ReqlObject, UUID]]
      abcJsonTable.orderBy(r.asc("code")).update(
        JsonObject.empty,
        nonAtomic = NonAtomicUpdate
      ) =%=[ReqlModificationResult[JsonObject, String]] """[53,[[41,[[15,["abc"]],[73,["code"]]]],{}],{"non_atomic":true}]"""

      //selectionOfObject
      abcJsonTable.get("uuid") shouldBe an[ReqlSelectionOfObject[JsonObject, String]]
      abcJsonTable.get("uuid").update(
        JsonObject.empty,
        nonAtomic = NonAtomicUpdate
      ) =%=[ReqlModificationResult[JsonObject, String]] """[53,[[16,[[15,["abc"]],"uuid"]],{}],{"non_atomic":true}]"""
    }

    "delete" in {
      //table
      abcJsonTable.delete() =%=[ReqlModificationResult[JsonObject, String]] """[54,[[15,["abc"]]]]"""

      abcJsonTable.delete(Hard) =%=[ReqlModificationResult[JsonObject, String]] """[54,[[15,["abc"]]],{"durability":"hard"}]"""
      abcJsonTable.delete(Soft) =%=[ReqlModificationResult[JsonObject, String]] """[54,[[15,["abc"]]],{"durability":"soft"}]"""

      abcJsonTable.delete(returnChanges = AlwaysReturnChanges) =%=[ReqlModificationResult[JsonObject, String]] """[54,[[15,["abc"]]],{"return_changes":"always"}]"""
      abcJsonTable.delete(Soft, DoReturnChanges) =%=[ReqlModificationResult[JsonObject, String]] """[54,[[15,["abc"]]],{"durability":"soft","return_changes":true}]"""

      //tableSlice
      abcJsonTable.orderBy(OrderedIndex(r.asc("code"))) shouldBe an[ReqlTableSlice[JsonObject, String]]
      abcJsonTable.orderBy(OrderedIndex(r.asc("code"))).delete() =%=[ReqlModificationResult[JsonObject, String]] """[54,[[41,[[15,["abc"]]],{"index":[73,["code"]]}]]]"""

      //selectionOfStream
      abcJsonTable.skip(10) shouldBe an[ReqlSelectionOfStream[JsonObject, String]]
      abcJsonTable.skip(10).delete() =%=[ReqlModificationResult[JsonObject, String]] """[54,[[70,[[15,["abc"]],10]]]]"""

      //selectionOfArray
      abcJsonTable.orderBy(r.asc("code")) shouldBe an[ReqlSelectionOfArray[JsonObject, String]]
      abcJsonTable.orderBy(r.asc("code")).delete() =%=[ReqlModificationResult[JsonObject, String]] """[54,[[41,[[15,["abc"]],[73,["code"]]]]]]"""

      //selectionOfObject
      abcJsonTable.get("uuid") shouldBe an[ReqlSelectionOfObject[JsonObject, String]]
      abcJsonTable.get("uuid").delete() =%=[ReqlModificationResult[JsonObject, String]] """[54,[[16,[[15,["abc"]],"uuid"]]]]"""
    }

    "replace" in {
      //table
      r.table[JsonObject, String]("abc").replace(r.expr(null)) =%=[ReqlModificationResult[JsonObject, String]] """[55,[[15,["abc"]],null]]"""
      r.table[JsonObject, String]("abc").replace(JsonObject.empty) =%=[ReqlModificationResult[JsonObject, String]] """[55,[[15,["abc"]],{}]]"""
      r.table[JsonObject, String]("abc").replace({x: ReqlModel[JsonObject, String] => x}) =%=[ReqlModificationResult[JsonObject, String]] """[55,[[15,["abc"]],[69,[[2,[0]],[10,[0]]]]]]"""
      r.table[JsonObject, String]("abc").replace(JsonObject.empty, Soft, AlwaysReturnChanges, NonAtomicUpdate) =%=[ReqlModificationResult[JsonObject, String]] """[55,[[15,["abc"]],{}],{"durability":"soft","return_changes":"always","non_atomic":true}]"""

      //tableSlice
      r.table[JsonObject, String]("abc").orderBy(OrderedIndex(r.asc("code"))) shouldBe an[ReqlTableSlice[JsonObject, String]]
      r.table[JsonObject, String]("abc").orderBy(OrderedIndex(r.asc("code"))).replace(
        JsonObject.empty,
        nonAtomic = NonAtomicUpdate
      ) =%=[ReqlModificationResult[JsonObject, String]] """[55,[[41,[[15,["abc"]]],{"index":[73,["code"]]}],{}],{"non_atomic":true}]"""

      //selectionOfStream
      r.table[JsonObject, String]("abc").skip(10) shouldBe an[ReqlSelectionOfStream[JsonObject, String]]
      r.table[JsonObject, String]("abc").skip(10).replace(
        JsonObject.empty,
        nonAtomic = NonAtomicUpdate
      ) =%=[ReqlModificationResult[JsonObject, String]] """[55,[[70,[[15,["abc"]],10]],{}],{"non_atomic":true}]"""

      //selectionOfArray
      r.table[JsonObject, String]("abc").orderBy(r.asc("code")) shouldBe an[ReqlSelectionOfArray[JsonObject, String]]
      r.table[JsonObject, String]("abc").orderBy(r.asc("code")).replace(
        JsonObject.empty,
        nonAtomic = NonAtomicUpdate
      ) =%=[ReqlModificationResult[JsonObject, String]] """[55,[[41,[[15,["abc"]],[73,["code"]]]],{}],{"non_atomic":true}]"""

      //selectionOfObject
      abcJsonTable.get("uuid") shouldBe an[ReqlSelectionOfObject[JsonObject, String]]
      abcJsonTable.get("uuid").replace(
        JsonObject.empty,
        nonAtomic = NonAtomicUpdate
      ) =%=[ReqlModificationResult[JsonObject, String]] """[55,[[16,[[15,["abc"]],"uuid"]],{}],{"non_atomic":true}]"""
    }

    "insert" in new ShapesData {
      //one document
      r.table[JsonObject, String]("abc").insert(JsonObject.empty) =%=[ReqlModificationResult[JsonObject, String]] """[56,[[15,["abc"]],{}]]"""

      r.table[JsonObject, String]("abc").insert(
        JsonObject.empty,
        Soft,
        AlwaysReturnChanges,
        ReplaceOnConflict()
      ) =%=[ReqlModificationResult[JsonObject, String]] """[56,[[15,["abc"]],{}],{"durability":"soft","return_changes":"always","conflict":"replace"}]"""

      r.table[JsonObject, String]("abc").insert(JsonObject.empty, conflict = ErrorOnConflict()) =%=[ReqlModificationResult[JsonObject, String]] """[56,[[15,["abc"]],{}]]"""
      r.table[JsonObject, String]("abc").insert(JsonObject.empty, conflict = ReplaceOnConflict()) =%=[ReqlModificationResult[JsonObject, String]] """[56,[[15,["abc"]],{}],{"conflict":"replace"}]"""
      r.table[JsonObject, String]("abc").insert(JsonObject.empty, conflict = UpdateOnConflict()) =%=[ReqlModificationResult[JsonObject, String]] """[56,[[15,["abc"]],{}],{"conflict":"update"}]"""
      r.table[JsonObject, String]("abc").insert(
        JsonObject.empty,
        conflict = ResolveOnConflict[JsonObject, String]((id, oldDoc, newDoc) => newDoc)
      ) =%=[ReqlModificationResult[JsonObject, String]] """[56,[[15,["abc"]],{}],{"conflict":[69,[[2,[0,1,2]],[10,[2]]]]}]"""

      {
        import TestDatabase.abc

        val q = abc.table().insert(Abc("abc"))
        val _: ReqlModificationResult[Abc, String] = q
        q =*= """[56,[[15,[[14,["test"]],"abc"]],{"name":"abc"}]]"""
      }

      //many documents
      r.table[JsonObject, String]("abc").insertMany(
        r.expr(Seq(JsonObject.empty, JsonObject.empty)),
        conflict = ResolveOnConflict((id, oldDoc, newDoc) => newDoc)
      ) =%=[ReqlModificationResult[JsonObject, String]] """[56,[[15,["abc"]],[2,[{},{}]]],{"conflict":[69,[[2,[0,1,2]],[10,[2]]]]}]"""

      //auto
      {
        import TestDatabase.abc

        // No TypeTag available for rere.ql.queries.all.InsertTableQuery[qual$558.Model]
        val q = abc.table().insertAuto(Abc("abc"))
        val _: ReqlModificationResult[Abc, String] = q
        q =*= """[56,[[15,[[14,["test"]],"abc"]],{}]]"""
      }

    }

    "db_create" in {
      r.dbCreate("superheroes") =%=[ReqlDatabaseCreationResult] """[57,["superheroes"]]"""

      r.dbCreate(r.expr("super").add("heroes")) =%=[ReqlDatabaseCreationResult] """[57,[[24,["super","heroes"]]]]"""
    }

    "db_drop" in {
      r.dbDrop("superheroes") =%=[ReqlObject] """[58,["superheroes"]]"""

      r.dbDrop(r.expr("super").add("heroes")) =%=[ReqlDatabaseDroppingResult] """[58,[[24,["super","heroes"]]]]"""
    }

    "db_list" in {
      r.dbList() =%=[ReqlArray[ReqlString]] """[59,[]]"""
    }

    "table_create" in {
      //on db
      r.db("test").tableCreate("abc") =%=[ReqlTableCreationResult] """[60,[[14,["test"]],"abc"]]"""

      r.db("test").tableCreate(
        "abc",
        PrimaryKey("name"),
        Soft,
        Shards(3),
        Replicas(1)
      ) =%=[ReqlTableCreationResult] """[60,[[14,["test"]],"abc"],{"primary_key":"name","durability":"soft","shards":3,"replicas":1}]"""

      r.db("test").tableCreate(
        "abc",
        PrimaryKey("code"),
        Hard,
        Shards(7),
        ReplicasByTags(ServerTag("spb"), ServerTag("spb") -> 3, ServerTag("msk") -> 2)
      ) =%=[ReqlTableCreationResult] """[60,[[14,["test"]],"abc"],{"primary_key":"code","durability":"hard","shards":7,"replicas":{"spb":3,"msk":2},"primary_replica_tag":"spb"}]"""

      //on r
      r.tableCreate("abc") =%=[ReqlTableCreationResult] """[60,["abc"]]"""

      r.tableCreate(
        "abc",
        PrimaryKey("code"),
        Hard,
        Shards(7),
        ReplicasByTags(ServerTag("spb"), ServerTag("spb") -> 3, ServerTag("msk") -> 2)
      ) =%=[ReqlTableCreationResult] """[60,["abc"],{"primary_key":"code","durability":"hard","shards":7,"replicas":{"spb":3,"msk":2},"primary_replica_tag":"spb"}]"""
    }

    "table_drop" in {
      r.db("test").tableDrop("abc") =%=[ReqlTableDroppingResult] """[61,[[14,["test"]],"abc"]]"""
      r.db("test").tableDrop(r.expr("abc").add("def")) =%=[ReqlTableDroppingResult] """[61,[[14,["test"]],[24,["abc","def"]]]]"""
    }

    "table_list" in {
      r.db("test").tableList() =%=[ReqlArray[ReqlString]] """[62,[[14,["test"]]]]"""
    }

    "config" in {
      //table
      r.table("abc").config() =%=[ReqlSelectionOfObject[TableConfig, UUID]] """[174,[[15,["abc"]]]]"""

      //db
      r.db("test").config() =%=[ReqlSelectionOfObject[DatabaseConfig, UUID]] """[174,[[14,["test"]]]]"""

      //example from docs
      //TODO: fix it
      /*r.table[ReqlObject]("users").config().update(r.expr(Map(
        "write_acks" -> r.expr("single")
      ))) =%=[ReqlObject] """[53,[[174,[[15,["users"]]]],{"write_acks":"single"}]]"""*/
    }

    "status" in {
      r.table("abc").status() =%=[ReqlSelectionOfObject[TableStatus, UUID]] """[175,[[15,["abc"]]]]"""
    }

    "wait" in {
      //table
      r.table("abc").waitFor(ReadyForOutdatedReads, WithoutTimeout) =%=[ReqlWaitingResult] """[177,[[15,["abc"]]],{"wait_for":"ready_for_outdated_reads"}]"""
      r.table("abc").waitFor(ReadyForOutdatedReads, WithTimeout(2)) =%=[ReqlWaitingResult] """[177,[[15,["abc"]]],{"wait_for":"ready_for_outdated_reads","timeout":2}]"""

      r.table("abc").waitFor(ReadyForReads, WithTimeout(2)) =%=[ReqlWaitingResult] """[177,[[15,["abc"]]],{"wait_for":"ready_for_reads","timeout":2}]"""
      r.table("abc").waitFor(ReadyForWrites, WithTimeout(2)) =%=[ReqlWaitingResult] """[177,[[15,["abc"]]],{"wait_for":"ready_for_writes","timeout":2}]"""
      r.table("abc").waitFor(AllReplicasReady, WithTimeout(2)) =%=[ReqlWaitingResult] """[177,[[15,["abc"]]],{"wait_for":"all_replicas_ready","timeout":2}]"""

      //db
      r.db("test").waitFor(AllReplicasReady, WithTimeout(2)) =%=[ReqlWaitingResult] """[177,[[14,["test"]]],{"wait_for":"all_replicas_ready","timeout":2}]"""
    }

    "reconfigure" in {
      //table
      r.table("abc").reconfigure(Shards(3), Replicas(1).allVoting(), RealRun) =%=[ReqlReconfiguringResult] """[176,[[15,["abc"]]],{"shards":3,"replicas":1}]"""
      r.table("abc").reconfigure(Shards(3), Replicas(1).allVoting(), DryRun) =%=[ReqlReconfiguringDryResult] """[176,[[15,["abc"]]],{"shards":3,"replicas":1,"dry_run":true}]"""

      //TODO: test all queries with tags on real cluster - maybe they not work as i expect
      //ReqlQueryLogicError: `replicas` must be an OBJECT if `nonvoting_replica_tags` is specified in
      r.table("abc").reconfigure(
        Shards(4),
        ReplicasByTags(
          primary = ServerTag("spb"),
          ServerTag("spb") -> 3,
          ServerTag("msk") -> 2
        ).nonvoting(
          ServerTag("msk")
        ),
        DryRun
      ) =%=[ReqlReconfiguringDryResult] """[176,[[15,["abc"]]],{"shards":4,"nonvoting_replica_tags":[2,["msk"]],"replicas":{"spb":3,"msk":2},"primary_replica_tag":"spb","dry_run":true}]""" //order of options not by spec

      r.table("abc").reconfigure(
        Shards(5),
        ReplicasByTags(
          primary = ServerTag("spb"),
          ServerTag("spb") -> 3,
          ServerTag("msk") -> 2
        ).allVoting(),
        RealRun
      ) =%=[ReqlReconfiguringResult] """[176,[[15,["abc"]]],{"shards":5,"replicas":{"spb":3,"msk":2},"primary_replica_tag":"spb"}]"""

      //emergencyRepair on table
      r.table("abc").reconfigure(UnsafeRollback, DryRun) =%=[ReqlReconfiguringDryResult] """[176,[[15,["abc"]]],{"emergency_repair":"unsafe_rollback","dry_run":true}]"""
      r.table("abc").reconfigure(UnsafeRollbackOrErase, RealRun) =%=[ReqlReconfiguringResult] """[176,[[15,["abc"]]],{"emergency_repair":"unsafe_rollback_or_erase"}]"""

      //db
      r.db("test").reconfigure(
        Shards(4),
        ReplicasByTags(
          primary = ServerTag("spb"),
          ServerTag("spb") -> 3,
          ServerTag("msk") -> 2
        ).nonvoting(
          ServerTag("msk")
        ),
        DryRun
      ) =%=[ReqlReconfiguringDryResult] """[176,[[14,["test"]]],{"shards":4,"nonvoting_replica_tags":[2,["msk"]],"replicas":{"spb":3,"msk":2},"primary_replica_tag":"spb","dry_run":true}]""" //order of options not by spec

      r.db("test").reconfigure(
        Shards(4),
        ReplicasByTags(
          primary = ServerTag("spb"),
          ServerTag("spb") -> 3,
          ServerTag("msk") -> 2
        ).nonvoting(
          ServerTag("msk")
        ),
        RealRun
      ) =%=[ReqlReconfiguringResult] """[176,[[14,["test"]]],{"shards":4,"nonvoting_replica_tags":[2,["msk"]],"replicas":{"spb":3,"msk":2},"primary_replica_tag":"spb"}]""" //order of options not by spec
    }

    "rebalance" in {
      //table
      r.table("abc").rebalance() =%=[ReqlRebalancingResult] """[179,[[15,["abc"]]]]"""

      //db
      r.db("test").rebalance() =%=[ReqlRebalancingResult] """[179,[[14,["test"]]]]"""
    }

    "sync" in {
      r.table("abc").sync() =%=[ReqlSyncingResult] """[138,[[15,["abc"]]]]"""
    }

    "grant" in {
      //on r
      r.grant(
        "user",
        UserPermissions(Some(true), Some(false), None, None)
      ) =%=[ReqlGrantingResult] """[188,["user",{"read":true,"write":false}]]"""

      r.grant(
        "user",
        UserPermissions(Some(true), Some(false), Some(true), Some(false))
      ) =%=[ReqlGrantingResult] """[188,["user",{"read":true,"write":false,"connect":true,"config":false}]]"""

      //on db
      r.db("test").grant(
        "user",
        UserPermissions(Some(true), Some(false), None, Some(false))
      ) =%=[ReqlGrantingResult] """[188,[[14,["test"]],"user",{"read":true,"write":false,"config":false}]]"""

      //on table
      r.table("abc").grant(
        "user",
        UserPermissions(Some(true), Some(false), None, Some(false))
      ) =%=[ReqlGrantingResult] """[188,[[15,["abc"]],"user",{"read":true,"write":false,"config":false}]]"""
    }

    "index_create" in {
      r.table[JsonObject, String]("abc").indexCreate("code") =%=[ReqlIndexCreationResult] """[75,[[15,["abc"]],"code"]]"""
      r.table[JsonObject, String]("abc").indexCreate("code", SimpleIndex, RangeIndex) =%=[ReqlIndexCreationResult] """[75,[[15,["abc"]],"code"]]"""
      r.table[JsonObject, String]("abc").indexCreate("code", MultiIndex, GeoIndex) =%=[ReqlIndexCreationResult] """[75,[[15,["abc"]],"code"],{"multi":true,"geo":true}]"""

      r.table[JsonObject, String]("abc").indexCreate("code", _("code_name")) =%=[ReqlIndexCreationResult] """[75,[[15,["abc"]],"code",[69,[[2,[0]],[170,[[10,[0]],"code_name"]]]]]]"""
      r.table[JsonObject, String]("abc").indexCreate("code", _("code_name"), SimpleIndex, RangeIndex) =%=[ReqlIndexCreationResult] """[75,[[15,["abc"]],"code",[69,[[2,[0]],[170,[[10,[0]],"code_name"]]]]]]"""
      r.table[JsonObject, String]("abc").indexCreate("code", _("code_name"), MultiIndex, GeoIndex) =%=[ReqlIndexCreationResult] """[75,[[15,["abc"]],"code",[69,[[2,[0]],[170,[[10,[0]],"code_name"]]]]],{"multi":true,"geo":true}]"""
    }

    "index_drop" in {
      r.table("abc").indexDrop("code") =%=[ReqlIndexDroppingResult] """[76,[[15,["abc"]],"code"]]"""
    }

    "index_list" in {
      r.table("abc").indexList() =%=[ReqlArray[ReqlString]] """[77,[[15,["abc"]]]]"""
    }

    "index_status" in {
      //without arguments - return status of all indices
      r.table("abc").indexStatus() =%=[ReqlArray[ReqlIndexStatusResult]] """[139,[[15,["abc"]]]]"""

      //with arguments - return status of only listed indices
      r.table("abc").indexStatus("code") =%=[ReqlArray[ReqlIndexStatusResult]] """[139,[[15,["abc"]],"code"]]"""
      r.table("abc").indexStatus("code", "code1") =%=[ReqlArray[ReqlIndexStatusResult]] """[139,[[15,["abc"]],"code","code1"]]"""
    }

    "index_wait" in {
      //without arguments - return status of all indices
      r.table("abc").indexWait() =%=[ReqlArray[ReqlIndexStatusResult]] """[140,[[15,["abc"]]]]"""

      //with arguments - return status of only listed indices
      r.table("abc").indexWait("code") =%=[ReqlArray[ReqlIndexStatusResult]] """[140,[[15,["abc"]],"code"]]"""
      r.table("abc").indexWait("code", "code1") =%=[ReqlArray[ReqlIndexStatusResult]] """[140,[[15,["abc"]],"code","code1"]]"""
    }

    "index_rename" in {
      r.table("abc").indexRename("code", "type") =%=[ReqlIndexRenamingResult] """[156,[[15,["abc"]],"code","type"]]"""

      r.table("abc").indexRename("code", "type", NotOverwrite) =%=[ReqlIndexRenamingResult] """[156,[[15,["abc"]],"code","type"]]"""

      r.table("abc").indexRename("code", "type", Overwrite) =%=[ReqlIndexRenamingResult] """[156,[[15,["abc"]],"code","type"],{"overwrite":true}]"""
    }

    "do" in {
      // on r without arguments
      // return null
      r.do_(() => r.expr(null)) =%=[ReqlNull] """[64,[[69,[[2,[]],null]]]]"""

      //return bool
      r.do_(() => r.expr(true)) =%/%=[ReqlBoolean] """[64,[[69,[[2,[]],true]]]]"""

      // return int
      r.do_(() => r.expr(123)) =%=[ReqlInteger] """[64,[[69,[[2,[]],123]]]]"""

      // return float
      r.do_(() => r.expr(BigDecimal.valueOf(2.5d))) =%=[ReqlFloat] """[64,[[69,[[2,[]],2.5]]]]"""

      // return string
      r.do_(() => r.expr("code")) =%=[ReqlString] """[64,[[69,[[2,[]],"code"]]]]"""

      // return array
      r.do_(() => r.expr(Seq(
        r.expr(1), r.expr(2), r.expr(3)
      ))) =%=[ReqlArray[ReqlInteger]] """[64,[[69,[[2,[]],[2,[1,2,3]]]]]]"""

      // return object
      r.do_(() => r.expr(JsonObject.fromMap(Map(
        "key" -> Json.fromString("value")
      )))) =%=[ReqlObject] """[64,[[69,[[2,[]],{"key":"value"}]]]]"""

      // return table
      r.do_(() => abcJsonTable) =%=[ReqlTable[JsonObject, String]] """[64,[[69,[[2,[]],[15,["abc"]]]]]]"""

      // return tableSlice
      r.do_(() => abcJsonTable.orderBy(OrderedIndex(r.asc("code")))) =%=[ReqlTableSlice[JsonObject, String]] """[64,[[69,[[2,[]],[41,[[15,["abc"]]],{"index":[73,["code"]]}]]]]]"""

      // return selection of array
      r.do_(() => abcJsonTable.orderBy(r.asc("code"))) =%=[ReqlSelectionOfArray[JsonObject, String]] """[64,[[69,[[2,[]],[41,[[15,["abc"]],[73,["code"]]]]]]]]"""

      // return selection of stream
      r.do_(() => abcJsonTable.skip(10)) =%=[ReqlSelectionOfStream[JsonObject, String]] """[64,[[69,[[2,[]],[70,[[15,["abc"]],10]]]]]]"""

      // return selection of object
      r.do_(() => jsonTable("abc").get("uuid")) =%=[ReqlSelectionOfObject[JsonObject, String]] """[64,[[69,[[2,[]],[16,[[15,["abc"]],"uuid"]]]]]]"""

      // return finite stream
      r.do_(() => r.range(4)) =%=[ReqlFiniteStream[ReqlInteger]] """[64,[[69,[[2,[]],[173,[4]]]]]]"""

      // return infinite stream
      r.do_(() => r.range()) =%=[ReqlInfiniteStream[ReqlInteger]] """[64,[[69,[[2,[]],[173,[]]]]]]"""

      //TODO: make tests for pseudo types, geo types, db

      // chaining test
      r.do_ {() =>
        r.do_ {() =>
          r.range(4)
        }
      } =%=[ReqlFiniteStream[ReqlInteger]] """[64,[[69,[[2,[]],[64,[[69,[[2,[]],[173,[4]]]]]]]]]]"""


      // on query without additional arguments
      r.expr(234).do_(show => r.expr(123)) =%=[ReqlInteger] """[64,[[69,[[2,[0]],123]],234]]"""

      r.expr(234).do_(_.ge(123)) =%/%=[ReqlBoolean] """[64,[[69,[[2,[0]],[22,[[10,[0]],123]]]],234]]"""


      jsonTable("tv_shows").get("99c03c40-162c-49a4-a041-1a4b84edfccc").do_ {
        show => show("code").asInteger.add(1).asInteger
      } =%=[ReqlInteger] """[64,[[69,[[2,[0]],[24,[[170,[[10,[0]],"code"]],1]]]],[16,[[15,["tv_shows"]],"99c03c40-162c-49a4-a041-1a4b84edfccc"]]]]"""

      //TODO: cases that should not compile (different args, r.do(arg => arg.(...)))
    }

    "branch" in {
      // on r
      r.branch(r.expr(123).gt(5), r.expr("big"), r.expr("small")) =%=[ReqlString] """[65,[[21,[123,5]],"big","small"]]"""

      r.branch {
        if_(r.expr(123).gt(5)) _then_ r.expr("big") _else_ r.expr("small")
      } =%=[ReqlString] """[65,[[21,[123,5]],"big","small"]]"""

      r.branch {
        if_(r.expr(123).gt(200))_then_{
          r.expr("huge")
        }_else_if_(r.expr(123).gt(100))_then_{
          r.expr("big")
        }_else_{
          r.expr("small")
        }
      } =%=[ReqlString] """[65,[[21,[123,200]],"huge",[21,[123,100]],"big","small"]]"""

      // on bool
      r.expr(123).gt(5).branch(r.expr("big"), r.expr("small")) =%=[ReqlString] """[65,[[21,[123,5]],"big","small"]]"""

      r.expr(123).gt(5).branch {
        then_{
          r.expr("big")
        }_else_{
          r.expr("small")
        }
      } =%=[ReqlString] """[65,[[21,[123,5]],"big","small"]]"""

      r.expr(123).gt(200).branch {
        then_{
          r.expr("huge")
        }_else_if_(r.expr(123).gt(100))_then_{
          r.expr("big")
        }_else_{
          r.expr("small")
        }
      } =%=[ReqlString] """[65,[[21,[123,200]],"huge",[21,[123,100]],"big","small"]]"""
    }

    "or" in {
      // on r
      r.or() =%=[ReqlBoolean] """[66,[]]"""

      r.or(r.expr(true)) =%=[ReqlBoolean] """[66,[true]]"""

      r.or(r.expr(true), r.expr(123).gt(5)) =%=[ReqlBoolean] """[66,[true,[21,[123,5]]]]"""

      r.or(r.expr(true), r.expr(123).gt(5), r.expr(false)) =%=[ReqlBoolean] """[66,[true,[21,[123,5]],false]]"""

      // on bool
      r.expr(true).or() =%=[ReqlBoolean] """[66,[true]]"""

      r.expr(true).or(r.expr(123).gt(5)) =%=[ReqlBoolean] """[66,[true,[21,[123,5]]]]"""

      r.expr(true).or(r.expr(123).gt(5), r.expr(false)) =%=[ReqlBoolean] """[66,[true,[21,[123,5]],false]]"""
    }

    "and" in {
      // on r
      r.and() =%=[ReqlBoolean] """[67,[]]"""

      r.and(r.expr(true)) =%=[ReqlBoolean] """[67,[true]]"""

      r.and(r.expr(true), r.expr(123).gt(5)) =%=[ReqlBoolean] """[67,[true,[21,[123,5]]]]"""

      r.and(r.expr(true), r.expr(123).gt(5), r.expr(false)) =%=[ReqlBoolean] """[67,[true,[21,[123,5]],false]]"""

      // on bool
      r.expr(true).and() =%=[ReqlBoolean] """[67,[true]]"""

      r.expr(true).and(r.expr(123).gt(5)) =%=[ReqlBoolean] """[67,[true,[21,[123,5]]]]"""

      r.expr(true).and(r.expr(123).gt(5), r.expr(false)) =%=[ReqlBoolean] """[67,[true,[21,[123,5]],false]]"""
    }

    "for_each" in {
      //table
      abcJsonTable.forEach(
        x => bcdJsonTable.get("uuid").delete()
      ) =%=[ReqlModificationResult[JsonObject, String]] """[68,[[15,["abc"]],[69,[[2,[0]],[54,[[16,[[15,["bcd"]],"uuid"]]]]]]]]"""

      //tableSlice
      abcJsonTable.orderBy(OrderedIndex(r.asc("code"))).forEach(
        x => bcdJsonTable.get("uuid").delete()
      ) =%=[ReqlModificationResult[JsonObject, String]] """[68,[[41,[[15,["abc"]]],{"index":[73,["code"]]}],[69,[[2,[0]],[54,[[16,[[15,["bcd"]],"uuid"]]]]]]]]"""

      //selectionOfArray
      abcJsonTable.orderBy(r.asc("code")).forEach(
        x => bcdJsonTable.get("uuid").delete()
      ) =%=[ReqlModificationResult[JsonObject, String]] """[68,[[41,[[15,["abc"]],[73,["code"]]]],[69,[[2,[0]],[54,[[16,[[15,["bcd"]],"uuid"]]]]]]]]"""

      //selectionOfStream
      abcJsonTable.skip(1).forEach(
        x => bcdJsonTable.get("uuid").delete()
      ) =%=[ReqlModificationResult[JsonObject, String]] """[68,[[70,[[15,["abc"]],1]],[69,[[2,[0]],[54,[[16,[[15,["bcd"]],"uuid"]]]]]]]]"""

      //finite stream
      r.range(4).forEach(
        x => bcdJsonTable.get("uuid").delete()
      ) =%=[ReqlModificationResult[JsonObject, String]] """[68,[[173,[4]],[69,[[2,[0]],[54,[[16,[[15,["bcd"]],"uuid"]]]]]]]]"""

      //infinite stream
      //Cannot use an infinite stream with an aggregation function (`reduce`, `count`, etc.)
      """r.range().forEach(x => r.table("bcd").get("uuid").delete())""".shouldNot(compile)

      //array
      r.expr(Seq(r.expr(123), r.expr(234))).forEach(
        x => bcdJsonTable.get("uuid").delete()
      ) =%=[ReqlModificationResult[JsonObject, String]] """[68,[[2,[123,234]],[69,[[2,[0]],[54,[[16,[[15,["bcd"]],"uuid"]]]]]]]]"""
    }

    "func" in {
      //FunctionWrapOld.wrapToFunction0(() => r.expr("test")) =*= """[69,[[2,[]],"test"]]"""
      //FunctionWrapOld.wrapToFunction1(_.asNumber.add(5)) =*= """[69,[[2,[0]],[24,[[10,[0]],5]]]]"""
      //TODO: cases for functions with many parameters

      Func.wrap0(() => r.expr("test")) =*= """[69,[[2,[]],"test"]]"""
      Func.wrap1[ReqlInteger, ReqlInteger](_.add(5)) =*= """[69,[[2,[0]],[24,[[10,[0]],5]]]]"""
    }

    "asc" in {
      r.asc("field") =*= """[73,["field"]]"""

      r.asc(_.asNumber.add(5)) =*= """[73,[[69,[[2,[0]],[24,[[10,[0]],5]]]]]]"""

      """r.asc(r.asc("code"))""".shouldNot(compile)
    }

    "desc" in {
      r.desc("field") =*= """[74,["field"]]"""

      r.desc(_.asNumber.add(5)) =*= """[74,[[69,[[2,[0]],[24,[[10,[0]],5]]]]]]"""

      """r.desc(r.desc("code"))""".shouldNot(compile)
    }

    "info" in {
      //db
      r.db("test").info() =%=[ReqlObject] """[79,[[14,["test"]]]]"""

      //table
      r.table("abc").info() =%=[ReqlObject] """[79,[[15,["abc"]]]]"""

      //on r
      //r.info(r.db("test")) =%=[ReqlObject] """[79,[[14,["test"]]]]"""
      //r.info(r.table("abc")) =%=[ReqlObject] """[79,[[15,["abc"]]]]"""
    }

    "match" in {
      r.expr("ABC").match_("^A") =%=[ReqlObject] """[97,["ABC","^A"]]"""
    }

    "upcase" in {
      r.expr("Sentence about LaTeX.").upcase() =%=[ReqlString] """[141,["Sentence about LaTeX."]]"""
    }

    "downcase" in {
      r.expr("Sentence about LaTeX.").downcase() =%=[ReqlString] """[142,["Sentence about LaTeX."]]"""
    }

    "sample" in {
      //table
      abcJsonTable.sample(r.expr(3)) =%=[ReqlSelectionOfArray[JsonObject, String]] """[81,[[15,["abc"]],3]]"""

      //tableSlice
      abcJsonTable.orderBy(OrderedIndex(r.asc("code"))).
        sample(r.expr(3)) =%=[ReqlSelectionOfArray[JsonObject, String]] """[81,[[41,[[15,["abc"]]],{"index":[73,["code"]]}],3]]"""

      //selectionOfArray
      abcJsonTable.orderBy(r.asc("code")).
        sample(r.expr(3)) =%=[ReqlSelectionOfArray[JsonObject, String]] """[81,[[41,[[15,["abc"]],[73,["code"]]]],3]]"""

      //selectionOfStream
      abcJsonTable.skip(1).sample(3) =%=[ReqlSelectionOfArray[JsonObject, String]] """[81,[[70,[[15,["abc"]],1]],3]]"""

      //finite stream
      r.range(4).sample(3) =%=[ReqlArray[ReqlInteger]] """[81,[[173,[4]],3]]"""

      //infinite stream
      //Cannot use an infinite stream with an aggregation function (`reduce`, `count`, etc.)
      """r.range().sample(3)""".shouldNot(compile)

      //array
      r.expr(Seq(r.expr(123), r.expr(234), r.expr(345), r.expr(456))).
        sample(3) =%=[ReqlArray[ReqlInteger]] """[81,[[2,[123,234,345,456]],3]]"""
    }

    "default" in {
      //null
      """r.expr(null).default(r.expr("test"))""".shouldNot(compile)
      r.expr(null).asString.default(r.expr("test")) =%=[ReqlString] """[92,[null,"test"]]"""

      //object
      """r.expr(Map.empty[String, ReqlDatum]).default(r.expr("test"))""".shouldNot(compile)
      r.expr(Map.empty[String, ReqlDatum]).default(r.expr(Map(
        "key" -> r.expr("value")
      ))) =%=[ReqlObject] """[92,[{},{"key":"value"}]]"""

      //null with defaultFunction
      r.expr(null).asObject.
        default(_ => r.expr(Map("key" -> r.expr("value")))) =%=[ReqlObject] """[92,[null,[69,[[2,[0]],{"key":"value"}]]]]"""
    }

    "json" in {
      r.json("""{"key": 123}""") =%=[ReqlJson] """[98,["{\"key\": 123}"]]"""
      r.json("""{"key": 123}""").asObject =%=[ReqlObject] """[98,["{\"key\": 123}"]]"""
    }

    "to_json_string" in {
      //null
      r.expr(null).toJsonString() =%=[ReqlString] """[172,[null]]"""
      r.expr(null).toJSON() =%=[ReqlString] """[172,[null]]"""

      //bool
      r.expr(true).toJsonString() =%=[ReqlString] """[172,[true]]"""
      r.expr(true).toJSON() =%=[ReqlString] """[172,[true]]"""

      //integer
      r.expr(123).toJsonString() =%=[ReqlString] """[172,[123]]"""
      r.expr(123).toJSON() =%=[ReqlString] """[172,[123]]"""

      //float
      r.expr(BigDecimal(123.45)).toJsonString() =%=[ReqlString] """[172,[123.45]]"""
      r.expr(BigDecimal(123.45)).toJSON() =%=[ReqlString] """[172,[123.45]]"""

      //string
      r.expr("test").toJsonString() =%=[ReqlString] """[172,["test"]]"""
      r.expr("test").toJSON() =%=[ReqlString] """[172,["test"]]"""

      //array
      r.expr(Seq(r.expr(123), r.expr(234))).toJsonString() =%=[ReqlString] """[172,[[2,[123,234]]]]"""
      r.expr(Seq(r.expr(123), r.expr(234))).toJSON() =%=[ReqlString] """[172,[[2,[123,234]]]]"""

      //object
      r.expr(JsonObject.empty).toJsonString() =%=[ReqlString] """[172,[{}]]"""
      r.expr(JsonObject.empty).toJSON() =%=[ReqlString] """[172,[{}]]"""

      //selectionOfObject
      abcJsonTable.get("uuid").toJsonString() =%=[ReqlString] """[172,[[16,[[15,["abc"]],"uuid"]]]]"""
      abcJsonTable.get("uuid").toJSON() =%=[ReqlString] """[172,[[16,[[15,["abc"]],"uuid"]]]]"""

      //binary
      r.binary(ByteString(10, 20, 30, 40)).toJsonString() =%=[ReqlString] """[172,[{"$reql_type$":"BINARY","data":"ChQeKA=="}]]"""
      r.binary(ByteString(10, 20, 30, 40)).toJSON() =%=[ReqlString] """[172,[{"$reql_type$":"BINARY","data":"ChQeKA=="}]]"""

      //time
      r.now().toJsonString() =%=[ReqlString] """[172,[[103,[]]]]"""
      r.now().toJSON() =%=[ReqlString] """[172,[[103,[]]]]"""

      //geometry
      //TODO: implement later
    }

    "iso8601" in {
      r.ISO8601("1986-11-03T08:30:00-07:00") =%=[ReqlTime] """[99,["1986-11-03T08:30:00-07:00"]]"""

      r.ISO8601("1986-11-03T08:30:00-07:00", WithoutTimezone) =%=[ReqlTime] """[99,["1986-11-03T08:30:00-07:00"]]"""

      r.ISO8601("1986-11-03T08:30:00", DefaultTimezone("+07:00")) =%=[ReqlTime] """[99,["1986-11-03T08:30:00"],{"default_timezone":"+07:00"}]"""
    }

    "to_iso8601" in {
      r.now().toISO8601() =%=[ReqlString] """[100,[[103,[]]]]"""
    }

    "epoch_time" in {
      r.epochTime(531360000) =%=[ReqlTime] """[101,[531360000]]"""

      r.epochTime(BigDecimal(531360000.123)) =%=[ReqlTime] """[101,[531360000.123]]"""
    }

    "to_epoch_time" in {
      r.now().toEpochTime() =%=[ReqlFloat] """[102,[[103,[]]]]"""
    }

    "now" in {
      r.now() =*= "[103,[]]"
    }

    "in_timezone" in {
      r.now().inTimezone("-08:00") =%=[ReqlTime] """[104,[[103,[]],"-08:00"]]"""
    }

    "during" in {
      r.now().during(r.epochTime(531360000), r.epochTime(531370000)) =%=[ReqlBoolean] """[105,[[103,[]],[101,[531360000]],[101,[531370000]]]]"""

      r.now().during(r.epochTime(531360000), r.epochTime(531370000), DefaultBounds) =%=[ReqlBoolean] """[105,[[103,[]],[101,[531360000]],[101,[531370000]]]]"""

      r.now().during(r.epochTime(531360000), r.epochTime(531370000), Bounds(OpenBound, OpenBound)) =%=[ReqlBoolean] """[105,[[103,[]],[101,[531360000]],[101,[531370000]]],{"left_bound":"open","right_bound":"open"}]"""

      r.now().during(r.epochTime(531360000), r.epochTime(531370000), Bounds(ClosedBound, ClosedBound)) =%=[ReqlBoolean] """[105,[[103,[]],[101,[531360000]],[101,[531370000]]],{"left_bound":"closed","right_bound":"closed"}]"""
    }

    "date" in {
      r.now().date() =%=[ReqlTime] """[106,[[103,[]]]]"""
    }

    "time_of_day" in {
      r.now().timeOfDay() =%=[ReqlFloat] """[126,[[103,[]]]]"""
    }

    "timezone" in {
      r.now().timezone() =%=[ReqlString] """[127,[[103,[]]]]"""
    }

    "year" in {
      r.now().year() =%=[ReqlInteger] """[128,[[103,[]]]]"""
    }

    "month" in {
      r.now().month() =%=[ReqlInteger] """[129,[[103,[]]]]"""
    }

    "day" in {
      r.now().day() =%=[ReqlInteger] """[130,[[103,[]]]]"""
    }

    "day_of_week" in {
      r.now().dayOfWeek() =%=[ReqlInteger] """[131,[[103,[]]]]"""
    }

    "day_of_year" in {
      r.now().dayOfYear() =%=[ReqlInteger] """[132,[[103,[]]]]"""
    }

    "hours" in {
      r.now().hours() =%=[ReqlInteger] """[133,[[103,[]]]]"""
    }

    "minutes" in {
      r.now().minutes() =%=[ReqlInteger] """[134,[[103,[]]]]"""
    }

    "seconds" in {
      r.now().seconds() =%=[ReqlFloat] """[135,[[103,[]]]]"""
    }

    "time" in {
      r.time(2015, 2, 3, "Z") =%=[ReqlTime] """[136,[2015,2,3,"Z"]]"""

      r.time(2015, 2, 3, 4, 5, BigDecimal(6.789), "Z") =%=[ReqlTime] """[136,[2015,2,3,4,5,6.789,"Z"]]"""
    }

    "monday" in {
      r.monday =%=[ReqlInteger] """[107,[]]"""
    }

    "tuesday" in {
      r.tuesday =%=[ReqlInteger] """[108,[]]"""
    }

    "wednesday" in {
      r.wednesday =%=[ReqlInteger] """[109,[]]"""
    }

    "thursday" in {
      r.thursday =%=[ReqlInteger] """[110,[]]"""
    }

    "friday" in {
      r.friday =%=[ReqlInteger] """[111,[]]"""
    }

    "saturday" in {
      r.saturday =%=[ReqlInteger] """[112,[]]"""
    }

    "sunday" in {
      r.sunday =%=[ReqlInteger] """[113,[]]"""
    }

    "january" in {
      r.january =%=[ReqlInteger] """[114,[]]"""
    }

    "february" in {
      r.february =%=[ReqlInteger] """[115,[]]"""
    }

    "march" in {
      r.march =%=[ReqlInteger] """[116,[]]"""
    }

    "april" in {
      r.april =%=[ReqlInteger] """[117,[]]"""
    }

    "may" in {
      r.may =%=[ReqlInteger] """[118,[]]"""
    }

    "june" in {
      r.june =%=[ReqlInteger] """[119,[]]"""
    }

    "july" in {
      r.july =%=[ReqlInteger] """[120,[]]"""
    }

    "august" in {
      r.august =%=[ReqlInteger] """[121,[]]"""
    }

    "september" in {
      r.september =%=[ReqlInteger] """[122,[]]"""
    }

    "october" in {
      r.october =%=[ReqlInteger] """[123,[]]"""
    }

    "november" in {
      r.november =%=[ReqlInteger] """[124,[]]"""
    }

    "december" in {
      r.december =%=[ReqlInteger] """[125,[]]"""
    }

    "literal" in {
      r.literal(JsonObject.empty) =%=[ReqlLiteral] """[137,[{}]]"""
      r.literal(JsonObject.empty) =%=[ReqlValue] """[137,[{}]]"""
      r.literal(JsonObject.empty) =%=[ReqlSpecific] """[137,[{}]]"""

      r.literal() =%=[ReqlLiteral] """[137,[]]"""
      r.literal() =%=[ReqlValue] """[137,[]]"""
      r.literal() =%=[ReqlSpecific] """[137,[]]"""

      abcJsonTable.get("uuid").update(Map(
        "data" -> r.literal(Map(
          "age" -> r.expr(19),
          "job" -> r.expr("Engineer")
        ))
      )) =%=[ReqlObject] """[53,[[16,[[15,["abc"]],"uuid"]],{"data":[137,[{"age":19,"job":"Engineer"}]]}]]"""

      abcJsonTable.get("uuid").merge(Map(
        "data" -> r.literal()
      )) =%=[ReqlObject] """[35,[[16,[[15,["abc"]],"uuid"]],{"data":[137,[]]}]]"""
    }

    "group" should {
      "table" in {
        //one field
        abcJsonTable
          .group(
            r.sel[ReqlModel[JsonObject, String], ReqlDatum]("code")
          ) =%=[ReqlGroupedStream[ReqlDatum, ReqlModel[JsonObject, String]]] """[144,[[15,["abc"]],"code"]]"""

        //many fields
        abcJsonTable
          .group(
            r.sel[ReqlModel[JsonObject, String], ReqlDatum]("code"),
            r.sel[ReqlModel[JsonObject, String], ReqlDatum]("type")
          ) =%=[ReqlGroupedStream[ReqlArray[ReqlDatum], ReqlModel[JsonObject, String]]] """[144,[[15,["abc"]],"code","type"]]"""

        //function
        abcJsonTable.group(
          { obj: ReqlModel[JsonObject, String] => obj.pluck("code", "type") }
        ) =%=[ReqlGroupedStream[PluckObjectQuery, ReqlModel[JsonObject, String]]] """[144,[[15,["abc"]],[69,[[2,[0]],[33,[[10,[0]],"code","type"]]]]]]"""

        //mixed
        abcJsonTable.group(
          r.sel[ReqlModel[JsonObject, String], ReqlDatum]("code"),
          { obj: ReqlModel[JsonObject, String] => obj.pluck("group", "type") }
        ) =%=[ReqlGroupedStream[ReqlArray[ReqlDatum], ReqlModel[JsonObject, String]]] """[144,[[15,["abc"]],"code",[69,[[2,[0]],[33,[[10,[0]],"group","type"]]]]]]"""

        //index
        //TODO: index with types
        abcJsonTable.group(Index("code")) =%=[ReqlGroupedStream[Nothing, ReqlModel[JsonObject, String]]] """[144,[[15,["abc"]]],{"index":"code"}]"""

        r.table("abc").group(
          r.sel[ReqlModel[JsonObject, String], ReqlTime]("day"),
          { obj: ReqlModel[JsonObject, String] => obj.pluck("group", "type") },
          Index("code")
        ) =%=[ReqlGroupedStream[ReqlArray[ReqlDatum], ReqlModel[JsonObject, String]]] """[144,[[15,["abc"]],"day",[69,[[2,[0]],[33,[[10,[0]],"group","type"]]]]],{"index":"code"}]"""

        //multi
        r.table("abc").group(
          r.sel[ReqlModel[JsonObject, String], ReqlTime]("day"),
          { obj: ReqlModel[JsonObject, String] => obj.pluck("group", "type") },
          MultiGroup
        ) =%=[ReqlGroupedStream[ReqlArray[ReqlDatum], ReqlModel[JsonObject, String]]] """[144,[[15,["abc"]],"day",[69,[[2,[0]],[33,[[10,[0]],"group","type"]]]]],{"multi":true}]"""

        r.table("abc").group(
          r.sel[ReqlModel[JsonObject, String], ReqlTime]("day"),
          { obj: ReqlModel[JsonObject, String] => obj.pluck("group", "type") },
          Index("code"),
          MultiGroup
        ) =%=[ReqlGroupedStream[ReqlArray[ReqlDatum], ReqlModel[JsonObject, String]]] """[144,[[15,["abc"]],"day",[69,[[2,[0]],[33,[[10,[0]],"group","type"]]]]],{"index":"code","multi":true}]"""
      }
    }

    "sum" in {
      //table
      """abcJsonTable.sum()""".shouldNot(compile)
      abcJsonTable.sum("code") =%=[ReqlFloat] """[145,[[15,["abc"]],"code"]]"""
      abcJsonTable.sum(_("code").asNumber.add(7)) =%=[ReqlFloat] """[145,[[15,["abc"]],[69,[[2,[0]],[24,[[170,[[10,[0]],"code"]],7]]]]]]"""

      //tableSlice
      """abcJsonTable.orderBy(OrderedIndex(r.asc("code"))).sum()""".shouldNot(compile)
      abcJsonTable.orderBy(OrderedIndex(r.asc("code"))).sum("code") =%=[ReqlFloat] """[145,[[41,[[15,["abc"]]],{"index":[73,["code"]]}],"code"]]"""
      abcJsonTable.orderBy(OrderedIndex(r.asc("code"))).sum(_("code").asNumber.add(7)) =%=[ReqlFloat] """[145,[[41,[[15,["abc"]]],{"index":[73,["code"]]}],[69,[[2,[0]],[24,[[170,[[10,[0]],"code"]],7]]]]]]"""

      //selectionOfArray
      abcJsonTable.orderBy(r.asc("code")) shouldBe an[ReqlSelectionOfArray[JsonObject, String]]
      """abcJsonTable.orderBy(r.asc("code")).sum()""".shouldNot(compile)
      abcJsonTable.orderBy(r.asc("code")).sum("code") =%=[ReqlFloat] """[145,[[41,[[15,["abc"]],[73,["code"]]]],"code"]]"""
      abcJsonTable.orderBy(r.asc("code")).sum(_("code").asNumber.add(7)) =%=[ReqlFloat] """[145,[[41,[[15,["abc"]],[73,["code"]]]],[69,[[2,[0]],[24,[[170,[[10,[0]],"code"]],7]]]]]]"""

      //selectionOfStream
      abcJsonTable.skip(1) shouldBe an[ReqlSelectionOfStream[ReqlObject, UUID]]
      """abcJsonTable.skip(1).sum()""".shouldNot(compile)
      abcJsonTable.skip(1).sum("code") =%=[ReqlFloat] """[145,[[70,[[15,["abc"]],1]],"code"]]"""
      abcJsonTable.skip(1).sum(_("code").asNumber.add(7)) =%=[ReqlFloat] """[145,[[70,[[15,["abc"]],1]],[69,[[2,[0]],[24,[[170,[[10,[0]],"code"]],7]]]]]]"""

      //finiteStream
      r.range(4).sum() =%=[ReqlFloat] """[145,[[173,[4]]]]"""
      r.range(4).sum("code") =%=[ReqlFloat] """[145,[[173,[4]],"code"]]"""
      r.range(4).sum(_.asNumber.add(7)) =%=[ReqlFloat] """[145,[[173,[4]],[69,[[2,[0]],[24,[[10,[0]],7]]]]]]"""

      //infiniteStream
      """r.range().sum()""".shouldNot(compile)

      //array
      r.expr(Seq(r.expr(1), r.expr(2), r.expr(3))).
        sum() =%=[ReqlFloat] """[145,[[2,[1,2,3]]]]"""

      r.expr(Seq(r.expr(1), r.expr(2), r.expr(3))).
        sum("code") =%=[ReqlFloat] """[145,[[2,[1,2,3]],"code"]]"""

      r.expr(Seq(r.expr(1), r.expr(2), r.expr(3))).
        sum(_.asNumber.add(7)) =%=[ReqlFloat] """[145,[[2,[1,2,3]],[69,[[2,[0]],[24,[[10,[0]],7]]]]]]"""
    }

    "avg" in {
      //table
      """abcJsonTable.avg()""".shouldNot(compile)
      abcJsonTable.avg("code") =%=[ReqlFloat] """[146,[[15,["abc"]],"code"]]"""
      abcJsonTable.avg(_("code").asNumber.add(7)) =%=[ReqlFloat] """[146,[[15,["abc"]],[69,[[2,[0]],[24,[[170,[[10,[0]],"code"]],7]]]]]]"""

      //tableSlice
      """abcJsonTable.orderBy(OrderedIndex(r.asc("code"))).avg()""".shouldNot(compile)
      abcJsonTable.orderBy(OrderedIndex(r.asc("code"))).avg("code") =%=[ReqlFloat] """[146,[[41,[[15,["abc"]]],{"index":[73,["code"]]}],"code"]]"""
      abcJsonTable.orderBy(OrderedIndex(r.asc("code"))).avg(_("code").asNumber.add(7)) =%=[ReqlFloat] """[146,[[41,[[15,["abc"]]],{"index":[73,["code"]]}],[69,[[2,[0]],[24,[[170,[[10,[0]],"code"]],7]]]]]]"""

      //selectionOfArray
      abcJsonTable.orderBy(r.asc("code")) shouldBe an[ReqlSelectionOfArray[JsonObject, String]]
      """abcJsonTable.orderBy(r.asc("code")).avg()""".shouldNot(compile)
      abcJsonTable.orderBy(r.asc("code")).avg("code") =%=[ReqlFloat] """[146,[[41,[[15,["abc"]],[73,["code"]]]],"code"]]"""
      abcJsonTable.orderBy(r.asc("code")).avg(_("code").asNumber.add(7)) =%=[ReqlFloat] """[146,[[41,[[15,["abc"]],[73,["code"]]]],[69,[[2,[0]],[24,[[170,[[10,[0]],"code"]],7]]]]]]"""

      //selectionOfStream
      abcJsonTable.skip(1) shouldBe an[ReqlSelectionOfStream[JsonObject, String]]
      """abcJsonTable.skip(1).avg()""".shouldNot(compile)
      abcJsonTable.skip(1).avg("code") =%=[ReqlFloat] """[146,[[70,[[15,["abc"]],1]],"code"]]"""
      abcJsonTable.skip(1).avg(_("code").asNumber.add(7)) =%=[ReqlFloat] """[146,[[70,[[15,["abc"]],1]],[69,[[2,[0]],[24,[[170,[[10,[0]],"code"]],7]]]]]]"""

      //finiteStream
      r.range(4).avg() =%=[ReqlFloat] """[146,[[173,[4]]]]"""
      r.range(4).avg("code") =%=[ReqlFloat] """[146,[[173,[4]],"code"]]"""
      r.range(4).avg(_.asNumber.add(7)) =%=[ReqlFloat] """[146,[[173,[4]],[69,[[2,[0]],[24,[[10,[0]],7]]]]]]"""

      //infiniteStream
      """r.range().avg()""".shouldNot(compile)

      //array
      r.expr(Seq(r.expr(1), r.expr(2), r.expr(3))).
        avg() =%=[ReqlFloat] """[146,[[2,[1,2,3]]]]"""

      r.expr(Seq(r.expr(1), r.expr(2), r.expr(3))).
        avg("code") =%=[ReqlFloat] """[146,[[2,[1,2,3]],"code"]]"""

      r.expr(Seq(r.expr(1), r.expr(2), r.expr(3))).
        avg(_.asNumber.add(7)) =%=[ReqlFloat] """[146,[[2,[1,2,3]],[69,[[2,[0]],[24,[[10,[0]],7]]]]]]"""
    }

    "min" in {
      //table
      abcJsonTable.min() =%=[ReqlModel[JsonObject, String]] """[147,[[15,["abc"]]]]"""
      abcJsonTable.min(Index("code")) =%=[ReqlModel[JsonObject, String]] """[147,[[15,["abc"]]],{"index":"code"}]"""
      abcJsonTable.min("code") =%=[ReqlModel[JsonObject, String]] """[147,[[15,["abc"]],"code"]]"""
      abcJsonTable.min(_("code").asNumber.add(7)) =%=[ReqlModel[JsonObject, String]] """[147,[[15,["abc"]],[69,[[2,[0]],[24,[[170,[[10,[0]],"code"]],7]]]]]]"""
      abcJsonTable.min(_("code").asBinary) =%=[ReqlModel[JsonObject, String]] """[147,[[15,["abc"]],[69,[[2,[0]],[170,[[10,[0]],"code"]]]]]]"""

      //tableSlice
      abcJsonTable.orderBy(OrderedIndex(r.asc("code"))).min() =%=[ReqlModel[JsonObject, String]] """[147,[[41,[[15,["abc"]]],{"index":[73,["code"]]}]]]"""
      abcJsonTable.orderBy(OrderedIndex(r.asc("code"))).min("code") =%=[ReqlModel[JsonObject, String]] """[147,[[41,[[15,["abc"]]],{"index":[73,["code"]]}],"code"]]"""
      abcJsonTable.orderBy(OrderedIndex(r.asc("code"))).min(_("code").asNumber.add(7)) =%=[ReqlModel[JsonObject, String]] """[147,[[41,[[15,["abc"]]],{"index":[73,["code"]]}],[69,[[2,[0]],[24,[[170,[[10,[0]],"code"]],7]]]]]]"""
      abcJsonTable.orderBy(OrderedIndex(r.asc("code"))).min(_("code").asBinary) =%=[ReqlModel[JsonObject, String]] """[147,[[41,[[15,["abc"]]],{"index":[73,["code"]]}],[69,[[2,[0]],[170,[[10,[0]],"code"]]]]]]"""

      //selectionOfArray
      abcJsonTable.orderBy(r.asc("code")) shouldBe an[ReqlSelectionOfArray[JsonObject, String]]
      abcJsonTable.orderBy(r.asc("code")).min() =%=[ReqlModel[JsonObject, String]] """[147,[[41,[[15,["abc"]],[73,["code"]]]]]]"""
      abcJsonTable.orderBy(r.asc("code")).min("code") =%=[ReqlModel[JsonObject, String]] """[147,[[41,[[15,["abc"]],[73,["code"]]]],"code"]]"""
      abcJsonTable.orderBy(r.asc("code")).min(_("code").asNumber.add(7)) =%=[ReqlModel[JsonObject, String]] """[147,[[41,[[15,["abc"]],[73,["code"]]]],[69,[[2,[0]],[24,[[170,[[10,[0]],"code"]],7]]]]]]"""
      abcJsonTable.orderBy(r.asc("code")).min(_("code").asBinary) =%=[ReqlModel[JsonObject, String]] """[147,[[41,[[15,["abc"]],[73,["code"]]]],[69,[[2,[0]],[170,[[10,[0]],"code"]]]]]]"""

      //selectionOfStream
      abcJsonTable.skip(1) shouldBe an[ReqlSelectionOfStream[JsonObject, String]]
      abcJsonTable.skip(1).min() =%=[ReqlModel[JsonObject, String]] """[147,[[70,[[15,["abc"]],1]]]]"""
      abcJsonTable.skip(1).min("code") =%=[ReqlModel[JsonObject, String]] """[147,[[70,[[15,["abc"]],1]],"code"]]"""
      abcJsonTable.skip(1).min(_("code").asNumber.add(7)) =%=[ReqlModel[JsonObject, String]] """[147,[[70,[[15,["abc"]],1]],[69,[[2,[0]],[24,[[170,[[10,[0]],"code"]],7]]]]]]"""
      abcJsonTable.skip(1).min(_("code").asBinary) =%=[ReqlModel[JsonObject, String]] """[147,[[70,[[15,["abc"]],1]],[69,[[2,[0]],[170,[[10,[0]],"code"]]]]]]"""

      //finiteStream
      r.range(4).min() =%=[ReqlInteger] """[147,[[173,[4]]]]"""
      //TODO: it should not work
      r.range(4).min("code") =%=[ReqlInteger] """[147,[[173,[4]],"code"]]"""
      r.range(4).min(_.asNumber.add(7)) =%=[ReqlInteger] """[147,[[173,[4]],[69,[[2,[0]],[24,[[10,[0]],7]]]]]]"""

      //infiniteStream
      """r.range().min()""".shouldNot(compile)

      //array
      r.expr(Seq(r.expr(1), r.expr(2), r.expr(3))).
        min() =%=[ReqlInteger] """[147,[[2,[1,2,3]]]]"""

      r.expr(Seq(r.expr(1), r.expr(2), r.expr(3))).
        min("code") =%=[ReqlInteger] """[147,[[2,[1,2,3]],"code"]]"""

      r.expr(Seq(r.expr(1), r.expr(2), r.expr(3))).
        min(_.asNumber.add(7)) =%=[ReqlInteger] """[147,[[2,[1,2,3]],[69,[[2,[0]],[24,[[10,[0]],7]]]]]]"""
    }

    "max" in {
      //table
      abcJsonTable.max() =%=[ReqlModel[JsonObject, String]] """[148,[[15,["abc"]]]]"""
      abcJsonTable.max(Index("code")) =%=[ReqlModel[JsonObject, String]] """[148,[[15,["abc"]]],{"index":"code"}]"""
      abcJsonTable.max("code") =%=[ReqlModel[JsonObject, String]] """[148,[[15,["abc"]],"code"]]"""
      abcJsonTable.max(_("code").asNumber.add(7)) =%=[ReqlModel[JsonObject, String]] """[148,[[15,["abc"]],[69,[[2,[0]],[24,[[170,[[10,[0]],"code"]],7]]]]]]"""
      abcJsonTable.max(_("code").asBinary) =%=[ReqlModel[JsonObject, String]] """[148,[[15,["abc"]],[69,[[2,[0]],[170,[[10,[0]],"code"]]]]]]"""

      //tableSlice
      abcJsonTable.orderBy(OrderedIndex(r.asc("code"))).max() =%=[ReqlModel[JsonObject, String]] """[148,[[41,[[15,["abc"]]],{"index":[73,["code"]]}]]]"""
      abcJsonTable.orderBy(OrderedIndex(r.asc("code"))).max("code") =%=[ReqlModel[JsonObject, String]] """[148,[[41,[[15,["abc"]]],{"index":[73,["code"]]}],"code"]]"""
      abcJsonTable.orderBy(OrderedIndex(r.asc("code"))).max(_("code").asNumber.add(7)) =%=[ReqlModel[JsonObject, String]] """[148,[[41,[[15,["abc"]]],{"index":[73,["code"]]}],[69,[[2,[0]],[24,[[170,[[10,[0]],"code"]],7]]]]]]"""
      abcJsonTable.orderBy(OrderedIndex(r.asc("code"))).max(_("code").asBinary) =%=[ReqlModel[JsonObject, String]] """[148,[[41,[[15,["abc"]]],{"index":[73,["code"]]}],[69,[[2,[0]],[170,[[10,[0]],"code"]]]]]]"""

      //selectionOfArray
      abcJsonTable.orderBy(r.asc("code")) shouldBe an[ReqlSelectionOfArray[JsonObject, String]]
      abcJsonTable.orderBy(r.asc("code")).max() =%=[ReqlModel[JsonObject, String]] """[148,[[41,[[15,["abc"]],[73,["code"]]]]]]"""
      abcJsonTable.orderBy(r.asc("code")).max("code") =%=[ReqlModel[JsonObject, String]] """[148,[[41,[[15,["abc"]],[73,["code"]]]],"code"]]"""
      abcJsonTable.orderBy(r.asc("code")).max(_("code").asNumber.add(7)) =%=[ReqlModel[JsonObject, String]] """[148,[[41,[[15,["abc"]],[73,["code"]]]],[69,[[2,[0]],[24,[[170,[[10,[0]],"code"]],7]]]]]]"""
      abcJsonTable.orderBy(r.asc("code")).max(_("code").asBinary) =%=[ReqlModel[JsonObject, String]] """[148,[[41,[[15,["abc"]],[73,["code"]]]],[69,[[2,[0]],[170,[[10,[0]],"code"]]]]]]"""

      //selectionOfStream
      abcJsonTable.skip(1) shouldBe an[ReqlSelectionOfStream[JsonObject, String]]
      abcJsonTable.skip(1).max() =%=[ReqlModel[JsonObject, String]] """[148,[[70,[[15,["abc"]],1]]]]"""
      abcJsonTable.skip(1).max("code") =%=[ReqlModel[JsonObject, String]] """[148,[[70,[[15,["abc"]],1]],"code"]]"""
      abcJsonTable.skip(1).max(_("code").asNumber.add(7)) =%=[ReqlModel[JsonObject, String]] """[148,[[70,[[15,["abc"]],1]],[69,[[2,[0]],[24,[[170,[[10,[0]],"code"]],7]]]]]]"""
      abcJsonTable.skip(1).max(_("code").asBinary) =%=[ReqlModel[JsonObject, String]] """[148,[[70,[[15,["abc"]],1]],[69,[[2,[0]],[170,[[10,[0]],"code"]]]]]]"""

      //finiteStream
      r.range(4).max() =%=[ReqlInteger] """[148,[[173,[4]]]]"""
      //TODO: it should not work
      r.range(4).max("code") =%=[ReqlInteger] """[148,[[173,[4]],"code"]]"""
      r.range(4).max(_.asNumber.add(7)) =%=[ReqlInteger] """[148,[[173,[4]],[69,[[2,[0]],[24,[[10,[0]],7]]]]]]"""

      //infiniteStream
      """r.range().max()""".shouldNot(compile)

      //array
      r.expr(Seq(r.expr(1), r.expr(2), r.expr(3))).
        max() =%=[ReqlInteger] """[148,[[2,[1,2,3]]]]"""

      r.expr(Seq(r.expr(1), r.expr(2), r.expr(3))).
        max("code") =%=[ReqlInteger] """[148,[[2,[1,2,3]],"code"]]"""

      r.expr(Seq(r.expr(1), r.expr(2), r.expr(3))).
        max(_.asNumber.add(7)) =%=[ReqlInteger] """[148,[[2,[1,2,3]],[69,[[2,[0]],[24,[[10,[0]],7]]]]]]"""
    }

    "split" in {
      r.expr("foo  bar bax").split() =%=[ReqlArray[ReqlString]] """[149,["foo  bar bax"]]"""

      r.expr("12,37,,22,").split(",") =%=[ReqlArray[ReqlString]] """[149,["12,37,,22,",","]]"""

      r.expr("mlucy").split("") =%=[ReqlArray[ReqlString]] """[149,["mlucy",""]]"""

      r.expr("12,37,,22,").split(",", 3) =%=[ReqlArray[ReqlString]] """[149,["12,37,,22,",",",3]]"""

      r.expr("foo  bar bax").split(r.expr(null), 1) =%=[ReqlArray[ReqlString]] """[149,["foo  bar bax",null,1]]"""
      r.expr("foo  bar bax").split(1) =%=[ReqlArray[ReqlString]] """[149,["foo  bar bax",null,1]]"""
    }

    "ungroup" in {
      //groupedStream
      abcJsonTable.group(r.sel[ReqlModel[JsonObject, String], ReqlDatum]("code")) shouldBe an[ReqlGroupedStream[ReqlDatum, ReqlModel[JsonObject, String]]]
      abcJsonTable.group(r.sel[ReqlModel[JsonObject, String], ReqlDatum]("code")).ungroup() =%=[ReqlArray[ReqlDatum]] """[150,[[144,[[15,["abc"]],"code"]]]]"""

      //groupedData
      //TODO: fixit
      //r.table("abc").group("code").count() shouldBe an[ReqlGroupedData]
      //r.table("abc").group("code").count().ungroup() =%=[ReqlArray[ReqlDatum]] """[150,[[43,[[144,[[15,["abc"]],"code"]]]]]]"""
    }

    "random" in {
      r.random() =%=[ReqlFloat] "[151,[]]"

      r.random(5) =%=[ReqlInteger] "[151,[5]]"
      "r.random(BigDecimal(2.7))".shouldNot(compile)

      r.random(5, IntegerValues) =%=[ReqlInteger] "[151,[5]]"
      r.random(5, FloatValues) =%=[ReqlFloat] """[151,[5],{"float":true}]"""
      r.random(BigDecimal(2.7), FloatValues) =%=[ReqlFloat] """[151,[2.7],{"float":true}]"""
      "r.random(BigDecimal(2.7), IntegerValues)".shouldNot(compile)

      r.random(2, 5) =%=[ReqlInteger] "[151,[2,5]]"
      "r.random(BigDecimal(2.7), BigDecimal(5.5))".shouldNot(compile)

      r.random(2, 5, IntegerValues) =%=[ReqlInteger] "[151,[2,5]]"
      r.random(2, 5, FloatValues) =%=[ReqlFloat] """[151,[2,5],{"float":true}]"""
      r.random(BigDecimal(2.7), BigDecimal(5.5), FloatValues) =%=[ReqlFloat] """[151,[2.7,5.5],{"float":true}]"""
      "r.random(BigDecimal(2.7), BigDecimal(5.5), IntegerValues)".shouldNot(compile)
    }

    "changes" in {
      //table
      abcJsonTable.changes() =%=[ReqlInfiniteStream[ReqlChangefeedNotification[JsonObject]]] """[152,[[15,["abc"]]]]"""

      abcJsonTable.changes(NotSquash) =%=[ReqlInfiniteStream[ReqlChangefeedNotification[JsonObject]]] """[152,[[15,["abc"]]]]"""
      abcJsonTable.changes(DoSquash) =%=[ReqlInfiniteStream[ReqlChangefeedNotification[JsonObject]]] """[152,[[15,["abc"]]],{"squash":true}]"""
      abcJsonTable.changes(SquashDuring(BigDecimal(4.5))) =%=[ReqlInfiniteStream[ReqlChangefeedNotification[JsonObject]]] """[152,[[15,["abc"]]],{"squash":4.5}]"""

      abcJsonTable.changes(changefeedQueueSize = DefaultChangefeedQueueSize) =%=[ReqlInfiniteStream[ReqlChangefeedNotification[JsonObject]]] """[152,[[15,["abc"]]]]"""
      abcJsonTable.changes(changefeedQueueSize = ChangefeedQueueSize(10)) =%=[ReqlInfiniteStream[ReqlChangefeedNotification[JsonObject]]] """[152,[[15,["abc"]]],{"changefeed_queue_size":10}]"""

      abcJsonTable.changes(includeInitial = NotIncludeInitial) =%=[ReqlInfiniteStream[ReqlChangefeedNotification[JsonObject]]] """[152,[[15,["abc"]]]]"""
      abcJsonTable.changes(includeInitial = IncludeInitial) =%=[ReqlInfiniteStream[ReqlChangefeedNotification[JsonObject]]] """[152,[[15,["abc"]]],{"include_initial":true}]"""

      abcJsonTable.changes(includeStates = NotIncludeStates) =%=[ReqlInfiniteStream[ReqlChangefeedNotification[JsonObject]]] """[152,[[15,["abc"]]]]"""
      abcJsonTable.changes(includeStates = IncludeStates) =%=[ReqlInfiniteStream[ReqlChangefeedNotification[JsonObject]]] """[152,[[15,["abc"]]],{"include_states":true}]"""

      """r.table[ReqlObject]("abc").changes(includeOffsets = NotIncludeOffsets)""".shouldNot(compile)
      //r.table("abc").changes(includeOffsets = NotIncludeOffsets) =%=[ReqlInfiniteStream] """[152,[[15,["abc"]]]]"""
      //r.table("abc").changes(includeOffsets = IncludeOffsets) =%=[ReqlInfiniteStream] """[[152,[[15,["abc"]]],{"include_offsets":true}]"""

      abcJsonTable.changes(includeTypes = NotIncludeTypes) =%=[ReqlInfiniteStream[ReqlChangefeedNotification[JsonObject]]] """[152,[[15,["abc"]]]]"""
      abcJsonTable.changes(includeTypes = IncludeTypes) =%=[ReqlInfiniteStream[ReqlChangefeedNotification[JsonObject]]] """[152,[[15,["abc"]]],{"include_types":true}]"""


      //tableSlice
      abcJsonTable.orderBy(OrderedIndex(r.asc("code"))) shouldBe an[ReqlTableSlice[JsonObject, String]]
      abcJsonTable.orderBy(OrderedIndex(r.asc("code"))).
        changes() =%=[ReqlInfiniteStream[ReqlChangefeedNotification[JsonObject]]] """[152,[[41,[[15,["abc"]]],{"index":[73,["code"]]}]]]"""
      abcJsonTable.orderBy(OrderedIndex(r.asc("code"))).
        changes(includeTypes = IncludeTypes) =%=[ReqlInfiniteStream[ReqlChangefeedNotification[JsonObject]]] """[152,[[41,[[15,["abc"]]],{"index":[73,["code"]]}]],{"include_types":true}]"""
      """
        |r.table("abc").orderBy(OrderedIndex(r.asc("code"))).
        |        changes(includeOffsets = NotIncludeOffsets)
      """.stripMargin.shouldNot(compile)


      //selectionOfArray
      //Cannot call `changes` on an eager stream in:
      abcJsonTable.orderBy(r.asc("code")) shouldBe an[ReqlSelectionOfArray[JsonObject, String]]
      """
        |r.table[ReqlObject]("abc").orderBy(r.asc("code")).changes()
      """.stripMargin.shouldNot(compile)


      //selectionOfStream
      //Cannot call `changes` on an eager stream in:
      abcJsonTable.skip(1) shouldBe an[ReqlSelectionOfStream[JsonObject, String]]
      """
        |r.table("abc").skip(1).changes()
      """.stripMargin.shouldNot(compile)


      //finiteStream
      r.range(4).max() =%=[ReqlDatum] """[148,[[173,[4]]]]"""
      """
        |r.range(4).changes()
      """.stripMargin.shouldNot(compile)


      //infiniteStream
      """r.range().changes()""".shouldNot(compile)


      //array
      """r.expr(Seq(r.expr(1), r.expr(2))).changes()""".shouldNot(compile)


      //selectionOfObject
      abcJsonTable.get("uuid").changes() =%=[ReqlInfiniteStream[ReqlChangefeedNotification[JsonObject]]] """[152,[[16,[[15,["abc"]],"uuid"]]]]"""

      abcJsonTable.get("uuid").changes(NotSquash) =%=[ReqlInfiniteStream[ReqlChangefeedNotification[JsonObject]]] """[152,[[16,[[15,["abc"]],"uuid"]]]]"""
      abcJsonTable.get("uuid").changes(DoSquash) =%=[ReqlInfiniteStream[ReqlChangefeedNotification[JsonObject]]] """[152,[[16,[[15,["abc"]],"uuid"]]],{"squash":true}]"""
      abcJsonTable.get("uuid").changes(SquashDuring(BigDecimal(4.5))) =%=[ReqlInfiniteStream[ReqlChangefeedNotification[JsonObject]]] """[152,[[16,[[15,["abc"]],"uuid"]]],{"squash":4.5}]"""

      abcJsonTable.get("uuid").changes(changefeedQueueSize = DefaultChangefeedQueueSize) =%=[ReqlInfiniteStream[ReqlChangefeedNotification[JsonObject]]] """[152,[[16,[[15,["abc"]],"uuid"]]]]"""
      abcJsonTable.get("uuid").changes(changefeedQueueSize = ChangefeedQueueSize(10)) =%=[ReqlInfiniteStream[ReqlChangefeedNotification[JsonObject]]] """[152,[[16,[[15,["abc"]],"uuid"]]],{"changefeed_queue_size":10}]"""

      abcJsonTable.get("uuid").changes(includeInitial = NotIncludeInitial) =%=[ReqlInfiniteStream[ReqlChangefeedNotification[JsonObject]]] """[152,[[16,[[15,["abc"]],"uuid"]]]]"""
      abcJsonTable.get("uuid").changes(includeInitial = IncludeInitial) =%=[ReqlInfiniteStream[ReqlChangefeedNotification[JsonObject]]] """[152,[[16,[[15,["abc"]],"uuid"]]],{"include_initial":true}]"""

      abcJsonTable.get("uuid").changes(includeStates = NotIncludeStates) =%=[ReqlInfiniteStream[ReqlChangefeedNotification[JsonObject]]] """[152,[[16,[[15,["abc"]],"uuid"]]]]"""
      abcJsonTable.get("uuid").changes(includeStates = IncludeStates) =%=[ReqlInfiniteStream[ReqlChangefeedNotification[JsonObject]]] """[152,[[16,[[15,["abc"]],"uuid"]]],{"include_states":true}]"""

      """abcJsonTable.get("uuid").changes(includeOffsets = NotIncludeOffsets)""".shouldNot(compile)

      abcJsonTable.get("uuid").changes(includeTypes = NotIncludeTypes) =%=[ReqlInfiniteStream[ReqlChangefeedNotification[JsonObject]]] """[152,[[16,[[15,["abc"]],"uuid"]]]]"""
      abcJsonTable.get("uuid").changes(includeTypes = IncludeTypes) =%=[ReqlInfiniteStream[ReqlChangefeedNotification[JsonObject]]] """[152,[[16,[[15,["abc"]],"uuid"]]],{"include_types":true}]"""


      //complex case from docs
      //TODO: it's special case, it allows includeOffsets option
      /*r.table("abc").orderBy(
        OrderedIndex(r.desc("score"))
      ).limit(10).changes()*/
    }

    "args" in {
      //type
      r.args(r.expr(Nil: List[ReqlDatum])) shouldBe an[ReqlArgs]
      r.args(r.expr(Nil: List[ReqlDatum])) should not be an[ReqlValue]

      //query
      r.args(Seq(
        r.expr("code"), r.expr("status")
      )) =%=[ReqlArgs] """[154,[[2,["code","status"]]]]"""

      r.args(r.expr(Seq(
        r.expr("code"), r.expr("status")
      ))) =%=[ReqlArgs] """[154,[[2,["code","status"]]]]"""
    }

    "binary" in {
      r.binary(ByteString(10, 20, 30, 40)) shouldBe an[ReqlBinary]
      r.binary(ByteString(10, 20, 30, 40)) shouldBe an[ReqlPseudo]
      r.binary(ByteString(10, 20, 30, 40)) shouldBe an[ReqlDatum]
      r.binary(ByteString(10, 20, 30, 40)) =%=[ReqlBinary] """{"$reql_type$":"BINARY","data":"ChQeKA=="}"""
    }

    "geojson" in {
      r.geojson(JsonObject.fromMap(Map(
        "type" -> Json.fromString("Point"),
        "coordinates" -> Json.arr(
          Json.fromBigDecimal(-122.423246),
          Json.fromBigDecimal(37.779388)
        )
      ))) =%=[ReqlGeometry] """[157,[{"type":"Point","coordinates":[2,[-122.423246,37.779388]]}]]"""
    }

    "toGeojson" in {
      r.geojson(JsonObject.fromMap(Map(
        "type" -> Json.fromString("Point"),
        "coordinates" -> Json.arr(
          Json.fromBigDecimal(-122.423246),
          Json.fromBigDecimal(37.779388)
        )
      ))).toGeojson() =%=[ReqlObject] """[158,[[157,[{"type":"Point","coordinates":[2,[-122.423246,37.779388]]}]]]]"""
    }

    "point" in {
      r.point(r.expr(-122.423246), r.expr(37.779388)) shouldBe an[ReqlPoint]
      r.point(r.expr(-122.423246), r.expr(37.779388)) shouldBe an[ReqlGeometry]
      r.point(r.expr(-122.423246), r.expr(37.779388)) shouldBe an[ReqlDatum]
      r.point(r.expr(-122.423246), r.expr(37.779388)) =%=[ReqlPoint] """[159,[-122.423246,37.779388]]"""
    }

    "line" in {
      """r.line()""".shouldNot(compile)
      """r.line(r.point(r.expr(-122.423246), r.expr(37.779388)))""".shouldNot(compile)

      r.line(
        r.point(r.expr(-122.423246), r.expr(37.779388)),
        r.point(r.expr(-121.886420), r.expr(37.329898))
      ) =%=[ReqlLine] """[160,[[159,[-122.423246,37.779388]],[159,[-121.88642,37.329898]]]]"""

      r.line(
        r.point(r.expr(-122.423246), r.expr(37.779388)),
        r.point(r.expr(-121.886420), r.expr(37.329898)),
        r.point(r.expr(-121.986420), r.expr(37.229898))
      ) =%=[ReqlLine] """[160,[[159,[-122.423246,37.779388]],[159,[-121.88642,37.329898]],[159,[-121.98642,37.229898]]]]"""

      """r.line(Seq(r.expr(-122.423246), r.expr(37.779388)))""".shouldNot(compile)

      r.line(
        Seq(r.expr(-122.423246), r.expr(37.779388)),
        Seq(r.expr(-121.886420), r.expr(37.329898))
      ) =%=[ReqlLine] """[160,[[2,[-122.423246,37.779388]],[2,[-121.88642,37.329898]]]]"""

      r.line(
        Seq(r.expr(-122.423246), r.expr(37.779388)),
        Seq(r.expr(-121.886420), r.expr(37.329898)),
        Seq(r.expr(-121.986420), r.expr(37.229898))
      ) =%=[ReqlLine] """[160,[[2,[-122.423246,37.779388]],[2,[-121.88642,37.329898]],[2,[-121.98642,37.229898]]]]"""
    }

    "polygon" in {
      """r.polygon()""".shouldNot(compile)
      """r.polygon(r.point(r.expr(-122.423246), r.expr(37.779388)))""".shouldNot(compile)
      """
        |r.polygon(
        |  r.point(r.expr(-122.423246), r.expr(37.779388)),
        |  r.point(r.expr(-121.886420), r.expr(37.329898))
        |)
      """.stripMargin.shouldNot(compile)

      r.polygon(
        r.point(r.expr(-122.423246), r.expr(37.779388)),
        r.point(r.expr(-121.886420), r.expr(37.329898)),
        r.point(r.expr(-121.986420), r.expr(37.229898))
      ) =%=[ReqlPolygon] """[161,[[159,[-122.423246,37.779388]],[159,[-121.88642,37.329898]],[159,[-121.98642,37.229898]]]]"""

      r.polygon(
        r.point(r.expr(-122.423246), r.expr(37.779388)),
        r.point(r.expr(-121.886420), r.expr(37.329898)),
        r.point(r.expr(-121.986420), r.expr(37.229898)),
        r.point(r.expr(-122.986420), r.expr(38.229898))
      ) =%=[ReqlPolygon] """[161,[[159,[-122.423246,37.779388]],[159,[-121.88642,37.329898]],[159,[-121.98642,37.229898]],[159,[-122.98642,38.229898]]]]"""

      """r.polygon(Seq(r.expr(-122.423246), r.expr(37.779388)))""".shouldNot(compile)
      """r.polygon(
        |  Seq(r.expr(-122.423246), r.expr(37.779388)),
        |  Seq(r.expr(-121.886420), r.expr(37.329898))
        |)
      """.stripMargin.shouldNot(compile)

      r.polygon(
        Seq(r.expr(-122.423246), r.expr(37.779388)),
        Seq(r.expr(-121.886420), r.expr(37.329898)),
        Seq(r.expr(-121.986420), r.expr(37.229898))
      ) =%=[ReqlPolygon] """[161,[[2,[-122.423246,37.779388]],[2,[-121.88642,37.329898]],[2,[-121.98642,37.229898]]]]"""

      r.polygon(
        Seq(r.expr(-122.423246), r.expr(37.779388)),
        Seq(r.expr(-121.886420), r.expr(37.329898)),
        Seq(r.expr(-121.986420), r.expr(37.229898)),
        Seq(r.expr(-122.986420), r.expr(38.229898))
      ) =%=[ReqlPolygon] """[161,[[2,[-122.423246,37.779388]],[2,[-121.88642,37.329898]],[2,[-121.98642,37.229898]],[2,[-122.98642,38.229898]]]]"""
    }

    "distance" in {
      //point & point
      r.point(r.expr(-122.423246), r.expr(37.779388)).distance(
        r.point(r.expr(-117.220406), r.expr(32.719464))
      ) =%=[ReqlFloat] """[162,[[159,[-122.423246,37.779388]],[159,[-117.220406,32.719464]]]]"""

      //point & line
      r.point(r.expr(-122.423246), r.expr(37.779388)).distance(
        r.line(
          r.point(r.expr(-122.423246), r.expr(37.779388)),
          r.point(r.expr(-121.886420), r.expr(37.329898))
        )
      ) =%=[ReqlFloat] """[162,[[159,[-122.423246,37.779388]],[160,[[159,[-122.423246,37.779388]],[159,[-121.88642,37.329898]]]]]]"""

      //point & polygon
      r.point(r.expr(-122.423246), r.expr(37.779388)).distance(
        r.polygon(
          r.point(r.expr(-122.423246), r.expr(37.779388)),
          r.point(r.expr(-121.886420), r.expr(37.329898)),
          r.point(r.expr(-121.986420), r.expr(37.229898))
        )
      ) =%=[ReqlFloat] """[162,[[159,[-122.423246,37.779388]],[161,[[159,[-122.423246,37.779388]],[159,[-121.88642,37.329898]],[159,[-121.98642,37.229898]]]]]]"""

      //geoSystem options
      r.point(r.expr(-122.423246), r.expr(37.779388)).distance(
        r.point(r.expr(-117.220406), r.expr(32.719464)),
        DefaultGeoSystem
      ) =%=[ReqlFloat] """[162,[[159,[-122.423246,37.779388]],[159,[-117.220406,32.719464]]]]"""

      r.point(r.expr(-122.423246), r.expr(37.779388)).distance(
        r.point(r.expr(-117.220406), r.expr(32.719464)),
        WGS84
      ) =%=[ReqlFloat] """[162,[[159,[-122.423246,37.779388]],[159,[-117.220406,32.719464]]],{"geo_system":"WGS84"}]"""

      r.point(r.expr(-122.423246), r.expr(37.779388)).distance(
        r.point(r.expr(-117.220406), r.expr(32.719464)),
        UnitSphere
      ) =%=[ReqlFloat] """[162,[[159,[-122.423246,37.779388]],[159,[-117.220406,32.719464]]],{"geo_system":"unit_sphere"}]"""

      //unit options
      r.point(r.expr(-122.423246), r.expr(37.779388)).distance(
        r.point(r.expr(-117.220406), r.expr(32.719464)),
        unit = DefaultDistanceUnit
      ) =%=[ReqlFloat] """[162,[[159,[-122.423246,37.779388]],[159,[-117.220406,32.719464]]]]"""

      r.point(r.expr(-122.423246), r.expr(37.779388)).distance(
        r.point(r.expr(-117.220406), r.expr(32.719464)),
        unit = Meter
      ) =%=[ReqlFloat] """[162,[[159,[-122.423246,37.779388]],[159,[-117.220406,32.719464]]],{"unit":"m"}]"""

      r.point(r.expr(-122.423246), r.expr(37.779388)).distance(
        r.point(r.expr(-117.220406), r.expr(32.719464)),
        unit = Kilometer
      ) =%=[ReqlFloat] """[162,[[159,[-122.423246,37.779388]],[159,[-117.220406,32.719464]]],{"unit":"km"}]"""

      r.point(r.expr(-122.423246), r.expr(37.779388)).distance(
        r.point(r.expr(-117.220406), r.expr(32.719464)),
        unit = InternationalMile
      ) =%=[ReqlFloat] """[162,[[159,[-122.423246,37.779388]],[159,[-117.220406,32.719464]]],{"unit":"mi"}]"""

      r.point(r.expr(-122.423246), r.expr(37.779388)).distance(
        r.point(r.expr(-117.220406), r.expr(32.719464)),
        unit = NauticalMile
      ) =%=[ReqlFloat] """[162,[[159,[-122.423246,37.779388]],[159,[-117.220406,32.719464]]],{"unit":"nm"}]"""

      r.point(r.expr(-122.423246), r.expr(37.779388)).distance(
        r.point(r.expr(-117.220406), r.expr(32.719464)),
        unit = InternationalFoot
      ) =%=[ReqlFloat] """[162,[[159,[-122.423246,37.779388]],[159,[-117.220406,32.719464]]],{"unit":"ft"}]"""

      //line & point
      r.line(
        r.point(r.expr(-122.423246), r.expr(37.779388)),
        r.point(r.expr(-121.886420), r.expr(37.329898))
      ).distance(
        r.point(r.expr(-122.423246), r.expr(37.779388)),
        WGS84,
        Kilometer
      ) =%=[ReqlFloat] """[162,[[160,[[159,[-122.423246,37.779388]],[159,[-121.88642,37.329898]]]],[159,[-122.423246,37.779388]]],{"geo_system":"WGS84","unit":"km"}]"""

      //polygon & point
      r.polygon(
        r.point(r.expr(-122.423246), r.expr(37.779388)),
        r.point(r.expr(-121.886420), r.expr(37.329898)),
        r.point(r.expr(-121.986420), r.expr(37.229898))
      ).distance(
        r.point(r.expr(-122.423246), r.expr(37.779388)),
        WGS84,
        InternationalFoot
      ) =%=[ReqlFloat] """[162,[[161,[[159,[-122.423246,37.779388]],[159,[-121.88642,37.329898]],[159,[-121.98642,37.229898]]]],[159,[-122.423246,37.779388]]],{"geo_system":"WGS84","unit":"ft"}]"""
    }

    "intersects" in {
      //array
      r.expr(Seq(r.expr(1), r.expr(2), r.expr(3))).
        map({ x =>
          r.polygon(
            r.point(r.expr(0), r.expr(0)),
            r.point(r.expr(0), r.expr(10)),
            r.point(x.asFloat, x.asFloat)
          )
        }).intersects(r.point(r.expr(1), r.expr(2))) =%=[ReqlArray[ReqlPolygon]] """[163,[[38,[[2,[1,2,3]],[69,[[2,[0]],[161,[[159,[0,0]],[159,[0,10]],[159,[[10,[0]],[10,[0]]]]]]]]]],[159,[1,2]]]]"""

      //finiteStream
      //r.table("abc").skip(1).apply("area").
      //  intersects(r.point(r.expr(-117.206201), r.expr(32.725186)))


      //r.table("abc").orderBy(r.asc("code")) shouldBe an[ReqlSelectionOfArray]

      //r.table("abc").orderBy(r.asc("code")).apply("area")
    }

    "includes" in {
      //array
      r.expr(Seq(r.expr(1), r.expr(2), r.expr(3))).
        map({ x =>
          r.polygon(
            r.point(r.expr(0), r.expr(0)),
            r.point(r.expr(0), r.expr(10)),
            r.point(x.asFloat, x.asFloat)
          )
        }).includes(r.point(r.expr(1), r.expr(2))) =%=[ReqlArray[ReqlPolygon]] """[164,[[38,[[2,[1,2,3]],[69,[[2,[0]],[161,[[159,[0,0]],[159,[0,10]],[159,[[10,[0]],[10,[0]]]]]]]]]],[159,[1,2]]]]"""
    }

    "circle" in {
      //circle with point
      r.circle(
        r.point(r.expr(-122.423246), r.expr(37.779388)),
        r.expr(250.8)
      ) =%=[ReqlGeometry] """[165,[[159,[-122.423246,37.779388]],250.8]]"""

      r.circle(
        r.point(r.expr(-122.423246), r.expr(37.779388)),
        r.expr(250.8),
        NumVertices(16),
        WGS84,
        InternationalFoot
      ) =%=[ReqlGeometry] """[165,[[159,[-122.423246,37.779388]],250.8],{"num_vertices":16,"geo_system":"WGS84","unit":"ft"}]"""

      r.circle(
        r.point(r.expr(-122.423246), r.expr(37.779388)),
        r.expr(250.8),
        NumVertices(16),
        WGS84,
        InternationalFoot,
        DefaultCircleFill
      ) =%=[ReqlGeometry] """[165,[[159,[-122.423246,37.779388]],250.8],{"num_vertices":16,"geo_system":"WGS84","unit":"ft"}]"""

      r.circle(
        r.point(r.expr(-122.423246), r.expr(37.779388)),
        r.expr(250.8),
        NumVertices(16),
        WGS84,
        InternationalFoot,
        FillCircle
      ) =%=[ReqlGeometry] """[165,[[159,[-122.423246,37.779388]],250.8],{"num_vertices":16,"geo_system":"WGS84","unit":"ft","fill":true}]"""

      r.circle(
        r.point(r.expr(-122.423246), r.expr(37.779388)),
        r.expr(250.8),
        NumVertices(16),
        WGS84,
        InternationalFoot,
        NotFillCircle
      ) =%=[ReqlGeometry] """[165,[[159,[-122.423246,37.779388]],250.8],{"num_vertices":16,"geo_system":"WGS84","unit":"ft","fill":false}]"""

      //circle with array
      /*r.circle(
        r.expr(Seq(r.expr(-122.423246), r.expr(37.779388))),
        r.expr(250.8),
        NumVertices(16),
        WGS84,
        InternationalFoot,
        NotFillCircle
      ) =%=[ReqlGeometry] """[165,[[2,[122.423246,37.779388]],250.8],{"num_vertices":16,"geo_system":"WGS84","unit":"ft","fill":false}]"""*/
    }

    "getIntersecting" in {
      abcJsonTable.getIntersecting(
        r.point(r.expr(-117.220406), r.expr(32.719464)),
        Index("area")
      ) =%=[ReqlSelectionOfStream[JsonObject, String]] """[166,[[15,["abc"]],[159,[-117.220406,32.719464]]],{"index":"area"}]"""
    }

    "fill" in {
      r.line(
        r.point(r.expr(-122.423246), r.expr(37.779388)),
        r.point(r.expr(-121.886420), r.expr(37.329898)),
        r.point(r.expr(-121.986420), r.expr(37.229898))
      ).fill() =%=[ReqlPolygon] """[167,[[160,[[159,[-122.423246,37.779388]],[159,[-121.88642,37.329898]],[159,[-121.98642,37.229898]]]]]]"""
    }

    "get_nearest" in {
      abcJsonTable.getNearest(
        r.point(r.expr(-117.220406), r.expr(32.719464)),
        Index("area")
      ) =%=[ReqlArray[ReqlDistanceResult[JsonObject]]] """[168,[[15,["abc"]],[159,[-117.220406,32.719464]]],{"index":"area"}]"""

      abcJsonTable.getNearest(
        r.point(r.expr(-117.220406), r.expr(32.719464)),
        Index("area"),
        MaxResults(10),
        MaxDistance(2),
        Kilometer,
        WGS84
      ) =%=[ReqlArray[ReqlDistanceResult[JsonObject]]] """[168,[[15,["abc"]],[159,[-117.220406,32.719464]]],{"index":"area","max_results":10,"max_dist":2,"unit":"km","geo_system":"WGS84"}]"""
    }

    "polygon_sub" in {
      r.polygon(
        r.point(r.expr(-122.4), r.expr(37.7)),
        r.point(r.expr(-122.4), r.expr(37.3)),
        r.point(r.expr(-121.8), r.expr(37.3)),
        r.point(r.expr(-121.8), r.expr(37.7))
      ).polygonSub(
        r.polygon(
          r.point(r.expr(-122.3), r.expr(37.4)),
          r.point(r.expr(-122.3), r.expr(37.6)),
          r.point(r.expr(-122.1), r.expr(37.6)),
          r.point(r.expr(-122.1), r.expr(37.4))
        )
      ) =%=[ReqlPolygon] """[171,[[161,[[159,[-122.4,37.7]],[159,[-122.4,37.3]],[159,[-121.8,37.3]],[159,[-121.8,37.7]]]],[161,[[159,[-122.3,37.4]],[159,[-122.3,37.6]],[159,[-122.1,37.6]],[159,[-122.1,37.4]]]]]]"""
    }

    "minval" in {
      r.minval =*= "[180,[]]"
    }

    "maxval" in {
      r.maxval =*= "[181,[]]"
    }

  }

  "Shape" should {
    "work as table" in new ShapesData {
      import TestDatabase.abc

      abc.table() shouldBe an[ReqlTable[ReqlObject, String]]

      import queries.all._

      val abcTable: ReqlTable[Abc, String] = abc.table()
      abcTable.get("uuid") =%/%=[ReqlSelectionOfObject[Abc, String]] """[16,[[15,[[14,["test"]],"abc"]],"uuid"]]"""

      val sel: ReqlSelectionOfObject[Abc, String] = TestDatabase.abc.table().get("uuid")
      TestDatabase.abc.table().get("uuid") =%/%=[ReqlSelectionOfObject[ReqlObject, String]] """[16,[[15,[[14,["test"]],"abc"]],"uuid"]]"""
    }
  }

}
