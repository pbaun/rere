package rere.ql.options

import rere.ql.queries.{DSLKeyValuePair, DSLKeyValuePairList}
import rere.ql.types.ReqlObject

trait HttpParamsOptions { _: Classes =>

  sealed trait HttpParamsOptions extends ComposableOptions

  case object WithoutHttpParams extends HttpParamsOptions {
    def isEmpty = true
    def view = Nil
    val innerQuery = query
  }

  case class HttpParams(params: Seq[HttpQueryParam]) extends HttpParamsOptions {
    def isEmpty = false
    def view = {
      val paramsObject: ReqlObject = new DSLKeyValuePairList(params.foldLeft(Nil: List[DSLKeyValuePair]) {
        (acc, el) => new DSLKeyValuePair(el.key, el.value) :: acc
      })
      "params" -> paramsObject :: Nil
    }
    def innerQuery = query
  }

}
