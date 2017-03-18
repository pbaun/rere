package rere.ql.values

import rere.ql.options.Options
import rere.ql.ql2.Term.TermType
import rere.ql.types.{ReqlNumber, ReqlPoint}

class ReqlPointQuery(longitude: ReqlNumber, latitude: ReqlNumber) extends ReqlPoint {
  val command = TermType.POINT
  val string = "point"
  val arguments = longitude :: latitude :: Nil
  val options = Options.empty
}
