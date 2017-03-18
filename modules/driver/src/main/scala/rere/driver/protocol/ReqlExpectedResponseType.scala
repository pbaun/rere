package rere.driver.protocol

sealed trait ReqlExpectedResponseType

final case object Atom extends ReqlExpectedResponseType

final case object Stream extends ReqlExpectedResponseType
