package rere.sasl.scram.server

import rere.sasl.util.BinaryString

final case class AuthData(
  username: String,
  saltedPassword: BinaryString,
  salt: BinaryString,
  i: Int,
  isReal: Boolean,
  clientKey: BinaryString,
  storedKey: BinaryString)
