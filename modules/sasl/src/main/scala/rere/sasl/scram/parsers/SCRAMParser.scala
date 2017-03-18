package rere.sasl.scram.parsers

import org.parboiled2.{Parser, ParserInput}

class SCRAMParser(val input: ParserInput)
  extends Parser
  with RFC5802Parser
