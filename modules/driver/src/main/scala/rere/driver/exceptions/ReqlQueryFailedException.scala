package rere.driver.exceptions

import rere.driver.protocol.ReqlServerSentError

class ReqlQueryFailedException(serverSentError: ReqlServerSentError) extends Exception
