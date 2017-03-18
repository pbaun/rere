package rere.driver

package object runners {

  /**
    * OPTIONS OF .RUN
    *
    * readMode - implemented
    *
    * timeFormat - NOT IMPLEMENTED [option for driver/client]
    *   `raw` to get raw reql time pseudo type representation.
    *   `native` to get regular time object
    *
    * profile - implemented
    *
    * durability - implemented
    *
    * groupFormat - NOT IMPLEMENTED [option for driver/client]
    *
    * noreply - 1/2 implemented. Option transmitting to db but driver still waiting for reply
    *
    * db - NOT IMPLEMENTED [maybe unnecessary]
    *
    * arrayLimit - implemented
    *
    * binaryFormat - NOT IMPLEMENTED [option for driver/client]
    *
    * minBatchRows - implemented
    *
    * maxBatchRows - implemented
    *
    * maxBatchBytes - implemented
    *
    * firstBatchScaledownFactor - implemented
    *
    * */

  object all
    extends SingleValueRunners
    with SelectionOfObjectRunners
    with FiniteStreamRunners
    with InfiniteStreamRunners

}
