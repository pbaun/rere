package rere.ql

package object queries {
  object all extends AllQueries

  object values extends ValueQueries

  object binary extends BinaryQueries

  object logic extends LogicQueries

  object math extends MathQueries

  object db extends DatabaseQueries

  object table extends TableQueries

  object document extends DocumentQueries

  object control extends ControlQueries
}
