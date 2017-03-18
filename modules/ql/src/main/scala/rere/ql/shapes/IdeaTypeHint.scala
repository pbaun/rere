package rere.ql.shapes

trait IdeaTypeHint[M] {
  _: Shape[_, _, M] { type Model } =>

  override type Model = M
}
