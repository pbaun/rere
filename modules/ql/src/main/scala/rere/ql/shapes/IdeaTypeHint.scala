package rere.ql.shapes

trait IdeaTypeHint[M] {
  _: Shape[_, _, M, _] { type Model } =>

  override type Model = M
}
