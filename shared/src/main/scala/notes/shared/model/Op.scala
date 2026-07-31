package notes.shared.model

/** Causal operations that mutate a collaborative document. */
sealed trait Op:
  def siteId: String

object Op:
  final case class Insert(
      siteId: String,
      id: CharId,
      value: Char,
      leftOrigin: Option[CharId]
  ) extends Op

  final case class Delete(
      siteId: String,
      target: CharId
  ) extends Op
