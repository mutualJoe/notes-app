package notes.shared.model

/** A single character atom in an RGA-style CRDT. */
final case class Atom(
    id: CharId,
    value: Char,
    /** Character this atom was inserted after (`None` = beginning of document). */
    leftOrigin: Option[CharId],
    deleted: Boolean = false
)
