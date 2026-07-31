package notes.client

import notes.shared.crdt.RgaDocument
import notes.shared.model.Op

/** Derive CRDT ops from a textarea value change against the current document text. */
object TextDiff:

  final case class DiffOps(ops: List[Op], newText: String)

  def applyChange(doc: RgaDocument, siteId: String, newText: String): List[Op] =
    val oldText = doc.text
    if oldText == newText then Nil
    else
      val prefix = commonPrefix(oldText, newText)
      val oldSuffixStart = oldText.length - commonSuffix(oldText.drop(prefix), newText.drop(prefix))
      val newSuffixStart = newText.length - commonSuffix(oldText.drop(prefix), newText.drop(prefix))

      val deleteCount = oldSuffixStart - prefix
      val inserted = newText.substring(prefix, newSuffixStart)

      val deletes = doc.deleteLocalRange(siteId, prefix, deleteCount)
      val inserts = doc.insertLocalString(siteId, prefix, inserted)
      deletes ++ inserts

  private def commonPrefix(a: String, b: String): Int =
    val n = math.min(a.length, b.length)
    var i = 0
    while i < n && a.charAt(i) == b.charAt(i) do i += 1
    i

  private def commonSuffix(a: String, b: String): Int =
    val n = math.min(a.length, b.length)
    var i = 0
    while i < n && a.charAt(a.length - 1 - i) == b.charAt(b.length - 1 - i) do i += 1
    i
