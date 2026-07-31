package notes.shared.crdt

import notes.shared.model.{Atom, CharId, Op}

import scala.collection.mutable

/**
 * Replicated Growable Array (RGA) CRDT for collaborative text.
 *
 * Concurrent inserts after the same origin are ordered by [[CharId]].
 * Deletes are tombstones so replicas converge without coordination.
 */
final class RgaDocument(private var atoms: Vector[Atom] = Vector.empty):

  private var localClock: Long =
    atoms.map(_.id.clock).foldLeft(0L)(math.max)

  def snapshot: Vector[Atom] = atoms

  def clock: Long = localClock

  def visible: Vector[Atom] = atoms.filterNot(_.deleted)

  def text: String = visible.map(_.value).mkString

  def length: Int = visible.length

  /** Map a visible caret index (0..length) to the left-origin CharId. */
  def originAt(visibleIndex: Int): Option[CharId] =
    val vis = visible
    if visibleIndex <= 0 then None
    else if visibleIndex >= vis.length then vis.lastOption.map(_.id)
    else Some(vis(visibleIndex - 1).id)

  /** CharId at a visible index, if any. */
  def idAt(visibleIndex: Int): Option[CharId] =
    val vis = visible
    if visibleIndex < 0 || visibleIndex >= vis.length then None
    else Some(vis(visibleIndex).id)

  /** Visible index of a CharId, or -1 if missing / deleted. */
  def visibleIndexOf(id: CharId): Int =
    visible.indexWhere(_.id == id)

  def insertLocal(siteId: String, visibleIndex: Int, ch: Char): Op.Insert =
    localClock += 1
    val id = CharId(siteId, localClock)
    val origin = originAt(visibleIndex)
    val op = Op.Insert(siteId, id, ch, origin)
    apply(op)
    op

  def deleteLocal(siteId: String, visibleIndex: Int): Option[Op.Delete] =
    idAt(visibleIndex).map { target =>
      val op = Op.Delete(siteId, target)
      apply(op)
      op
    }

  def insertLocalString(siteId: String, visibleIndex: Int, str: String): List[Op.Insert] =
    str.zipWithIndex.map { case (ch, i) =>
      insertLocal(siteId, visibleIndex + i, ch)
    }.toList

  def deleteLocalRange(siteId: String, start: Int, count: Int): List[Op.Delete] =
    // Delete from the end so indices remain stable relative to remaining text
    (0 until count).toList.flatMap { _ =>
      deleteLocal(siteId, start)
    }

  def apply(op: Op): Unit = op match
    case Op.Insert(_, id, value, leftOrigin) =>
      if !atoms.exists(_.id == id) then
        localClock = math.max(localClock, id.clock)
        val atom = Atom(id, value, leftOrigin, deleted = false)
        atoms = insertAtom(atoms, atom)
    case Op.Delete(_, target) =>
      atoms = atoms.map { a =>
        if a.id == target then a.copy(deleted = true) else a
      }

  def applyAll(ops: Iterable[Op]): Unit =
    ops.foreach(apply)

  def replaceWith(snapshot: Vector[Atom]): Unit =
    atoms = snapshot
    localClock = atoms.map(_.id.clock).foldLeft(0L)(math.max)

  private def insertAtom(existing: Vector[Atom], atom: Atom): Vector[Atom] =
    val buf = mutable.ArrayBuffer.from(existing)
    val insertAt = findInsertIndex(buf.toVector, atom)
    buf.insert(insertAt, atom)
    buf.toVector

  /**
   * Place `atom` immediately after its left origin (skipping any concurrent
   * inserts with a greater CharId that already sit after that origin).
   */
  private def findInsertIndex(existing: Vector[Atom], atom: Atom): Int =
    atom.leftOrigin match
      case None =>
        // Newest-first among start-origin siblings keeps sequential typing at index 0 correct.
        var i = 0
        while i < existing.length &&
          existing(i).leftOrigin.isEmpty &&
          existing(i).id > atom.id
        do i += 1
        i
      case Some(origin) =>
        val originIdx = existing.indexWhere(_.id == origin)
        if originIdx < 0 then existing.length
        else
          var i = originIdx + 1
          while i < existing.length && isSiblingBefore(existing(i), origin, atom.id) do
            i += 1
          i

  /**
   * Among siblings sharing `origin`, higher [[CharId]] sorts closer to the origin
   * (newest-first). This makes "insert at visible index" match user intent while
   * still converging under concurrency.
   */
  private def isSiblingBefore(candidate: Atom, origin: CharId, newId: CharId): Boolean =
    candidate.leftOrigin.contains(origin) && candidate.id > newId

object RgaDocument:
  def empty: RgaDocument = new RgaDocument()

  def fromSnapshot(atoms: Vector[Atom]): RgaDocument =
    new RgaDocument(atoms)

  def fromText(siteId: String, text: String): RgaDocument =
    val doc = empty
    text.zipWithIndex.foreach { case (ch, i) =>
      doc.insertLocal(siteId, i, ch)
    }
    doc
