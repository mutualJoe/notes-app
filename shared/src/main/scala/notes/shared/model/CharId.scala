package notes.shared.model

/** Globally unique identifier for a character in the CRDT document. */
final case class CharId(siteId: String, clock: Long) extends Ordered[CharId]:
  def compare(that: CharId): Int =
    val bySite = siteId.compareTo(that.siteId)
    if bySite != 0 then bySite else clock.compareTo(that.clock)

  override def toString: String = s"$siteId@$clock"
