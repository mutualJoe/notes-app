package notes.shared.protocol

import notes.shared.model.{Atom, CharId, Op}

final case class Peer(
    userId: String,
    userName: String,
    color: String,
    cursor: Int = 0,
    selectionEnd: Int = 0
)

final case class DocumentSnapshot(
    docId: String,
    title: String,
    atoms: Vector[Atom],
    version: Long
)

final case class VersionEntry(
    id: String,
    version: Long,
    label: String,
    createdAt: Long,
    textPreview: String
)

/** Client → server */
sealed trait ClientMessage

object ClientMessage:
  final case class Join(
      docId: String,
      userId: String,
      userName: String,
      color: String
  ) extends ClientMessage

  final case class Ops(ops: List[Op]) extends ClientMessage

  final case class Cursor(
      index: Int,
      selectionEnd: Int
  ) extends ClientMessage

  final case class SetTitle(title: String) extends ClientMessage

  case object RequestHistory extends ClientMessage

  final case class RestoreVersion(versionId: String) extends ClientMessage

  case object SaveCheckpoint extends ClientMessage

/** Server → client */
sealed trait ServerMessage

object ServerMessage:
  final case class Welcome(
      snapshot: DocumentSnapshot,
      peers: List[Peer],
      yourUserId: String
  ) extends ServerMessage

  final case class PeerJoined(peer: Peer) extends ServerMessage

  final case class PeerLeft(userId: String) extends ServerMessage

  final case class RemoteOps(
      fromUserId: String,
      ops: List[Op],
      version: Long
  ) extends ServerMessage

  final case class Presence(
      peers: List[Peer]
  ) extends ServerMessage

  final case class TitleChanged(
      title: String,
      fromUserId: String
  ) extends ServerMessage

  final case class History(
      entries: List[VersionEntry]
  ) extends ServerMessage

  final case class Restored(
      snapshot: DocumentSnapshot
  ) extends ServerMessage

  final case class Error(message: String) extends ServerMessage

  final case class CheckpointSaved(entry: VersionEntry) extends ServerMessage
