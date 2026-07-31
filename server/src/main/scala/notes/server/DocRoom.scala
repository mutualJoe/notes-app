package notes.server

import cats.effect.IO
import cats.syntax.all.*
import fs2.concurrent.Topic
import notes.shared.crdt.RgaDocument
import notes.shared.model.{Atom, Op}
import notes.shared.protocol.*

import java.util.concurrent.atomic.AtomicLong
import scala.collection.mutable

final case class VersionRecord(
    entry: VersionEntry,
    atoms: Vector[Atom],
    title: String
)

final case class ConnectedPeer(
    peer: Peer,
    outbound: Topic[IO, ServerMessage]
)

final case class WelcomeBundle(
    welcome: ServerMessage.Welcome,
    announce: ServerMessage.PeerJoined
)

/**
 * In-memory collaborative document room.
 * One room per docId; fans out ops / presence over WebSocket topics.
 */
final class DocRoom private (
    val docId: String,
    private var title: String,
    private val document: RgaDocument,
    private val versionCounter: AtomicLong
):
  private val lock = new Object
  private val peers = mutable.Map.empty[String, ConnectedPeer]
  private val history = mutable.ArrayBuffer.empty[VersionRecord]
  private var opsSinceCheckpoint = 0

  def currentVersion: Long = versionCounter.get()

  def snapshot: DocumentSnapshot =
    lock.synchronized {
      DocumentSnapshot(docId, title, document.snapshot, currentVersion)
    }

  def peerList: List[Peer] = lock.synchronized(peers.values.map(_.peer).toList)

  def join(peer: Peer, outbound: Topic[IO, ServerMessage]): IO[WelcomeBundle] =
    IO {
      lock.synchronized {
        peers += peer.userId -> ConnectedPeer(peer, outbound)
        WelcomeBundle(
          ServerMessage.Welcome(snapshotUnsafe, peerListUnsafe.filterNot(_.userId == peer.userId), peer.userId),
          ServerMessage.PeerJoined(peer)
        )
      }
    }

  def leave(userId: String): IO[Option[ServerMessage.PeerLeft]] =
    IO {
      lock.synchronized {
        peers.remove(userId).map(_ => ServerMessage.PeerLeft(userId))
      }
    }

  def applyOps(fromUserId: String, ops: List[Op]): IO[ServerMessage.RemoteOps] =
    IO {
      lock.synchronized {
        document.applyAll(ops)
        val v = versionCounter.incrementAndGet()
        opsSinceCheckpoint += ops.size
        if opsSinceCheckpoint >= 40 then
          addCheckpoint(s"Auto-save v$v")
          opsSinceCheckpoint = 0
        ServerMessage.RemoteOps(fromUserId, ops, v)
      }
    }

  def updateCursor(userId: String, index: Int, selectionEnd: Int): IO[ServerMessage.Presence] =
    IO {
      lock.synchronized {
        peers.get(userId).foreach { cp =>
          peers(userId) = cp.copy(peer = cp.peer.copy(cursor = index, selectionEnd = selectionEnd))
        }
        ServerMessage.Presence(peerListUnsafe)
      }
    }

  def setTitle(fromUserId: String, newTitle: String): IO[ServerMessage.TitleChanged] =
    IO {
      lock.synchronized {
        title = newTitle
        ServerMessage.TitleChanged(newTitle, fromUserId)
      }
    }

  def requestHistory: IO[ServerMessage.History] =
    IO {
      lock.synchronized(ServerMessage.History(history.map(_.entry).toList.reverse))
    }

  def saveCheckpoint(label: Option[String] = None): IO[ServerMessage.CheckpointSaved] =
    IO {
      lock.synchronized {
        val entry = addCheckpoint(label.getOrElse(s"Checkpoint v${currentVersion}"))
        ServerMessage.CheckpointSaved(entry)
      }
    }

  def restore(versionId: String): IO[Either[String, ServerMessage.Restored]] =
    IO {
      lock.synchronized {
        history.find(_.entry.id == versionId) match
          case None => Left(s"Unknown version: $versionId")
          case Some(rec) =>
            document.replaceWith(rec.atoms)
            title = rec.title
            val v = versionCounter.incrementAndGet()
            addCheckpoint(s"Restored ${rec.entry.label}")
            Right(ServerMessage.Restored(DocumentSnapshot(docId, title, document.snapshot, v)))
      }
    }

  private def snapshotUnsafe: DocumentSnapshot =
    DocumentSnapshot(docId, title, document.snapshot, currentVersion)

  private def peerListUnsafe: List[Peer] = peers.values.map(_.peer).toList

  def broadcast(exceptUserId: Option[String], msg: ServerMessage): IO[Unit] =
    val targets = lock.synchronized {
      peers.values.toList.filterNot(p => exceptUserId.contains(p.peer.userId))
    }
    targets.traverse_(_.outbound.publish1(msg).void)

  def sendTo(userId: String, msg: ServerMessage): IO[Unit] =
    val target = lock.synchronized(peers.get(userId))
    target.traverse_(_.outbound.publish1(msg).void)

  private[server] def addCheckpoint(label: String): VersionEntry =
    val id = java.util.UUID.randomUUID().toString
    val text = document.text
    val preview = if text.length > 80 then text.take(77) + "..." else text
    val entry = VersionEntry(
      id = id,
      version = currentVersion,
      label = label,
      createdAt = System.currentTimeMillis(),
      textPreview = preview.replace('\n', ' ')
    )
    history += VersionRecord(entry, document.snapshot, title)
    if history.size > 50 then history.remove(0)
    entry

object DocRoom:
  def create(docId: String, initialMarkdown: String = defaultMarkdown): IO[DocRoom] =
    IO {
      val doc =
        if initialMarkdown.nonEmpty then RgaDocument.fromText("system", initialMarkdown)
        else RgaDocument.empty
      val room = new DocRoom(docId, "Untitled note", doc, new AtomicLong(1L))
      room.addCheckpoint("Initial version")
      room
    }

  val defaultMarkdown: String =
    """# Welcome to Notes
      |
      |A **real-time** collaborative Markdown editor built with Scala, CRDTs, and WebSockets.
      |
      |## Try it
      |
      |- Open this URL in another tab or browser
      |- Edit together — cursors and presence update live
      |- Toggle dark mode, browse version history, export HTML or print to PDF
      |
      |```scala
      |println("Hello, collaborators!")
      |```
      |
      |> Tip: share the document id in the URL hash.
      |""".stripMargin
