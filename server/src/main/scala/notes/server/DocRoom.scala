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
 * Collaborative document room with SQLite-backed persistence.
 * Presence stays in-memory; document content and version history are saved.
 */
final class DocRoom private (
    val docId: String,
    private var title: String,
    private val document: RgaDocument,
    private val versionCounter: AtomicLong,
    private val store: NoteStore,
    private val history: mutable.ArrayBuffer[VersionRecord]
):
  private val lock = new Object
  private val peers = mutable.Map.empty[String, ConnectedPeer]
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
    val result =
      IO {
        lock.synchronized {
          document.applyAll(ops)
          val v = versionCounter.incrementAndGet()
          opsSinceCheckpoint += ops.size
          val autoCheckpoint =
            if opsSinceCheckpoint >= 40 then
              opsSinceCheckpoint = 0
              addCheckpointUnsafe(s"Auto-save v$v")
              Some(history.last)
            else None
          (ServerMessage.RemoteOps(fromUserId, ops, v), persistedUnsafe, autoCheckpoint)
        }
      }
    result.flatMap { case (remote, persisted, checkpoint) =>
      persistDocument(persisted) *>
        checkpoint.traverse_(persistCheckpoint) *>
        IO.pure(remote)
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
    val result =
      IO {
        lock.synchronized {
          title = newTitle
          (ServerMessage.TitleChanged(newTitle, fromUserId), persistedUnsafe)
        }
      }
    result.flatMap { case (msg, persisted) =>
      persistDocument(persisted).as(msg)
    }

  def requestHistory: IO[ServerMessage.History] =
    IO {
      lock.synchronized(ServerMessage.History(history.map(_.entry).toList.reverse))
    }

  def saveCheckpoint(label: Option[String] = None): IO[ServerMessage.CheckpointSaved] =
    val result =
      IO {
        lock.synchronized {
          val entry = addCheckpointUnsafe(label.getOrElse(s"Checkpoint v${currentVersion}"))
          val record = history.last
          (ServerMessage.CheckpointSaved(entry), persistedUnsafe, record)
        }
      }
    result.flatMap { case (msg, persisted, record) =>
      persistDocument(persisted) *> persistCheckpoint(record).as(msg)
    }

  def restore(versionId: String): IO[Either[String, ServerMessage.Restored]] =
    val result =
      IO {
        lock.synchronized {
          history.find(_.entry.id == versionId) match
            case None => Left("unknown")
            case Some(rec) =>
              document.replaceWith(rec.atoms)
              title = rec.title
              val v = versionCounter.incrementAndGet()
              val checkpoint = addCheckpointUnsafe(s"Restored ${rec.entry.label}")
              val record = history.last
              Right(
                (
                  ServerMessage.Restored(DocumentSnapshot(docId, title, document.snapshot, v)),
                  persistedUnsafe,
                  record
                )
              )
        }
      }
    result.flatMap {
      case Left(_) => IO.pure(Left(s"Unknown version: $versionId"))
      case Right((restored, persisted, record)) =>
        persistDocument(persisted) *> persistCheckpoint(record).as(Right(restored))
    }

  def broadcast(exceptUserId: Option[String], msg: ServerMessage): IO[Unit] =
    val targets = lock.synchronized {
      peers.values.toList.filterNot(p => exceptUserId.contains(p.peer.userId))
    }
    targets.traverse_(_.outbound.publish1(msg).void)

  def sendTo(userId: String, msg: ServerMessage): IO[Unit] =
    val target = lock.synchronized(peers.get(userId))
    target.traverse_(_.outbound.publish1(msg).void)

  private def snapshotUnsafe: DocumentSnapshot =
    DocumentSnapshot(docId, title, document.snapshot, currentVersion)

  private def peerListUnsafe: List[Peer] = peers.values.map(_.peer).toList

  private def persistedUnsafe: PersistedDocument =
    PersistedDocument(docId, title, document.snapshot, currentVersion)

  private def addCheckpointUnsafe(label: String): VersionEntry =
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
    while history.size > DocRoom.MaxHistory do history.remove(0)
    entry

  private def persistDocument(doc: PersistedDocument): IO[Unit] =
    store.saveDocument(doc)

  private def persistCheckpoint(record: VersionRecord): IO[Unit] =
    store.saveVersion(docId, record) *> store.trimVersions(docId, DocRoom.MaxHistory)

object DocRoom:
  private val MaxHistory = 50

  def create(docId: String, store: NoteStore, initialMarkdown: String = defaultMarkdown): IO[DocRoom] =
    for
      existing <- store.loadDocument(docId)
      room <- existing match
        case Some(doc) =>
          store.loadVersions(docId).map { versions =>
            val history = mutable.ArrayBuffer.from(versions)
            new DocRoom(
              docId = doc.docId,
              title = doc.title,
              document = RgaDocument.fromSnapshot(doc.atoms),
              versionCounter = new AtomicLong(doc.version),
              store = store,
              history = history
            )
          }
        case None =>
          IO {
            val document =
              if initialMarkdown.nonEmpty then RgaDocument.fromText("system", initialMarkdown)
              else RgaDocument.empty
            val room = new DocRoom(
              docId = docId,
              title = "Untitled note",
              document = document,
              versionCounter = new AtomicLong(1L),
              store = store,
              history = mutable.ArrayBuffer.empty
            )
            room.addCheckpointUnsafe("Initial version")
            room
          }.flatMap { room =>
            val persisted = PersistedDocument(docId, room.title, room.document.snapshot, room.currentVersion)
            val initial = room.history.last
            store.saveDocument(persisted) *> store.saveVersion(docId, initial).as(room)
          }
    yield room

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
