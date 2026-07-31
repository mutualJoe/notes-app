package notes.server

import cats.effect.IO
import cats.effect.std.UUIDGen
import cats.syntax.all.*
import fs2.concurrent.Topic
import fs2.{Pipe, Stream}
import notes.shared.protocol.*
import notes.shared.protocol.Codecs.given
import notes.shared.util.Colors
import org.http4s.websocket.WebSocketFrame
import org.http4s.websocket.WebSocketFrame.{Close, Text}

final class WsHandler(hub: RoomHub):

  def handle(docId: String): IO[(Stream[IO, WebSocketFrame], Pipe[IO, WebSocketFrame, Unit])] =
    for
      topic <- Topic[IO, ServerMessage]
      userId <- UUIDGen.randomUUID[IO].map(_.toString.take(8))
      userName = Colors.randomName()
      color = Colors.forUser(userId)
      room <- hub.getOrCreate(docId)
      state <- cats.effect.Ref.of[IO, Option[String]](None)
    yield
      val send: Stream[IO, WebSocketFrame] =
        topic.subscribe(32).map(msg => Text(Codecs.encodeServer(msg)))

      val receive: Pipe[IO, WebSocketFrame, Unit] = _.evalMap {
        case Text(str, _) =>
          Codecs.decodeClient(str) match
            case Left(err) =>
              topic.publish1(ServerMessage.Error(err.getMessage)).void
            case Right(msg) =>
              handleClient(room, topic, userId, userName, color, state, msg)
                .handleErrorWith(e => topic.publish1(ServerMessage.Error(e.getMessage)).void)
        case Close(_) =>
          leave(room, state)
        case _ =>
          IO.unit
      }.onFinalize(leave(room, state))

      (send, receive)

  private def leave(room: DocRoom, state: cats.effect.Ref[IO, Option[String]]): IO[Unit] =
    state.get.flatMap {
      case None => IO.unit
      case Some(uid) =>
        state.set(None) *>
          room.leave(uid).flatMap {
            case Some(msg) => room.broadcast(exceptUserId = Some(uid), msg)
            case None      => IO.unit
          }
    }

  private def handleClient(
      room: DocRoom,
      topic: Topic[IO, ServerMessage],
      defaultUserId: String,
      defaultName: String,
      defaultColor: String,
      state: cats.effect.Ref[IO, Option[String]],
      msg: ClientMessage
  ): IO[Unit] =
    msg match
      case ClientMessage.Join(_, userId, userName, color) =>
        val uid = if userId.nonEmpty then userId else defaultUserId
        val name = if userName.nonEmpty then userName else defaultName
        val col = if color.nonEmpty then color else defaultColor
        val peer = Peer(uid, name, col)
        state.set(Some(uid)) *>
          room.join(peer, topic).flatMap { bundle =>
            topic.publish1(bundle.welcome).void *>
              room.broadcast(exceptUserId = Some(uid), bundle.announce)
          }

      case ClientMessage.Ops(ops) =>
        requireJoined(state).flatMap { uid =>
          room.applyOps(uid, ops).flatMap { remote =>
            room.broadcast(exceptUserId = Some(uid), remote)
          }
        }

      case ClientMessage.Cursor(index, selectionEnd) =>
        requireJoined(state).flatMap { uid =>
          room.updateCursor(uid, index, selectionEnd).flatMap { presence =>
            room.broadcast(exceptUserId = Some(uid), presence)
          }
        }

      case ClientMessage.SetTitle(title) =>
        requireJoined(state).flatMap { uid =>
          room.setTitle(uid, title).flatMap { msg =>
            room.broadcast(exceptUserId = None, msg)
          }
        }

      case ClientMessage.RequestHistory =>
        requireJoined(state) *>
          room.requestHistory.flatMap(topic.publish1(_).void)

      case ClientMessage.RestoreVersion(versionId) =>
        requireJoined(state) *>
          room.restore(versionId).flatMap {
            case Left(err) =>
              topic.publish1(ServerMessage.Error(err)).void
            case Right(restored) =>
              room.broadcast(exceptUserId = None, restored)
          }

      case ClientMessage.SaveCheckpoint =>
        requireJoined(state) *>
          room.saveCheckpoint().flatMap { saved =>
            topic.publish1(saved).void
          }

  private def requireJoined(state: cats.effect.Ref[IO, Option[String]]): IO[String] =
    state.get.flatMap {
      case Some(uid) => IO.pure(uid)
      case None      => IO.raiseError(new IllegalStateException("Join the document first"))
    }
