package notes.server

import cats.effect.IO
import cats.effect.kernel.Ref

final class RoomHub(rooms: Ref[IO, Map[String, DocRoom]]):
  def getOrCreate(docId: String): IO[DocRoom] =
    rooms.get.flatMap { map =>
      map.get(docId) match
        case Some(room) => IO.pure(room)
        case None =>
          DocRoom.create(docId).flatMap { room =>
            rooms.modify { current =>
              current.get(docId) match
                case Some(existing) => (current, existing)
                case None           => (current + (docId -> room), room)
            }
          }
    }

object RoomHub:
  def create: IO[RoomHub] =
    Ref.of[IO, Map[String, DocRoom]](Map.empty).map(new RoomHub(_))
