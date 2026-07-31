package notes.shared.protocol

import io.circe.*
import io.circe.generic.semiauto.*
import io.circe.syntax.*
import notes.shared.model.{Atom, CharId, Op}

object Codecs:
  given Encoder[CharId] = deriveEncoder
  given Decoder[CharId] = deriveDecoder

  given Encoder[Atom] = deriveEncoder
  given Decoder[Atom] = deriveDecoder

  given Encoder[Op.Insert] = deriveEncoder
  given Decoder[Op.Insert] = deriveDecoder
  given Encoder[Op.Delete] = deriveEncoder
  given Decoder[Op.Delete] = deriveDecoder

  given Encoder[Op] = Encoder.instance {
    case i: Op.Insert => Json.obj("type" -> Json.fromString("insert")).deepMerge(i.asJson)
    case d: Op.Delete => Json.obj("type" -> Json.fromString("delete")).deepMerge(d.asJson)
  }

  given Decoder[Op] = Decoder.instance { cursor =>
    cursor.get[String]("type").flatMap {
      case "insert" => cursor.as[Op.Insert]
      case "delete" => cursor.as[Op.Delete]
      case other    => Left(DecodingFailure(s"Unknown op type: $other", cursor.history))
    }
  }

  given Encoder[Peer] = deriveEncoder
  given Decoder[Peer] = deriveDecoder

  given Encoder[DocumentSnapshot] = deriveEncoder
  given Decoder[DocumentSnapshot] = deriveDecoder

  given Encoder[VersionEntry] = deriveEncoder
  given Decoder[VersionEntry] = deriveDecoder

  given Encoder[ClientMessage.Join] = deriveEncoder
  given Decoder[ClientMessage.Join] = deriveDecoder
  given Encoder[ClientMessage.Ops] = deriveEncoder
  given Decoder[ClientMessage.Ops] = deriveDecoder
  given Encoder[ClientMessage.Cursor] = deriveEncoder
  given Decoder[ClientMessage.Cursor] = deriveDecoder
  given Encoder[ClientMessage.SetTitle] = deriveEncoder
  given Decoder[ClientMessage.SetTitle] = deriveDecoder
  given Encoder[ClientMessage.RestoreVersion] = deriveEncoder
  given Decoder[ClientMessage.RestoreVersion] = deriveDecoder

  given Encoder[ClientMessage] = Encoder.instance {
    case m: ClientMessage.Join           => Json.obj("type" -> "join".asJson).deepMerge(m.asJson)
    case m: ClientMessage.Ops            => Json.obj("type" -> "ops".asJson).deepMerge(m.asJson)
    case m: ClientMessage.Cursor         => Json.obj("type" -> "cursor".asJson).deepMerge(m.asJson)
    case m: ClientMessage.SetTitle       => Json.obj("type" -> "setTitle".asJson).deepMerge(m.asJson)
    case ClientMessage.RequestHistory    => Json.obj("type" -> "requestHistory".asJson)
    case m: ClientMessage.RestoreVersion => Json.obj("type" -> "restoreVersion".asJson).deepMerge(m.asJson)
    case ClientMessage.SaveCheckpoint    => Json.obj("type" -> "saveCheckpoint".asJson)
  }

  given Decoder[ClientMessage] = Decoder.instance { cursor =>
    cursor.get[String]("type").flatMap {
      case "join"            => cursor.as[ClientMessage.Join]
      case "ops"             => cursor.as[ClientMessage.Ops]
      case "cursor"          => cursor.as[ClientMessage.Cursor]
      case "setTitle"        => cursor.as[ClientMessage.SetTitle]
      case "requestHistory"  => Right(ClientMessage.RequestHistory)
      case "restoreVersion"  => cursor.as[ClientMessage.RestoreVersion]
      case "saveCheckpoint"  => Right(ClientMessage.SaveCheckpoint)
      case other             => Left(DecodingFailure(s"Unknown client message: $other", cursor.history))
    }
  }

  given Encoder[ServerMessage.Welcome] = deriveEncoder
  given Decoder[ServerMessage.Welcome] = deriveDecoder
  given Encoder[ServerMessage.PeerJoined] = deriveEncoder
  given Decoder[ServerMessage.PeerJoined] = deriveDecoder
  given Encoder[ServerMessage.PeerLeft] = deriveEncoder
  given Decoder[ServerMessage.PeerLeft] = deriveDecoder
  given Encoder[ServerMessage.RemoteOps] = deriveEncoder
  given Decoder[ServerMessage.RemoteOps] = deriveDecoder
  given Encoder[ServerMessage.Presence] = deriveEncoder
  given Decoder[ServerMessage.Presence] = deriveDecoder
  given Encoder[ServerMessage.TitleChanged] = deriveEncoder
  given Decoder[ServerMessage.TitleChanged] = deriveDecoder
  given Encoder[ServerMessage.History] = deriveEncoder
  given Decoder[ServerMessage.History] = deriveDecoder
  given Encoder[ServerMessage.Restored] = deriveEncoder
  given Decoder[ServerMessage.Restored] = deriveDecoder
  given Encoder[ServerMessage.Error] = deriveEncoder
  given Decoder[ServerMessage.Error] = deriveDecoder
  given Encoder[ServerMessage.CheckpointSaved] = deriveEncoder
  given Decoder[ServerMessage.CheckpointSaved] = deriveDecoder

  given Encoder[ServerMessage] = Encoder.instance {
    case m: ServerMessage.Welcome         => Json.obj("type" -> "welcome".asJson).deepMerge(m.asJson)
    case m: ServerMessage.PeerJoined      => Json.obj("type" -> "peerJoined".asJson).deepMerge(m.asJson)
    case m: ServerMessage.PeerLeft        => Json.obj("type" -> "peerLeft".asJson).deepMerge(m.asJson)
    case m: ServerMessage.RemoteOps       => Json.obj("type" -> "remoteOps".asJson).deepMerge(m.asJson)
    case m: ServerMessage.Presence        => Json.obj("type" -> "presence".asJson).deepMerge(m.asJson)
    case m: ServerMessage.TitleChanged    => Json.obj("type" -> "titleChanged".asJson).deepMerge(m.asJson)
    case m: ServerMessage.History         => Json.obj("type" -> "history".asJson).deepMerge(m.asJson)
    case m: ServerMessage.Restored        => Json.obj("type" -> "restored".asJson).deepMerge(m.asJson)
    case m: ServerMessage.Error           => Json.obj("type" -> "error".asJson).deepMerge(m.asJson)
    case m: ServerMessage.CheckpointSaved => Json.obj("type" -> "checkpointSaved".asJson).deepMerge(m.asJson)
  }

  given Decoder[ServerMessage] = Decoder.instance { cursor =>
    cursor.get[String]("type").flatMap {
      case "welcome"         => cursor.as[ServerMessage.Welcome]
      case "peerJoined"      => cursor.as[ServerMessage.PeerJoined]
      case "peerLeft"        => cursor.as[ServerMessage.PeerLeft]
      case "remoteOps"       => cursor.as[ServerMessage.RemoteOps]
      case "presence"        => cursor.as[ServerMessage.Presence]
      case "titleChanged"    => cursor.as[ServerMessage.TitleChanged]
      case "history"         => cursor.as[ServerMessage.History]
      case "restored"        => cursor.as[ServerMessage.Restored]
      case "error"           => cursor.as[ServerMessage.Error]
      case "checkpointSaved" => cursor.as[ServerMessage.CheckpointSaved]
      case other             => Left(DecodingFailure(s"Unknown server message: $other", cursor.history))
    }
  }

  def encodeClient(msg: ClientMessage): String = msg.asJson.noSpaces
  def decodeClient(json: String): Either[Error, ClientMessage] =
    parser.decode[ClientMessage](json)

  def encodeServer(msg: ServerMessage): String = msg.asJson.noSpaces
  def decodeServer(json: String): Either[Error, ServerMessage] =
    parser.decode[ServerMessage](json)
