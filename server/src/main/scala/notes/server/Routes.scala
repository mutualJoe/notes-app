package notes.server

import cats.effect.IO
import notes.shared.crdt.RgaDocument
import notes.shared.markdown.Markdown
import notes.shared.protocol.Codecs.given
import io.circe.syntax.*
import org.http4s.*
import org.http4s.circe.CirceEntityEncoder.*
import org.http4s.dsl.io.*
import org.http4s.headers.`Content-Type`
import org.http4s.server.websocket.WebSocketBuilder2

final class Routes(hub: RoomHub, wsHandler: WsHandler):

  def routes(wsb: WebSocketBuilder2[IO]): HttpRoutes[IO] =
    HttpRoutes.of[IO] {
      case GET -> Root =>
        Static.indexHtml

      case GET -> Root / "main.js" =>
        Static.mainJs

      case GET -> Root / "main.js.map" =>
        Static.mainJsMap

      case GET -> Root / "styles.css" =>
        Static.stylesCss

      case GET -> Root / "ws" / docId =>
        wsHandler.handle(docId).flatMap { case (send, receive) =>
          wsb.build(send, receive)
        }

      case GET -> Root / "api" / "docs" / docId =>
        hub.getOrCreate(docId).flatMap { room =>
          Ok(room.snapshot.asJson)
        }

      case GET -> Root / "api" / "docs" / docId / "export" / "html" =>
        hub.getOrCreate(docId).flatMap { room =>
          val snap = room.snapshot
          val html = Markdown.toStaticSite(snap.title, RgaDocument.fromSnapshot(snap.atoms).text)
          Ok(html).map(_.withContentType(`Content-Type`(MediaType.text.html, Charset.`UTF-8`)))
        }

      case GET -> Root / "health" =>
        Ok("ok")
    }

object Static:
  private def resource(path: String): IO[Response[IO]] =
    val url = Option(getClass.getResource(path))
    url match
      case None => NotFound(s"Missing resource: $path")
      case Some(u) =>
        val bytes = fs2.io.readInputStream[IO](IO(u.openStream()), 64 * 1024, closeAfterUse = true)
        val media =
          if path.endsWith(".js") then MediaType.application.javascript
          else if path.endsWith(".map") then MediaType.application.`json`
          else if path.endsWith(".css") then MediaType.text.css
          else MediaType.text.html
        Ok(bytes).map(_.withContentType(`Content-Type`(media, Charset.`UTF-8`)))

  def indexHtml: IO[Response[IO]] = resource("/web/index.html")
  def mainJs: IO[Response[IO]] = resource("/web/main.js")
  def mainJsMap: IO[Response[IO]] = resource("/web/main.js.map").handleErrorWith(_ => NotFound())
  def stylesCss: IO[Response[IO]] = resource("/web/styles.css")
