package notes.server

import cats.effect.{IO, IOApp}
import com.comcast.ip4s.*
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.middleware.{CORS, Logger}

object Main extends IOApp.Simple:

  private val host: Host = host"0.0.0.0"
  private val port: Port = port"8090"

  def run: IO[Unit] =
    for
      hub <- RoomHub.create
      ws = new WsHandler(hub)
      httpApp = { (wsb: org.http4s.server.websocket.WebSocketBuilder2[IO]) =>
        val routes = new Routes(hub, ws).routes(wsb)
        Logger.httpApp(logHeaders = true, logBody = false)(
          CORS.policy.withAllowOriginAll(routes).orNotFound
        )
      }
      _ <- IO.println(s"Notes server listening on http://$host:$port")
      _ <- EmberServerBuilder
        .default[IO]
        .withHost(host)
        .withPort(port)
        .withHttpWebSocketApp(httpApp)
        .build
        .useForever
    yield ()
