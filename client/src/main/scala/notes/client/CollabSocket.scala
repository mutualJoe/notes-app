package notes.client

import notes.shared.protocol.*
import notes.shared.protocol.Codecs.given
import org.scalajs.dom

import scala.scalajs.js

final class CollabSocket(
    docId: String,
    onMessage: ServerMessage => Unit,
    onOpen: () => Unit,
    onClose: () => Unit
):
  private var socket: Option[dom.WebSocket] = None

  def connect(userId: String, userName: String, color: String): Unit =
    disconnect()
    val proto = if dom.window.location.protocol == "https:" then "wss" else "ws"
    val url = s"$proto://${dom.window.location.host}/ws/$docId"
    val ws = new dom.WebSocket(url)
    socket = Some(ws)

    ws.onopen = (_: dom.Event) =>
      send(ClientMessage.Join(docId, userId, userName, color))
      onOpen()

    ws.onmessage = (ev: dom.MessageEvent) =>
      val data = ev.data.toString
      Codecs.decodeServer(data) match
        case Right(msg) => onMessage(msg)
        case Left(err)  => dom.console.error("Bad server message", err.toString, data)

    ws.onclose = (_: dom.CloseEvent) =>
      socket = None
      onClose()
      // simple reconnect
      js.timers.setTimeout(1200) {
        connect(userId, userName, color)
      }

    ws.onerror = (_: dom.Event) =>
      dom.console.error("WebSocket error")

  def disconnect(): Unit =
    socket.foreach { ws =>
      ws.onclose = (_: dom.CloseEvent) => ()
      ws.close()
    }
    socket = None

  def send(msg: ClientMessage): Unit =
    socket.foreach { ws =>
      if ws.readyState == dom.WebSocket.OPEN then
        ws.send(Codecs.encodeClient(msg))
    }

  def connected: Boolean = socket.exists(_.readyState == dom.WebSocket.OPEN)
