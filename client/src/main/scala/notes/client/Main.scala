package notes.client

import com.raquo.laminar.api.L.*
import com.raquo.laminar.codecs.StringAsIsCodec
import notes.shared.crdt.RgaDocument
import notes.shared.markdown.Markdown
import notes.shared.protocol.*
import notes.shared.util.Colors
import org.scalajs.dom
import org.scalajs.dom.HTMLTextAreaElement

import scala.scalajs.js
import scala.util.Random

object Main:

  def main(args: Array[String]): Unit =
    render(dom.document.getElementById("app"), App(docIdFromLocation))

  private val userId: String =
    Option(dom.window.localStorage.getItem("notes.userId")).getOrElse {
      val id = Random.alphanumeric.take(8).mkString
      dom.window.localStorage.setItem("notes.userId", id)
      id
    }

  private val userName: String =
    Option(dom.window.localStorage.getItem("notes.userName")).getOrElse {
      val name = Colors.randomName()
      dom.window.localStorage.setItem("notes.userName", name)
      name
    }

  private val userColor: String = Colors.forUser(userId)

  private def docIdFromLocation: String =
    Option(dom.window.location.hash).map(_.stripPrefix("#")).filter(_.nonEmpty).getOrElse {
      val id = "welcome"
      dom.window.location.hash = id
      id
    }

  final case class UiState(
      title: String = "Untitled note",
      text: String = "",
      peers: List[Peer] = Nil,
      connected: Boolean = false,
      dark: Boolean = false,
      historyOpen: Boolean = false,
      history: List[VersionEntry] = Nil,
      version: Long = 0,
      toast: Option[String] = None,
      applyingRemote: Boolean = false
  )

  def App(initialDocId: String): HtmlElement =
    val docVar = Var(RgaDocument.empty)
    val stateVar = Var(
      UiState(dark = Option(dom.window.localStorage.getItem("notes.theme")).contains("dark"))
    )
    val docIdVar = Var(initialDocId)
    val editorRef = Var[Option[HTMLTextAreaElement]](None)

    applyTheme(stateVar.now().dark)

    var socket: CollabSocket = null
    socket = new CollabSocket(
      docId = initialDocId,
      onMessage = msg => handleServer(msg, docVar, stateVar, editorRef),
      onOpen = () => stateVar.update(_.copy(connected = true)),
      onClose = () => stateVar.update(_.copy(connected = false))
    )

    dom.window.addEventListener(
      "hashchange",
      (_: dom.Event) => {
        val newId = docIdFromLocation
        if newId != docIdVar.now() then dom.window.location.reload()
      }
    )

    div(
      cls := "app-shell",
      onMountCallback(_ => socket.connect(userId, userName, userColor)),
      topBar(stateVar, () => socket),
      metaBar(stateVar, docIdVar),
      div(
        cls := "workspace",
        editorPanel(stateVar, docVar, editorRef, () => socket),
        previewPanel(stateVar)
      ),
      historyDrawer(stateVar, () => socket),
      div(
        cls <-- stateVar.signal.map(s => if s.historyOpen then "backdrop open" else "backdrop"),
        onClick --> (_ => stateVar.update(_.copy(historyOpen = false)))
      ),
      div(
        cls <-- stateVar.signal.map(s => if s.toast.isDefined then "toast show" else "toast"),
        child.text <-- stateVar.signal.map(_.toast.getOrElse(""))
      )
    )

  private def topBar(stateVar: Var[UiState], socket: () => CollabSocket): HtmlElement =
    headerTag(
      cls := "topbar",
      div(
        cls := "brand",
        div(cls := "brand-name", "Notes"),
        div(cls := "brand-sub", "Realtime collaborative markdown")
      ),
      input(
        cls := "title-input",
        typ := "text",
        controlled(
          value <-- stateVar.signal.map(_.title),
          onInput.mapToValue --> { title =>
            stateVar.update(_.copy(title = title))
            socket().send(ClientMessage.SetTitle(title))
          }
        )
      ),
      div(
        cls := "presence",
        span(cls := "presence-label", "Online"),
        span(cls <-- stateVar.signal.map(s => if s.connected then "status-dot" else "status-dot offline")),
        children <-- stateVar.signal.map { s =>
          val self = Peer(userId, userName, userColor)
          (self :: s.peers.filterNot(_.userId == userId)).map { p =>
            span(
              cls := "avatar",
              span(cls := "avatar-dot", backgroundColor := p.color),
              span(p.userName)
            )
          }
        }
      ),
      div(
        cls := "toolbar",
        button(
          cls := "btn",
          typ := "button",
          child.text <-- stateVar.signal.map(s => if s.dark then "Light" else "Dark"),
          onClick --> { _ =>
            val next = !stateVar.now().dark
            stateVar.update(_.copy(dark = next))
            applyTheme(next)
          }
        ),
        button(
          cls := "btn",
          typ := "button",
          "History",
          onClick --> { _ =>
            socket().send(ClientMessage.RequestHistory)
            stateVar.update(_.copy(historyOpen = true))
          }
        ),
        button(
          cls := "btn",
          typ := "button",
          "Checkpoint",
          onClick --> (_ => socket().send(ClientMessage.SaveCheckpoint))
        ),
        button(
          cls := "btn",
          typ := "button",
          "Export HTML",
          onClick --> (_ => exportHtml(stateVar.now()))
        ),
        button(
          cls := "btn btn-primary",
          typ := "button",
          "Export PDF",
          onClick --> (_ => dom.window.print())
        )
      )
    )

  private def metaBar(stateVar: Var[UiState], docIdVar: Var[String]): HtmlElement =
    div(
      cls := "meta-bar",
      span(child.text <-- docIdVar.signal.map(id => s"Document: $id")),
      span(child.text <-- stateVar.signal.map(s => s"Version: ${s.version}")),
      span(s"You: $userName"),
      span(
        a(
          href := "#",
          "New document",
          onClick.preventDefault --> { _ =>
            dom.window.location.hash = Random.alphanumeric.take(8).mkString
          }
        )
      )
    )

  private def editorPanel(
      stateVar: Var[UiState],
      docVar: Var[RgaDocument],
      editorRef: Var[Option[HTMLTextAreaElement]],
      socket: () => CollabSocket
  ): HtmlElement =
    div(
      cls := "panel",
      div(cls := "panel-header", span("Editor"), span("CRDT · live")),
      div(
        cls := "editor-wrap",
        textArea(
          cls := "editor",
          spellCheck := true,
          onMountCallback(ctx => editorRef.set(Some(ctx.thisNode.ref))),
          controlled(
            value <-- stateVar.signal.map(_.text),
            onInput.mapToValue --> { value =>
              if !stateVar.now().applyingRemote then
                val doc = docVar.now()
                val ops = TextDiff.applyChange(doc, userId, value)
                docVar.set(doc)
                stateVar.update(_.copy(text = doc.text))
                if ops.nonEmpty then socket().send(ClientMessage.Ops(ops))
            }
          ),
          onSelect --> (_ => sendCursor(editorRef, socket)),
          onClick --> (_ => sendCursor(editorRef, socket)),
          onKeyUp --> (_ => sendCursor(editorRef, socket)),
          onScroll --> { ev =>
            val ta = ev.target.asInstanceOf[HTMLTextAreaElement]
            Option(dom.document.querySelector(".editor-mirror")).foreach { mirror =>
              val el = mirror.asInstanceOf[dom.HTMLElement]
              el.scrollTop = ta.scrollTop
              el.scrollLeft = ta.scrollLeft
            }
          }
        ),
        pre(
          cls := "editor-mirror",
          aria.hidden := true,
          children <-- stateVar.signal.map(s => mirrorNodes(s.text, s.peers.filterNot(_.userId == userId)))
        )
      )
    )

  private def mirrorNodes(text: String, peers: List[Peer]): List[Node] =
    // Transparent text for scroll parity + absolutely positioned remote carets
    val textNode: Node = span(text)
    val carets: List[Node] = peers.map { peer =>
      span(
        cls := "remote-caret",
        styleAttr := caretStyle(text, peer.cursor, peer.color),
        htmlAttr("data-name", StringAsIsCodec) := peer.userName
      )
    }
    textNode :: carets

  private def caretStyle(text: String, cursor: Int, color: String): String =
    val idx = cursor.max(0).min(text.length)
    val before = text.take(idx)
    val lines = before.split("\n", -1)
    val row = lines.length - 1
    val col = lines.lastOption.map(_.length).getOrElse(0)
    val lineHeight = 1.617 // 0.98rem * 1.65
    val top = 1.0 + row * lineHeight
    val left = 1.1 + col * 0.589
    s"top:${top}rem;left:${left}rem;height:${lineHeight}rem;--caret-color:$color;"

  private def previewPanel(stateVar: Var[UiState]): HtmlElement =
    div(
      cls := "panel",
      div(cls := "panel-header", span("Preview"), span("Markdown")),
      div(
        cls := "preview",
        onMountBind { ctx =>
          stateVar.signal.map(_.text) --> { md =>
            ctx.thisNode.ref.innerHTML = Markdown.toHtml(md)
          }
        }
      )
    )

  private def historyDrawer(stateVar: Var[UiState], socket: () => CollabSocket): HtmlElement =
    asideTag(
      cls <-- stateVar.signal.map(s => if s.historyOpen then "side-drawer open" else "side-drawer"),
      div(
        cls := "drawer-header",
        span("Version history"),
        button(
          cls := "btn btn-ghost",
          typ := "button",
          "Close",
          onClick --> (_ => stateVar.update(_.copy(historyOpen = false)))
        )
      ),
      div(
        cls := "drawer-body",
        children <-- stateVar.signal.map { s =>
          if s.history.isEmpty then
            List(div(color := "var(--ink-muted)", "No checkpoints yet."))
          else
            s.history.map { entry =>
              button(
                cls := "version-item",
                typ := "button",
                div(cls := "version-label", entry.label),
                div(cls := "version-meta", formatTime(entry.createdAt) + s" · v${entry.version}"),
                div(cls := "version-preview", entry.textPreview),
                onClick --> { _ =>
                  socket().send(ClientMessage.RestoreVersion(entry.id))
                  stateVar.update(_.copy(historyOpen = false))
                  showToast(stateVar, s"Restoring ${entry.label}…")
                }
              )
            }
        }
      )
    )

  private def handleServer(
      msg: ServerMessage,
      docVar: Var[RgaDocument],
      stateVar: Var[UiState],
      editorRef: Var[Option[HTMLTextAreaElement]]
  ): Unit =
    msg match
      case ServerMessage.Welcome(snapshot, peers, _) =>
        val doc = RgaDocument.fromSnapshot(snapshot.atoms)
        docVar.set(doc)
        stateVar.update(
          _.copy(
            title = snapshot.title,
            text = doc.text,
            peers = peers,
            version = snapshot.version
          )
        )

      case ServerMessage.PeerJoined(peer) =>
        stateVar.update(s => s.copy(peers = peer :: s.peers.filterNot(_.userId == peer.userId)))

      case ServerMessage.PeerLeft(uid) =>
        stateVar.update(s => s.copy(peers = s.peers.filterNot(_.userId == uid)))

      case ServerMessage.RemoteOps(_, ops, version) =>
        val doc = docVar.now()
        val ta = editorRef.now()
        val cursor = ta.map(_.selectionStart).getOrElse(0)
        doc.applyAll(ops)
        docVar.set(doc)
        stateVar.update(_.copy(text = doc.text, version = version, applyingRemote = true))
        js.timers.setTimeout(0) {
          stateVar.update(_.copy(applyingRemote = false))
          ta.foreach { el =>
            val pos = math.min(cursor, doc.text.length)
            el.selectionStart = pos
            el.selectionEnd = pos
          }
        }

      case ServerMessage.Presence(peers) =>
        stateVar.update(_.copy(peers = peers.filterNot(_.userId == userId)))

      case ServerMessage.TitleChanged(title, from) =>
        if from != userId then stateVar.update(_.copy(title = title))

      case ServerMessage.History(entries) =>
        stateVar.update(_.copy(history = entries))

      case ServerMessage.Restored(snapshot) =>
        val doc = RgaDocument.fromSnapshot(snapshot.atoms)
        docVar.set(doc)
        stateVar.update(
          _.copy(
            title = snapshot.title,
            text = doc.text,
            version = snapshot.version,
            applyingRemote = true
          )
        )
        js.timers.setTimeout(0) {
          stateVar.update(_.copy(applyingRemote = false))
          showToast(stateVar, "Document restored")
        }

      case ServerMessage.CheckpointSaved(entry) =>
        stateVar.update(s => s.copy(history = entry :: s.history.filterNot(_.id == entry.id)))
        showToast(stateVar, "Checkpoint saved")

      case ServerMessage.Error(message) =>
        showToast(stateVar, message)

  private def sendCursor(editorRef: Var[Option[HTMLTextAreaElement]], socket: () => CollabSocket): Unit =
    editorRef.now().foreach { el =>
      socket().send(ClientMessage.Cursor(el.selectionStart, el.selectionEnd))
    }

  private def applyTheme(dark: Boolean): Unit =
    val theme = if dark then "dark" else "light"
    dom.document.documentElement.setAttribute("data-theme", theme)
    dom.window.localStorage.setItem("notes.theme", theme)

  private def exportHtml(state: UiState): Unit =
    val html = Markdown.toStaticSite(state.title, state.text)
    val blob = new dom.Blob(js.Array(html), new dom.BlobPropertyBag { `type` = "text/html" })
    val url = dom.URL.createObjectURL(blob)
    val a = dom.document.createElement("a").asInstanceOf[dom.HTMLAnchorElement]
    a.href = url
    a.download = sanitizeFilename(state.title) + ".html"
    a.click()
    dom.URL.revokeObjectURL(url)

  private def sanitizeFilename(name: String): String =
    val cleaned = name.trim.replaceAll("""[^\w\-]+""", "-").stripPrefix("-").stripSuffix("-")
    if cleaned.isEmpty then "note" else cleaned.take(60)

  private def showToast(stateVar: Var[UiState], message: String): Unit =
    stateVar.update(_.copy(toast = Some(message)))
    js.timers.setTimeout(2200) {
      stateVar.update(s => if s.toast.contains(message) then s.copy(toast = None) else s)
    }

  private def formatTime(epochMs: Long): String =
    new js.Date(epochMs.toDouble).toLocaleString()
