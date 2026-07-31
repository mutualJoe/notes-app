# Notes — Real-time Collaborative Markdown

Cross-compiled Scala 3 app with:

- **Shared CRDT** (`shared`) — RGA document model + WebSocket protocol (Circe)
- **http4s WebSocket backend** (`server`) — rooms, presence, version history, HTML export, SQLite persistence
- **Scala.js + Laminar frontend** (`client`) — live cursors, presence, Markdown preview, dark mode, PDF/HTML export

## Run

```bash
sbt "server/run"
```

Open [http://localhost:8090/#welcome](http://localhost:8090/#welcome).  
Open a second tab to collaborate. Use **New document** for a fresh shared note.

Documents and version history are stored in `data/notes.db` (created on first run). Restarting the server keeps your notes.

## Modules

| Module   | Role |
|----------|------|
| `shared` | RGA CRDT, ops, protocol codecs, Markdown → HTML |
| `server` | Ember/http4s HTTP + WebSocket, SQLite, static assets |
| `client` | Laminar UI compiled to `main.js` (copied into server resources) |

## Features

- Concurrent editing via CRDT ops over `/ws/:docId`
- Presence avatars + remote caret indicators
- Live Markdown preview
- Dark / light theme (persisted)
- Version history with restore + manual checkpoints
- SQLite persistence (`data/notes.db`) for documents and history
- Export static HTML site, or **Export PDF** via print

## Tests

```bash
sbt sharedJVM/test
```
