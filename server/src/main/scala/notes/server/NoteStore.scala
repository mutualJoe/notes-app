package notes.server

import cats.effect.{IO, Resource}
import io.circe.parser.decode
import io.circe.syntax.*
import notes.shared.model.Atom
import notes.shared.protocol.Codecs.given
import notes.shared.protocol.VersionEntry

import java.nio.file.{Files, Path}
import java.sql.{Connection, DriverManager}

final case class PersistedDocument(
    docId: String,
    title: String,
    atoms: Vector[Atom],
    version: Long
)

/** SQLite-backed persistence for documents and version history. */
final class NoteStore(private val conn: Connection):

  def migrate: IO[Unit] = IO.blocking {
    conn.synchronized {
      val st = conn.createStatement()
      try
        st.execute(
          """CREATE TABLE IF NOT EXISTS documents (
            |  doc_id TEXT PRIMARY KEY,
            |  title TEXT NOT NULL,
            |  atoms_json TEXT NOT NULL,
            |  version INTEGER NOT NULL,
            |  updated_at INTEGER NOT NULL
            |)""".stripMargin
        )
        st.execute(
          """CREATE TABLE IF NOT EXISTS versions (
            |  id TEXT PRIMARY KEY,
            |  doc_id TEXT NOT NULL,
            |  version INTEGER NOT NULL,
            |  label TEXT NOT NULL,
            |  created_at INTEGER NOT NULL,
            |  title TEXT NOT NULL,
            |  atoms_json TEXT NOT NULL,
            |  text_preview TEXT NOT NULL,
            |  FOREIGN KEY (doc_id) REFERENCES documents(doc_id)
            |)""".stripMargin
        )
        st.execute(
          "CREATE INDEX IF NOT EXISTS idx_versions_doc ON versions(doc_id, created_at DESC)"
        )
      finally st.close()
    }
  }

  def loadDocument(docId: String): IO[Option[PersistedDocument]] = IO.blocking {
    conn.synchronized {
      val ps = conn.prepareStatement(
        "SELECT title, atoms_json, version FROM documents WHERE doc_id = ?"
      )
      try
        ps.setString(1, docId)
        val rs = ps.executeQuery()
        try
          if !rs.next() then None
          else
            val atoms = decode[Vector[Atom]](rs.getString("atoms_json")) match
              case Right(a) => a
              case Left(err) =>
                throw new IllegalStateException(s"Corrupt atoms for $docId: $err")
            Some(
              PersistedDocument(
                docId = docId,
                title = rs.getString("title"),
                atoms = atoms,
                version = rs.getLong("version")
              )
            )
        finally rs.close()
      finally ps.close()
    }
  }

  def saveDocument(doc: PersistedDocument): IO[Unit] = IO.blocking {
    conn.synchronized {
      val ps = conn.prepareStatement(
        """INSERT INTO documents(doc_id, title, atoms_json, version, updated_at)
          |VALUES (?, ?, ?, ?, ?)
          |ON CONFLICT(doc_id) DO UPDATE SET
          |  title = excluded.title,
          |  atoms_json = excluded.atoms_json,
          |  version = excluded.version,
          |  updated_at = excluded.updated_at""".stripMargin
      )
      try
        ps.setString(1, doc.docId)
        ps.setString(2, doc.title)
        ps.setString(3, doc.atoms.asJson.noSpaces)
        ps.setLong(4, doc.version)
        ps.setLong(5, System.currentTimeMillis())
        ps.executeUpdate()
        ()
      finally ps.close()
    }
  }

  def loadVersions(docId: String): IO[List[VersionRecord]] = IO.blocking {
    conn.synchronized {
      val ps = conn.prepareStatement(
        """SELECT id, version, label, created_at, title, atoms_json, text_preview
          |FROM versions WHERE doc_id = ?
          |ORDER BY created_at ASC""".stripMargin
      )
      try
        ps.setString(1, docId)
        val rs = ps.executeQuery()
        try
          val buf = List.newBuilder[VersionRecord]
          while rs.next() do
            val atoms = decode[Vector[Atom]](rs.getString("atoms_json")) match
              case Right(a)  => a
              case Left(err) => throw new IllegalStateException(s"Corrupt version atoms: $err")
            val entry = VersionEntry(
              id = rs.getString("id"),
              version = rs.getLong("version"),
              label = rs.getString("label"),
              createdAt = rs.getLong("created_at"),
              textPreview = rs.getString("text_preview")
            )
            buf += VersionRecord(entry, atoms, rs.getString("title"))
          buf.result()
        finally rs.close()
      finally ps.close()
    }
  }

  def saveVersion(docId: String, record: VersionRecord): IO[Unit] = IO.blocking {
    conn.synchronized {
      val ps = conn.prepareStatement(
        """INSERT OR REPLACE INTO versions
          |(id, doc_id, version, label, created_at, title, atoms_json, text_preview)
          |VALUES (?, ?, ?, ?, ?, ?, ?, ?)""".stripMargin
      )
      try
        ps.setString(1, record.entry.id)
        ps.setString(2, docId)
        ps.setLong(3, record.entry.version)
        ps.setString(4, record.entry.label)
        ps.setLong(5, record.entry.createdAt)
        ps.setString(6, record.title)
        ps.setString(7, record.atoms.asJson.noSpaces)
        ps.setString(8, record.entry.textPreview)
        ps.executeUpdate()
        ()
      finally ps.close()
    }
  }

  /** Keep only the newest `keep` versions for a document. */
  def trimVersions(docId: String, keep: Int): IO[Unit] = IO.blocking {
    conn.synchronized {
      val ps = conn.prepareStatement(
        """DELETE FROM versions WHERE doc_id = ? AND id NOT IN (
          |  SELECT id FROM versions WHERE doc_id = ?
          |  ORDER BY created_at DESC LIMIT ?
          |)""".stripMargin
      )
      try
        ps.setString(1, docId)
        ps.setString(2, docId)
        ps.setInt(3, keep)
        ps.executeUpdate()
        ()
      finally ps.close()
    }
  }

  private[server] def close(): Unit = conn.close()

object NoteStore:
  val DefaultPath: Path = Path.of("data", "notes.db")

  def open(path: Path = DefaultPath): Resource[IO, NoteStore] =
    Resource.make(connect(path))(store => IO.blocking(store.close()).void)

  private def connect(path: Path): IO[NoteStore] = IO.blocking {
    val parent = path.getParent
    if parent != null then Files.createDirectories(parent)
    Class.forName("org.sqlite.JDBC")
    val conn = DriverManager.getConnection(s"jdbc:sqlite:${path.toAbsolutePath}")
    conn.setAutoCommit(true)
    val st = conn.createStatement()
    try st.execute("PRAGMA foreign_keys = ON")
    finally st.close()
    new NoteStore(conn)
  }
