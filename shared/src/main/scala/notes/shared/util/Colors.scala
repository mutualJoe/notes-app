package notes.shared.util

object Colors:
  private val palette: Vector[String] = Vector(
    "#e11d48", // rose
    "#ea580c", // orange
    "#ca8a04", // yellow
    "#16a34a", // green
    "#0891b2", // cyan
    "#2563eb", // blue
    "#7c3aed", // violet
    "#db2777"  // pink
  )

  def forUser(userId: String): String =
    val hash = userId.foldLeft(0)((h, c) => 31 * h + c)
    palette(math.abs(hash) % palette.length)

  def randomName(): String =
    val adjectives = List("Amber", "Brave", "Calm", "Dusk", "Eager", "Fresh", "Golden", "Humble")
    val nouns = List("Fox", "Heron", "Cedar", "River", "Sparrow", "Maple", "Otter", "Quill")
    val a = adjectives((math.random() * adjectives.length).toInt)
    val n = nouns((math.random() * nouns.length).toInt)
    s"$a $n"
