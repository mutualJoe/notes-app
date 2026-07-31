package notes.shared.markdown

import scala.util.matching.Regex

/** Lightweight Markdown → HTML renderer shared by JVM export and Scala.js preview. */
object Markdown:

  def toHtml(source: String): String =
    val lines = source.replace("\r\n", "\n").split("\n", -1).toList
    val sb = new StringBuilder
    var i = 0
    var inCode = false
    var inUl = false
    var inOl = false
    var inBq = false

    def closeLists(): Unit =
      if inUl then { sb.append("</ul>\n"); inUl = false }
      if inOl then { sb.append("</ol>\n"); inOl = false }

    def closeBq(): Unit =
      if inBq then { sb.append("</blockquote>\n"); inBq = false }

    while i < lines.length do
      val line = lines(i)

      if line.startsWith("```") then
        closeLists(); closeBq()
        if inCode then
          sb.append("</code></pre>\n")
          inCode = false
        else
          val lang = line.drop(3).trim
          val cls = if lang.nonEmpty then s""" class="language-$lang"""" else ""
          sb.append(s"<pre><code$cls>")
          inCode = true
      else if inCode then
        sb.append(escape(line)).append('\n')
      else if line.trim.isEmpty then
        closeLists(); closeBq()
      else if line.startsWith("# ") then
        closeLists(); closeBq()
        sb.append("<h1>").append(formatInline(line.drop(2))).append("</h1>\n")
      else if line.startsWith("## ") then
        closeLists(); closeBq()
        sb.append("<h2>").append(formatInline(line.drop(3))).append("</h2>\n")
      else if line.startsWith("### ") then
        closeLists(); closeBq()
        sb.append("<h3>").append(formatInline(line.drop(4))).append("</h3>\n")
      else if line.startsWith("> ") then
        closeLists()
        if !inBq then { sb.append("<blockquote>\n"); inBq = true }
        sb.append("<p>").append(formatInline(line.drop(2))).append("</p>\n")
      else if line.matches("""\s*[-*]\s+.*""") then
        closeBq()
        if inOl then { sb.append("</ol>\n"); inOl = false }
        if !inUl then { sb.append("<ul>\n"); inUl = true }
        val item = line.replaceFirst("""\s*[-*]\s+""", "")
        sb.append("<li>").append(formatInline(item)).append("</li>\n")
      else if line.matches("""\s*\d+\.\s+.*""") then
        closeBq()
        if inUl then { sb.append("</ul>\n"); inUl = false }
        if !inOl then { sb.append("<ol>\n"); inOl = true }
        val item = line.replaceFirst("""\s*\d+\.\s+""", "")
        sb.append("<li>").append(formatInline(item)).append("</li>\n")
      else if line.matches("""-{3,}""") || line.matches("""\*{3,}""") then
        closeLists(); closeBq()
        sb.append("<hr/>\n")
      else
        closeLists(); closeBq()
        sb.append("<p>").append(formatInline(line)).append("</p>\n")

      i += 1

    closeLists(); closeBq()
    if inCode then sb.append("</code></pre>\n")
    sb.toString

  def toStaticSite(title: String, source: String): String =
    s"""<!DOCTYPE html>
       |<html lang="en">
       |<head>
       |  <meta charset="utf-8"/>
       |  <meta name="viewport" content="width=device-width, initial-scale=1"/>
       |  <title>${escape(title)}</title>
       |  <style>
       |    :root { color-scheme: light dark; }
       |    body { font-family: "Iowan Old Style", "Palatino Linotype", Palatino, Georgia, serif;
       |           max-width: 42rem; margin: 3rem auto; padding: 0 1.25rem;
       |           line-height: 1.65; color: #1c1917; background: #faf7f2; }
       |    @media (prefers-color-scheme: dark) {
       |      body { color: #e7e5e4; background: #1c1917; }
       |      a { color: #93c5fd; }
       |      pre { background: #292524; }
       |      code { background: #292524; }
       |    }
       |    h1,h2,h3 { font-family: "Avenir Next", "Segoe UI", sans-serif; line-height: 1.25; }
       |    pre { background: #f5f5f4; padding: 1rem; overflow-x: auto; border-radius: 6px; }
       |    code { font-family: ui-monospace, SFMono-Regular, Menlo, monospace; font-size: 0.9em;
       |           background: #f5f5f4; padding: 0.1em 0.3em; border-radius: 4px; }
       |    pre code { background: transparent; padding: 0; }
       |    blockquote { border-left: 3px solid #a8a29e; margin-left: 0; padding-left: 1rem; color: #57534e; }
       |    a { color: #1d4ed8; }
       |  </style>
       |</head>
       |<body>
       |${toHtml(source)}
       |</body>
       |</html>
       |""".stripMargin

  private val linkRe: Regex = """\[([^\]]+)\]\(([^)]+)\)""".r
  private val imageRe: Regex = """!\[([^\]]*)\]\(([^)]+)\)""".r
  private val codeRe: Regex = """`([^`]+)`""".r
  private val boldRe: Regex = """\*\*([^*]+)\*\*""".r
  private val italicRe: Regex = """\*([^*]+)\*""".r
  private val strikeRe: Regex = """~~([^~]+)~~""".r

  private def formatInline(text: String): String =
    var s = escape(text)
    s = imageRe.replaceAllIn(s, m => s"""<img alt="${m.group(1)}" src="${m.group(2)}"/>""")
    s = linkRe.replaceAllIn(s, m => s"""<a href="${m.group(2)}">${m.group(1)}</a>""")
    s = codeRe.replaceAllIn(s, m => s"<code>${m.group(1)}</code>")
    s = boldRe.replaceAllIn(s, m => s"<strong>${m.group(1)}</strong>")
    s = italicRe.replaceAllIn(s, m => s"<em>${m.group(1)}</em>")
    s = strikeRe.replaceAllIn(s, m => s"<del>${m.group(1)}</del>")
    s

  private def escape(s: String): String =
    s.flatMap {
      case '&'  => "&amp;"
      case '<'  => "&lt;"
      case '>'  => "&gt;"
      case '"'  => "&quot;"
      case '\'' => "&#39;"
      case c    => c.toString
    }
