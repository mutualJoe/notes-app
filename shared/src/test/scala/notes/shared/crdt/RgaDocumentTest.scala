package notes.shared.crdt

import notes.shared.model.Op

class RgaDocumentTest extends munit.FunSuite:

  test("sequential insert builds text") {
    val doc = RgaDocument.empty
    "Hi".zipWithIndex.foreach { case (c, i) => doc.insertLocal("a", i, c) }
    assertEquals(doc.text, "Hi")
  }

  test("delete leaves tombstone but hides char") {
    val doc = RgaDocument.fromText("a", "abc")
    doc.deleteLocal("a", 1)
    assertEquals(doc.text, "ac")
    assertEquals(doc.snapshot.count(_.deleted), 1)
  }

  test("concurrent inserts after same origin converge") {
    val base = RgaDocument.fromText("seed", "A")
    val snap = base.snapshot

    val left = RgaDocument.fromSnapshot(snap)
    val right = RgaDocument.fromSnapshot(snap)

    val opL = left.insertLocal("L", 1, 'L')
    val opR = right.insertLocal("R", 1, 'R')

    left.apply(opR)
    right.apply(opL)

    assertEquals(left.text, right.text)
    // Newest-first among siblings: site "R" > "L" → "ARL"
    assertEquals(left.text, "ARL")
  }

  test("insert in middle") {
    val doc = RgaDocument.fromText("a", "ac")
    doc.insertLocal("a", 1, 'b')
    assertEquals(doc.text, "abc")
  }

  test("apply remote ops is idempotent") {
    val doc = RgaDocument.empty
    val op = doc.insertLocal("a", 0, 'x')
    doc.apply(op)
    assertEquals(doc.text, "x")
    assertEquals(doc.snapshot.length, 1)
  }

  test("delete range") {
    val doc = RgaDocument.fromText("a", "hello")
    val ops = doc.deleteLocalRange("a", 1, 3)
    assertEquals(ops.length, 3)
    assertEquals(doc.text, "ho")
  }
