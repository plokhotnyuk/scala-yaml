package org.virtuslab.yaml.internal.load.reader

import scala.annotation.tailrec
import org.virtuslab.yaml.Position
import org.virtuslab.yaml.Range

trait Reader {

  /** Read current character and advance by 1 position
    * @return current character or '\u0000' in case there are no chars left
    */
  def read(): Char

  /** Read current character without advancing
    * @return current character or '\u0000' in case there are no chars left
    */
  def peek(n: Int = 0): Char

  def line: Int
  def column: Int
  def offset: Int
  def pos: Position
  def range: Range

  def skipCharacter(): Unit
  def skipN(n: Int): Unit
  def skipWhitespaces(): Unit
  def peekN(n: Int): String

  final def peekNext(): Char          = peek(1)
  final def isWhitespace: Boolean     = peek().isWhitespace
  final def isNextWhitespace: Boolean = peekNext().isWhitespace
  final def isNewline: Boolean        = isNewlineN(0)
  final def isNextNewline: Boolean    = isNewlineN(1)

  private def isNewlineN(n: Int): Boolean = {
    val c = peek(n)
    c == '\n' || isWindowsNewline(c)
  }
  protected def isWindowsNewline(c: Char): Boolean = c == '\r' && peekNext() == '\n'
}

object Reader {
  final val nullTerminator: Char = '\u0000'
}

private[yaml] class StringReader(in: String) extends Reader {
  private val len = in.length
  var line: Int   = 0
  var column: Int = 0
  var offset: Int = 0
  val lines       = in.split("\n", -1).toVector

  override def pos   = Position(offset, line, column)
  override def range = Range(pos, lines)

  override def peek(n: Int = 0): Char = {
    val i = offset + n
    if (i < len) in.charAt(i)
    else Reader.nullTerminator
  }

  override def peekN(n: Int): String = {
    val end = offset + n
    if (end <= len) in.substring(offset, end)
    else {
      val available = math.max(0, len - offset)
      val padding   = new String(Array.fill(n - available)(Reader.nullTerminator))
      if (available > 0) in.substring(offset, len) + padding
      else padding
    }
  }

  private def nextLine(): Unit = { column = 0; line += 1 }

  private def skipAndMaintainPosition() = {
    val char = in.charAt(offset)
    if (isWindowsNewline(char)) {
      offset += 2
      nextLine()
      2
    } else if (char == '\n') {
      offset += 1
      nextLine()
      1
    } else {
      offset += 1
      column += 1
      1
    }
  }

  override def skipN(n: Int): Unit = {
    @tailrec def loop(left: Int): Unit =
      if (left <= 0) ()
      else {
        val skipped = skipAndMaintainPosition()
        loop(left - skipped)
      }
    loop(n)
  }

  override def skipCharacter(): Unit = skipAndMaintainPosition()

  override def skipWhitespaces(): Unit =
    while (isWhitespace)
      skipCharacter()

  override def read(): Char = {
    skipCharacter()
    in.charAt(offset - 1)
  }

}
