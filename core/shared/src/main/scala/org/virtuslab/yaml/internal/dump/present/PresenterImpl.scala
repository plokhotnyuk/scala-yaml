package org.virtuslab.yaml.internal.dump.present

import scala.annotation.{switch, tailrec}
import scala.collection.mutable
import org.virtuslab.yaml.Tag
import org.virtuslab.yaml.internal.load.parse.EventKind
import org.virtuslab.yaml.internal.load.parse.EventKind._
import org.virtuslab.yaml.internal.load.reader.token.ScalarStyle

object PresenterImpl extends Presenter {
  override def asString(events: Seq[EventKind]): String = {
    val sb      = new java.lang.StringBuilder
    val stack   = new mutable.Stack[EventKind]
    val newline = System.lineSeparator()

    var toplevelNode = true // toplevel node should't insert newline and increase indent
    var indent       = 0

    @tailrec
    def serializeNode(events: List[EventKind]): List[EventKind] = events match {
      case head :: tail =>
        head match {
          case s: Scalar =>
            insertSequencePadding()
            serializeScalar(s)
            sb.append(newline)
            tail
          case _: MappingStart =>
            insertSequencePadding()
            pushAndIncreaseIndent(MappingStart())
            serializeMapping(tail)
          case _: SequenceStart =>
            insertSequencePadding()
            pushAndIncreaseIndent(SequenceStart())
            serializeSequence(tail)
          case _ => serializeNode(tail)
        }
      case _ => Nil
    }

    @tailrec
    def serializeMapping(events: List[EventKind]): List[EventKind] = events match {
      case head :: tail =>
        head match {
          case s: Scalar =>
            var n = indent
            while (n > 0) {
              sb.append(' ')
              n -= 1
            }
            serializeScalar(s)
            sb.append(':').append(' ')
            serializeMapping(serializeNode(tail))
          case _: MappingEnd.type =>
            indent -= 2
            stack.pop()
            tail
          case _ => sys.error("Cannot serialize non-scalar mapping key: " + head)
        }
      case _ => Nil
    }

    @tailrec
    def serializeSequence(events: List[EventKind]): List[EventKind] = events match {
      case head :: tail =>
        head match {
          case _: SequenceEnd.type =>
            indent -= 2
            stack.pop()
            tail
          case _ =>
            serializeSequence(serializeNode(events))
        }
      case _ => Nil
    }

    def insertSequencePadding() = stack.headOption match {
      case Some(_: SequenceStart) =>
        var n = indent
        while (n > 0) {
          sb.append(' ')
          n -= 1
        }
        sb.append('-').append(' ')
      case _ => ()
    }

    def pushAndIncreaseIndent(event: EventKind) = {
      if (toplevelNode) toplevelNode = false
      else {
        indent += 2
        sb.append(newline)
      }
      stack.prepend(event)
    }

    def serializeScalar(s: Scalar) = {
      val value = s.value
      val style = s.style
      val tag   = s.metadata.tag
      if (tag.exists(_ eq Tag.str)) {
        if ((style eq ScalarStyle.DoubleQuoted) || requiresDoubleQuoting(value)) {
          escapeDoubleQuoted(value)
        } else if (style eq ScalarStyle.Plain) sb.append(value)
        else if (style eq ScalarStyle.SingleQuoted) escapeSingleQuoted(value)
        else if (style eq ScalarStyle.Literal) escapeLiteral(value)
        else if (style eq ScalarStyle.Folded) escapeFolded(value)
      } else if (tag.exists(_ eq Tag.nullTag)) sb.append("!!null")
      else sb.append(value)
    }

    def requiresDoubleQuoting(s: String): Boolean = {
      val len = s.length
      if (len == 0) return true
      var c     = s.charAt(0)
      val cLast = s.charAt(len - 1)
      if (
        c < 32 || c == 127 || cLast == ':' ||
        Character.isWhitespace(c) || Character.isWhitespace(cLast)
      ) return true
      (c: @switch) match {
        case 'n' | 'N' | '~' =>
          if (Tag.nullPattern.pattern.matcher(s).matches) return true
        case 'f' | 'F' =>
          if (Tag.falsePattern.pattern.matcher(s).matches) return true
        case 't' | 'T' =>
          if (Tag.truePattern.pattern.matcher(s).matches) return true
        case '-' =>
          if (
            len == 1 || Character.isWhitespace(s.charAt(1)) ||
            Tag.int10Pattern.pattern.matcher(s).matches ||
            Tag.floatPattern.pattern.matcher(s).matches ||
            Tag.minusInfinity.pattern.matcher(s).matches
          ) return true
        case '+' =>
          if (
            Tag.int10Pattern.pattern.matcher(s).matches ||
            Tag.floatPattern.pattern.matcher(s).matches ||
            Tag.plusInfinity.pattern.matcher(s).matches
          ) return true
        case '0' =>
          if (
            Tag.int8Pattern.pattern.matcher(s).matches ||
            Tag.int16Pattern.pattern.matcher(s).matches ||
            Tag.int10Pattern.pattern.matcher(s).matches ||
            Tag.floatPattern.pattern.matcher(s).matches
          ) return true
        case '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
          if (
            Tag.int10Pattern.pattern.matcher(s).matches ||
            Tag.floatPattern.pattern.matcher(s).matches
          ) return true
        case '.' =>
          if (
            Tag.floatPattern.pattern.matcher(s).matches ||
            Tag.nan.pattern.matcher(s).matches ||
            Tag.plusInfinity.pattern.matcher(s).matches
          ) return true
        case '?' | ':' =>
          if (len == 1 || Character.isWhitespace(s.charAt(1))) return true
        case ',' | '[' | ']' | '{' | '}' | '#' | '&' | '*' | '!' | '|' | '>' | '\'' | '"' | '%' |
            '@' | '`' =>
          return true
        case _ =>

      }
      var prev = c
      var i    = 1
      while (i < len) {
        c = s.charAt(i)
        i += 1
        if (
          c < 32 || c == 127 ||
          prev == ':' && c == ' ' || prev == ' ' && c == '#'
        ) return true
        prev = c
      }
      false
    }

    def escapeDoubleQuoted(s: String): Unit = {
      sb.append('"')
      val len = s.length
      var i   = 0
      while (i < len) {
        val c = s.charAt(i)
        (c: @switch) match {
          case '"'  => sb.append("\\\"")
          case '\\' => sb.append("\\\\")
          case '\n' => sb.append("\\n")
          case '\r' => sb.append("\\r")
          case '\t' => sb.append("\\t")
          case '\b' => sb.append("\\b")
          case '\f' => sb.append("\\f")
          case _ =>
            if (c < 32 || c == 127) {
              sb.append("\\u")
              sb.append(Character.forDigit((c >> 12) & 0xf, 16))
              sb.append(Character.forDigit((c >> 8) & 0xf, 16))
              sb.append(Character.forDigit((c >> 4) & 0xf, 16))
              sb.append(Character.forDigit(c & 0xf, 16))
            } else sb.append(c)
        }
        i += 1
      }
      sb.append('"')
    }

    def escapeSingleQuoted(s: String): Unit = {
      sb.append('\'')
      val len = s.length
      var i   = 0
      while (i < len) {
        val c = s.charAt(i)
        if (c == '\'') sb.append(c)
        sb.append(c)
        i += 1
      }
      sb.append('\'')
    }

    def escapeLiteral(s: String): Unit = {
      sb.append('|')
      val len = s.length
      if (len == 0) {
        sb.append('-')
        sb.append(newline)
      } else {
        if (s.charAt(len - 1) != '\n') sb.append('-')
        else {
          val lastNlLen = if (len >= 2 && s.charAt(len - 2) == '\r') 2 else 1
          if (len > lastNlLen && s.charAt(len - lastNlLen - 1) == '\n') sb.append('+')
        }
        sb.append(newline)
        var i         = 0
        var isNewLine = true
        while (i < len) {
          val c      = s.charAt(i)
          val isCRLF = c == '\r' && (i + 1 < len) && s.charAt(i + 1) == '\n'
          if (isNewLine && c != '\n' && !isCRLF) {
            var n = indent
            while (n > 0) {
              sb.append(' ')
              n -= 1
            }
          }
          sb.append(c)
          isNewLine = c == '\n'
          i += 1
        }
      }
    }

    def escapeFolded(s: String): Unit = {
      sb.append('>')
      val len = s.length
      if (len == 0) {
        sb.append('-')
        sb.append(newline)
      } else {
        if (s.charAt(len - 1) != '\n') sb.append('-')
        else {
          val lastNlLen = if (len >= 2 && s.charAt(len - 2) == '\r') 2 else 1
          if (len > lastNlLen && s.charAt(len - lastNlLen - 1) == '\n') sb.append('+')
        }
        sb.append(newline)
        var i         = 0
        var isNewLine = true
        while (i < len) {
          val c      = s.charAt(i)
          val isCRLF = c == '\r' && (i + 1 < len) && s.charAt(i + 1) == '\n'
          if (isNewLine && c != '\n' && !isCRLF) {
            var n = indent
            while (n > 0) {
              sb.append(' ')
              n -= 1
            }
          }
          sb.append(c)
          isNewLine = c == '\n'
          i += 1
        }
      }
    }

    serializeNode(events.toList)
    sb.toString
  }
}
