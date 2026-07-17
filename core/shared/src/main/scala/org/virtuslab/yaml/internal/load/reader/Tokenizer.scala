package org.virtuslab.yaml.internal.load.reader

import scala.annotation.tailrec
import org.virtuslab.yaml.Range
import org.virtuslab.yaml.ScannerError
import org.virtuslab.yaml.YamlError
import org.virtuslab.yaml.internal.load.TagHandle
import org.virtuslab.yaml.internal.load.TagPrefix
import org.virtuslab.yaml.internal.load.TagValue
import org.virtuslab.yaml.internal.load.reader.token.BlockChompingIndicator
import org.virtuslab.yaml.internal.load.reader.token.BlockChompingIndicator._
import org.virtuslab.yaml.internal.load.reader.token.ScalarStyle
import org.virtuslab.yaml.internal.load.reader.token.Token
import org.virtuslab.yaml.internal.load.reader.token.TokenKind
import org.virtuslab.yaml.internal.load.reader.token.TokenKind._
import scala.collection.mutable

trait Tokenizer {
  def peekToken(): Either[YamlError, Token]
  def popToken(): Token
}

object Tokenizer {
  def make(str: String): Tokenizer = new StringTokenizer(str)
}

private class StringTokenizer(str: String) extends Tokenizer {

  private val ctx = TokenizerContext(str)
  private val in  = ctx.reader

  override def peekToken(): Either[YamlError, Token] = ctx.tokens.headOption match {
    case Some(token) => new Right(token)
    case _ =>
      try new Right(getToken())
      catch {
        case e: ScannerError => new Left(e)
      }
  }

  override def popToken(): Token = ctx.tokens.removeHead()

  private def getToken(): Token = {
    while (ctx.needMoreTokens()) appendNextTokens(ctx.tokens)
    ctx.tokens.head
  }

  /**
  * Plain keys have to be resolved in the same line they were created, otherwise they are ordinary tokens.
  */
  private def shouldPopPlainKeys: Boolean =
    ctx.isInBlockCollection && ctx.potentialKeyOpt
      .exists(_.range.start.line != in.line)

  private def appendNextTokens(queue: mutable.ArrayDeque[Token]): Unit = {
    skipUntilNextToken()
    val closedBlockTokens = ctx.checkIndents(in.column)
    if (closedBlockTokens.nonEmpty || shouldPopPlainKeys) queue.appendAll(ctx.popPotentialKeys())
    queue.appendAll(closedBlockTokens)
    val peeked = in.peek()
    peeked match {
      case Reader.nullTerminator =>
        queue.appendAll(ctx.popPotentialKeys())
        queue.appendAll(ctx.checkIndents(-1))
        queue.append(Token(StreamEnd, in.range))
      case '-' if isDocumentStart =>
        in.skipN(if (in.peek(3) == Reader.nullTerminator) 3 else 4)
        queue.appendAll(ctx.parseDocumentStart(in.column))
      case '-' if in.isNextWhitespace =>
        // when last indent is lesser than current one, it means that this is start of the sequence
        if (ctx.isInBlockCollection && ctx.indent < in.column) {
          ctx.addIndent(in.column)
          queue.append(Token(SequenceStart, in.range))
        }
        if (ctx.isInBlockCollection && !ctx.isPlainKeyAllowed) {
          throw ScannerError.from(in.range, "cannot start sequence")
        }
        in.skipCharacter() // skip '-'
        queue.appendAll(ctx.popPotentialKeys())
        queue.append(Token(SequenceValue, in.range))
      case '.' if isDocumentEnd =>
        in.skipN(if (in.peek(3) == Reader.nullTerminator) 3 else 4)
        queue.appendAll(ctx.parseDocumentEnd())
      case '[' =>
        in.skipCharacter()
        ctx.enterFlowSequence
        queue.appendAll(ctx.popPotentialKeys())
        queue.append(Token(FlowSequenceStart, in.range))
      case ']' =>
        in.skipCharacter()
        ctx.leaveFlowSequence
        queue.appendAll(ctx.popPotentialKeys())
        queue.append(Token(FlowSequenceEnd, in.range))
      case '{' =>
        in.skipCharacter()
        ctx.enterFlowMapping
        ctx.isPlainKeyAllowed = true
        queue.appendAll(ctx.popPotentialKeys())
        queue.append(Token(FlowMappingStart, in.range))
      case '}' =>
        in.skipCharacter()
        ctx.leaveFlowMapping
        queue.appendAll(ctx.popPotentialKeys())
        queue.append(Token(FlowMappingEnd, in.range))
      case '&' =>
        val (name, range) = parseAnchorName()
        val anchorToken   = Token(Anchor(name), range)
        if (ctx.isPlainKeyAllowed) ctx.addPotentialKey(anchorToken)
        else queue.append(anchorToken)
      case '!' =>
        val range = in.range

        def parseVerbatimTag(): String = {
          val sb = new java.lang.StringBuilder
          sb.append('!')
          while ({
            val c = in.peek()
            c != '>' && !c.isWhitespace
          }) sb.append(in.read())
          in.peek() match {
            case '>' =>
              sb.append(in.read())
              sb.toString
            case _ =>
              throw ScannerError.from(in.range, "Lacks '>' which closes verbatim tag attribute")
          }
        }

        def parseTagSuffix(): String = {
          val sb = new java.lang.StringBuilder
          while ({
            val c = in.peek()
            !(c == '[' || c == ']' || c == '{' || c == '}') && !c.isWhitespace
          }) sb.append(in.read())
          val c = in.peek()
          if (c == '[' || c == ']' || c == '{' || c == '}')
            throw ScannerError.from(in.range, "Invalid character in tag")
          UrlDecoder.decode(sb.toString)
        }

        def parseShorthandTag(second: Char): TagValue =
          second match {
            case '!' => // tag handle starts with '!!'
              in.skipCharacter()
              TagValue.Shorthand(TagHandle.Secondary, parseTagSuffix())
            case _ => // tag handle starts with '!<char>' where char isn't space
              val sb = new java.lang.StringBuilder
              while ({
                val c = in.peek()
                !(c == '[' || c == ']' || c == '{' || c == '}') && !c.isWhitespace && c != '!'
              }) sb.append(in.read())
              val c = in.peek()
              if (c == '[' || c == ']' || c == '{' || c == '}')
                throw ScannerError.from(in.range, "Invalid character in tag")
              in.peek() match {
                case '!' =>
                  sb.insert(0, '!')    // prepend already skipped exclamation mark
                  sb.append(in.read()) // append ending exclamation mark
                  TagValue.Shorthand(TagHandle.Named(sb.toString), parseTagSuffix())
                case ' ' =>
                  TagValue.Shorthand(TagHandle.Primary, sb.toString)
                case _ => throw ScannerError.from(in.range, "Invalid tag handle")
              }
          }

        in.skipCharacter() // skip first '!'
        val peeked = in.peek()
        val tag: Tag = peeked match {
          case Reader.nullTerminator =>
            throw ScannerError.from(in.range, "Input stream ended unexpectedly")
          case '<' =>
            val tag = parseVerbatimTag()
            Tag(TagValue.Verbatim(tag))
          case ' ' =>
            Tag(TagValue.NonSpecific)
          case char =>
            val tagValue = parseShorthandTag(char)
            Tag(tagValue)
        }
        val token = Token(tag, range)
        if (ctx.isPlainKeyAllowed) ctx.addPotentialKey(token)
        else queue.append(token)
      case '%' =>
        val range = in.range
        in.skipCharacter() // skip %

        in.peek() match {
          case 'Y' if in.peekN(4) == "YAML" =>
            in.skipN(4)
            throw ScannerError.from(in.range, "YAML directives are not supported yet.")
          case 'T' if in.peekN(3) == "TAG" =>
            in.skipN(3)

            def parseTagHandle() = {
              in.peekNext() match { // peeking next char!! current char is exclamation mark
                case ' ' =>
                  in.skipCharacter() // skip exclamation mark
                  TagHandle.Primary
                case '!' =>
                  in.skipN(2) // skip both exclamation marks
                  TagHandle.Secondary
                case _ =>
                  val sb = new java.lang.StringBuilder
                  sb.append(in.read())

                  def condition = {
                    val c = in.peek()
                    !c.isWhitespace && c != '!'
                  }

                  while (condition) {
                    sb.append(in.read())
                  }
                  sb.append(in.read())
                  TagHandle.Named(sb.toString)
              }
            }

            def parseTagPrefix() = {
              skipSpaces()
              in.peek() match {
                case '!' =>
                  val sb = new java.lang.StringBuilder
                  while (!in.peek().isWhitespace) {
                    sb.append(in.read())
                  }
                  TagPrefix.Local(sb.toString)
                case char if char != '!' && char != ',' =>
                  val sb = new java.lang.StringBuilder
                  while (!in.peek().isWhitespace) {
                    sb.append(in.read())
                  }
                  TagPrefix.Global(sb.toString)
                case _ => throw ScannerError.from(in.range, "Invalid tag prefix in TAG directive")
              }
            }

            skipSpaces()
            in.peek() match {
              case '!' =>
                val handle = parseTagHandle()
                val prefix = parseTagPrefix()
                queue.append(Token(TokenKind.TagDirective(handle, prefix), range))
              case _ =>
                throw ScannerError.from(
                  in.range,
                  "Tag handle in TAG directive should start with '!'"
                )
            }
          case _ => throw ScannerError.from(in.range, "Unknown directive, expected YAML or TAG")
        }
      case '"' =>
        val sb = new java.lang.StringBuilder

        @tailrec
        def readScalar(): String =
          in.peek() match {
            case Reader.nullTerminator =>
              sb.toString
            case _ if in.isNewline =>
              skipUntilNextToken()
              sb.append(' ')
              readScalar()
            case '\\' if in.peekNext() == '"' =>
              in.skipN(2)
              sb.append('"')
              readScalar()
            case '"' =>
              in.skipCharacter()
              sb.toString
            case _ =>
              sb.append(in.read())
              readScalar()
          }

        val isPlainKeyAllowed = ctx.isPlainKeyAllowed
        val range             = in.range
        in.skipCharacter() // skip double quote
        val scalar      = readScalar()
        val endRange    = range.withEndPos(in.pos)
        val scalarToken = Token(Scalar(scalar, ScalarStyle.DoubleQuoted), endRange)
        if (isPlainKeyAllowed) ctx.addPotentialKey(scalarToken)
        else queue.append(scalarToken)
      case '\'' =>
        val sb = new java.lang.StringBuilder
        @tailrec
        def readScalar(): String =
          in.peek() match {
            case Reader.nullTerminator => sb.toString
            case '\'' if in.peekNext() == '\'' =>
              in.skipN(2)
              sb.append('\'')
              readScalar()
            case '\n' =>
              sb.append(' ')
              skipUntilNextToken()
              readScalar()
            case '\'' =>
              in.skipCharacter()
              sb.toString
            case _ =>
              sb.append(in.read())
              readScalar()
          }
        val isPlainKeyAllowed = ctx.isPlainKeyAllowed
        val range             = in.range
        in.skipCharacter() // skip single quote
        val scalar      = readScalar()
        val endRange    = range.withEndPos(in.pos)
        val scalarToken = Token(Scalar(scalar, ScalarStyle.SingleQuoted), endRange)
        if (isPlainKeyAllowed) ctx.addPotentialKey(scalarToken)
        else queue.append(scalarToken)
      case '>' =>
        val sb    = new java.lang.StringBuilder
        val range = in.range
        in.skipCharacter() // skip >
        val indentationIndicator: Option[Int] = parseIndentationIndicator()
        val chompingIndicator                 = parseChompingIndicator()
        val indentation =
          if (indentationIndicator.isEmpty) parseIndentationIndicator()
          else indentationIndicator
        parseBlockHeader()
        if (indentation.isEmpty) skipUntilNextToken()
        val foldedIndent = indentation.getOrElse(in.column)
        skipUntilNextIndent(foldedIndent)

        def chompedEmptyLines() =
          while (in.isNextNewline) {
            in.skipCharacter()
            sb.append('\n')
          }

        @tailrec
        def readFolded(
            prevCharWasNewline: Boolean = false,
            thisLineIsIndented: Boolean = false
        ): String = {
          in.peek() match {
            case Reader.nullTerminator => sb.toString
            case _ if in.isNewline =>
              ctx.isPlainKeyAllowed = true
              if (in.isNextNewline) {
                chompedEmptyLines()
                if (in.peek() != Reader.nullTerminator) {
                  in.skipCharacter()
                  skipUntilNextIndent(foldedIndent)
                }
                if (in.column != foldedIndent || in.peek() == Reader.nullTerminator) {
                  if (chompingIndicator == BlockChompingIndicator.Keep) sb.append('\n')
                  sb.toString
                } else readFolded(prevCharWasNewline = true)
              } else {
                in.skipCharacter() // skip newline
                skipUntilNextIndent(foldedIndent)
                if (in.column != foldedIndent || in.peek() == Reader.nullTerminator) {
                  chompingIndicator match {
                    case Keep => // if keep, strip all trailing newlines and spaces but count them and append counted amount of newlines
                      var count    = 1
                      var lastChar = sb.charAt(sb.length - 1)
                      while (lastChar == '\n' || lastChar == ' ') {
                        sb.deleteCharAt(sb.length - 1)
                        lastChar = sb.charAt(sb.length - 1)
                        count += 1
                      }
                      while (count > 0) {
                        sb.append('\n')
                        count -= 1
                      }
                    case Strip => // if strip, strip all trailing newlines and spaces
                      var lastChar = sb.charAt(sb.length - 1)
                      while (lastChar == '\n' || lastChar == ' ') {
                        sb.deleteCharAt(sb.length - 1)
                        lastChar = sb.charAt(sb.length - 1)
                      }
                    case Clip => // if clip, strip all trailing newlines and spaces and append a single newline
                      var lastChar = sb.charAt(sb.length - 1)
                      while (lastChar == '\n' || lastChar == ' ') {
                        sb.deleteCharAt(sb.length - 1)
                        lastChar = sb.charAt(sb.length - 1)
                      }
                      sb.append('\n')
                  }
                  sb.toString // final result
                } else {
                  sb.append({
                    if (prevCharWasNewline || thisLineIsIndented) '\n'
                    else ' '
                  })
                  readFolded(prevCharWasNewline = true)
                }
              }
            case ' ' if in.column == foldedIndent => // beginning of a line that is indented
              if (prevCharWasNewline) { // we are at the beginning of a line that is indented
                sb.setCharAt(sb.length() - 1, '\n') // replace last space with a newline
              }
              sb.append(in.read())
              readFolded(thisLineIsIndented = true)
            case _ =>
              sb.append(in.read())
              readFolded(thisLineIsIndented = thisLineIsIndented)
          }
        }
        val scalar        = readFolded()
        val chompedScalar = chompingIndicator.removeBlankLinesAtEnd(scalar)
        queue.append(Token(Scalar(chompedScalar, ScalarStyle.Folded), range))
      case '|' =>
        val sb    = new java.lang.StringBuilder
        val range = in.range
        in.skipCharacter() // skip |
        val indentationIndicator: Option[Int] = parseIndentationIndicator()
        val chompingIndicator                 = parseChompingIndicator()
        val indentation =
          if (indentationIndicator.isEmpty) parseIndentationIndicator()
          else indentationIndicator
        parseBlockHeader()
        if (indentation.isEmpty) skipUntilNextChar()
        val foldedIndent = indentation.getOrElse(in.column)
        skipUntilNextIndent(foldedIndent)

        @tailrec
        def readLiteral(): String =
          in.peek() match {
            case Reader.nullTerminator => sb.toString
            case _ if in.isNewline =>
              ctx.isPlainKeyAllowed = true
              sb.append(in.read())
              skipUntilNextIndent(foldedIndent)
              if (!in.isWhitespace && in.column != foldedIndent) sb.toString
              else readLiteral()
            case _ =>
              sb.append(in.read())
              readLiteral()
          }

        val scalar        = readLiteral()
        val chompedScalar = chompingIndicator.removeBlankLinesAtEnd(scalar)
        queue.append(Token(Scalar(chompedScalar, ScalarStyle.Literal), range))
      case '*' =>
        val (name, pos) = parseAnchorName()
        val aliasToken  = Token(Alias(name), pos)
        if (ctx.isPlainKeyAllowed) ctx.addPotentialKey(aliasToken)
        else queue.append(aliasToken)
      case ',' =>
        in.skipCharacter()
        ctx.isPlainKeyAllowed = true
        queue.appendAll(ctx.popPotentialKeys())
        queue.append(Token(Comma, in.range))
      case ':' if in.isNextWhitespace || (ctx.isInFlowCollection && ctx.isPlainKeyAllowed) =>
        in.skipCharacter() // skip
        val mappingValueToken = Token(MappingValue, in.range)
        lazy val firstSimpleKey = ctx.potentialKeys.headOption.getOrElse(
          throw ScannerError.from("Not found expected key for value", mappingValueToken)
        )
        if (ctx.isInBlockCollection && ctx.indent < firstSimpleKey.start.column) {
          ctx.addIndent(firstSimpleKey.start.column)
          queue.append(Token(MappingStart, firstSimpleKey.range))
        }
        val potentialKeys = ctx.popPotentialKeys()
        ctx.isPlainKeyAllowed = false
        if (
          ctx.isInBlockCollection &&
          firstSimpleKey.range.end.exists(
            _.line > firstSimpleKey.range.start.line
          )
        ) throw ScannerError.from("Mapping value is not allowed", mappingValueToken)
        queue.append(Token(MappingKey, in.range))
        queue.appendAll(potentialKeys)
        queue.append(mappingValueToken)
      case _ => queue.appendAll(parsePlainScalar())
    }
  }

  private def isDocumentStart = {
    val charAfterMarker = in.peek(3)
    in.peekN(3) == "---" &&
    (charAfterMarker.isWhitespace || charAfterMarker == Reader.nullTerminator)
  }

  private def isDocumentEnd = {
    val charAfterMarker = in.peek(3)
    in.peekN(
      3
    ) == "..." && (charAfterMarker.isWhitespace || charAfterMarker == Reader.nullTerminator)
  }

  private def parseAnchorName(): (String, Range) = {
    val sb = new java.lang.StringBuilder

    @tailrec
    def readAnchorName(): String =
      in.peek() match {
        case Reader.nullTerminator => sb.toString
        case char
            if !(char == '[' || char == ']' || char == '{' || char == '}' || char == ',') && !in.isWhitespace =>
          sb.append(in.read())
          readAnchorName()
        case _ => sb.toString
      }

    val range = in.range
    in.skipCharacter()
    val name = readAnchorName()
    (name, range)
  }

  /**
   * This header is followed by a non-content line break with an optional comment.
   */
  private def parseBlockHeader(): Unit = {
    while (in.peek() == ' ')
      in.skipCharacter()

    if (in.peek() == '#')
      skipComment()

    if (in.isNewline) in.skipCharacter()
  }

  /**
   * final break interpretation - https://yaml.org/spec/1.2/#b-chomped-last(t)
   */
  private def parseChompingIndicator(): BlockChompingIndicator =
    in.peek() match {
      case '-' =>
        in.skipCharacter()
        BlockChompingIndicator.Strip
      case '+' =>
        in.skipCharacter()
        BlockChompingIndicator.Keep
      case _ => BlockChompingIndicator.Clip
    }

  private def parseIndentationIndicator(): Option[Int] =
    in.peek() match {
      case number if number.isDigit =>
        in.skipCharacter()
        Some(number.asDigit)
      case _ => None
    }

  private def parsePlainScalar(): List[Token] = {
    val sb = new java.lang.StringBuilder

    def chompedEmptyLines() =
      while (in.isNextNewline) {
        in.skipCharacter()
        sb.append('\n')
      }

    def readScalar(): String = {
      val peeked = in.peek()
      peeked match {
        case Reader.nullTerminator                                       => sb.toString
        case ':' if in.isNextWhitespace                                  => sb.toString
        case ':' if in.peekNext() == ',' && ctx.isInFlowCollection       => sb.toString
        case char if !ctx.isAllowedSpecialCharacter(char)                => sb.toString
        case _ if (isDocumentEnd || isDocumentStart) && ctx.indent == -1 => sb.toString
        case ' ' if in.peekNext() == '#'                                 => sb.toString
        case _ if in.isNewline =>
          ctx.isPlainKeyAllowed = true
          if (in.isNextNewline) chompedEmptyLines()
          else sb.append(' ')
          skipUntilNextToken()
          if (in.column > ctx.indent)
            readScalar()
          else sb.toString
        case _ =>
          sb.append(in.read())
          readScalar()
      }
    }

    val isPlainKeyAllowed = ctx.isPlainKeyAllowed
    val range             = in.range
    val scalar            = readScalar()
    val endRange          = range.withEndPos(in.pos)
    val scalarToken       = Token(Scalar(scalar.trim, ScalarStyle.Plain), endRange)
    if (isPlainKeyAllowed) {
      ctx.addPotentialKey(scalarToken)
      Nil
    } else List(scalarToken)
  }

  def skipUntilNextToken(): Unit = {
    while (in.isWhitespace && !in.isNewline) in.skipCharacter()
    if (in.peek() == '#') skipComment()
    if (in.isNewline) {
      ctx.isPlainKeyAllowed = true
      in.skipCharacter()
      skipUntilNextToken()
    }
  }

  def skipSpaces(): Unit =
    while (in.peek() == ' ') in.skipCharacter()

  def skipUntilNextIndent(indentBlock: Int): Unit =
    while (in.peek() == ' ' && in.column < indentBlock) in.skipCharacter()

  def skipUntilNextChar() =
    while (in.isWhitespace) in.skipCharacter()

  private def skipComment(): Unit = while (in.peek() != Reader.nullTerminator && !in.isNewline)
    in.skipCharacter()
}
