package org.virtuslab.yaml.internal.load.parse

import scala.collection.mutable

import org.virtuslab.yaml.CoreSchemaTag
import org.virtuslab.yaml.CustomTag
import org.virtuslab.yaml.ParseError
import org.virtuslab.yaml.Tag
import org.virtuslab.yaml.YamlError
import org.virtuslab.yaml.internal.load.TagValue
import org.virtuslab.yaml.internal.load.reader.Tokenizer
import org.virtuslab.yaml.internal.load.reader.token.ScalarStyle
import org.virtuslab.yaml.internal.load.reader.token.Token
import org.virtuslab.yaml.internal.load.reader.token.TokenKind

private sealed abstract class Production
private object Production {
  case object ParseStreamStart      extends Production
  case object ParseStreamEnd        extends Production
  case object ParseDocumentStart    extends Production
  case object ParseDocumentEnd      extends Production
  case object ParseDocumentStartOpt extends Production

  case object ParseNode   extends Production
  case object ParseScalar extends Production

  case object ParseMappingEnd         extends Production
  case object ParseMappingEntry       extends Production
  case object ParseMappingValue       extends Production
  case object ParseMappingValueNode   extends Production
  case object ParseMappingSequenceEnd extends Production
  case object ParseMappingEntryOpt    extends Production

  case object ParseSequenceEnd      extends Production
  case object ParseSequenceEntry    extends Production
  case object ParseSequenceEntryOpt extends Production

  case object ParseFlowNode extends Production

  case object ParseFlowMappingEnd      extends Production
  case object ParseFlowMappingEntry    extends Production
  case object ParseFlowMappingEntryOpt extends Production
  case object ParseFlowMappingComma    extends Production

  case object ParseFlowSeqEnd       extends Production
  case object ParseFlowSeqEntry     extends Production
  case object ParseFlowSeqEntryOpt  extends Production
  case object ParseFlowSeqPairKey   extends Production
  case object ParseFlowSeqPairValue extends Production
  case object ParseFlowSeqComma     extends Production

  case class ReturnEvent(produceEvent: Token => Event) extends Production
}

/**
 * Parser takes a stream of [[Token]]s and produces a series of serialization [[Event]]s. Parsing can fail due to ill-formed input.
 * 
 * ParserImpl is using following productions:
 * 
 * ParseStreamStart              ::= (stream_start) ParseDocumentStart ParseDocumentStartOpt ParseStreamEnd
 * ParseStreamEnd                ::= (stream_end)
 *         
 * ParseDocumentStart            ::= (document_start) ParseNode ParseDocumentEnd
 * ParseDocumentEnd              ::= (document_end)
 * ParseDocumentStartOpt         ::= epsilon | ParseDocumentStart ParseDocumentStartOpt
 * 
 * ParseNode(indentLess=false)   ::= ParseMappingStart | ParseFlowMappingStart |
 *                                   ParseSequenceStart(indentLess) | ParseFlowSeqStart | ParseScalar
 * ParseScalar                   ::= scalar
 * 
 * ParseMappingStart             ::= mapping_start ParseMappingEntry ParseMappingEnd
 * ParseMappingEnd               ::= mapping_end
 * ParseMappingEntry             ::= mapping_key ParseScalar ParseMappingValue ParseMappingEntryOpt
 * ParseMappingValue             ::= mapping_value ParseNode(true)
 * ParseMappingEntryOpt          ::= epsilon | ParseMappingEntry
 * 
 * 
 * ParseSequenceStart(indentLess)::= (seq_start) ParseSequenceEntry ParseSequenceEnd(indentLess)
 * ParseSequenceEnd(indentLess)  ::= (seq_end)
 * ParseSequenceEntry            ::= seq_value ParseNode ParseSequenceEntryOpt
 * ParseSequenceEntryOpt         ::= epsilon | ParseSequenceEntry
 * 
 * ParseFlowNode                 ::= ParseFlowMappingStart | ParseFlowSeqStart | ParseScalar 
 * 
 * ParseFlowMappingStart         ::= flow_mapping_start ParseFlowMappingEntryOpt ParseFlowMappingEnd
 * ParseFlowMappingEnd           ::= flow_mapping_end
 * ParseFlowMappingEntry         ::= mapping_key ParseScalar ParseMappingValue 
 * ParseFlowMappingEntryOpt      ::= epsilon | ParseFlowMappingEntry
 * ParseFlowMappingComma         ::= epsilon | comma ParseFlowMappingEntryOpt
 * 
 * ParseFlowSeqStart             ::= flow_seq_start ParseFlowSeqEntryOpt ParseFlowSeqEnd
 * ParseFlowSeqEnd               ::= flow_seq_end
 * ParseFlowSeqEntry             ::= (ParseFlowNode | ParseFlowPairKey) ParseFlowSeqComma
 * ParseFlowSeqEntryOpt          ::= epsilon | ParseFlowSeqEntry
 * ParseFlowSeqComma             ::= epsilon | comma ParseFlowSeqEntryOpt
 * 
 * 
 * ParseFlowPairKey              ::= mapping_key ParseFlowNode ParseFlowPairValue
 * ParseFlowPairValue            ::= mapping_value ParseFlowNode
 * 
*/
final class ParserImpl private (in: Tokenizer) extends Parser {

  import Production._

  private val productions = mutable.ArrayDeque[Production](ParseStreamStart)
  // Primary ('!') and secondary ('!!') tags, if not overwritten by tag directive, have some predefined defaults
  private val defaultDirectives = Map("!" -> "!", "!!" -> "tag:yaml.org,2002:")
  private val directives        = defaultDirectives.to(mutable.Map)

  private[yaml] def getEvents(): Either[YamlError, List[Event]] = {
    val events = new mutable.ListBuffer[Event]
    while (productions.length > 0) {
      getNextEventImpl() match {
        case Right(event) => events.append(event)
        case err          => return err.asInstanceOf[Either[YamlError, List[Event]]]
      }
    }
    new Right(events.toList)
  }

  override def getNextEvent(): Either[YamlError, Event] =
    if (productions.length > 0) getNextEventImpl()
    else new Right(Event.streamEnd)

  private def clearDirectives(): Unit = {
    directives.clear()
    directives.addAll(defaultDirectives)
  }

  private def parseStreamStart(token: Token) = {
    productions.prepend(ParseStreamEnd)
    productions.prepend(ParseDocumentStartOpt)
    new Right(Event(EventKind.StreamStart, token.range))
  }

  private def parseDocumentStart(token: Token) = {
    productions.prepend(ParseDocumentEnd)
    productions.prepend(ParseNode)
    new Right(
      Event(
        EventKind.DocumentStart(explicit = {
          (token.kind eq TokenKind.DocumentStart) && {
            in.popToken()
            true
          }
        }),
        token.range
      )
    )
  }

  private def parseDocumentStartOpt(token: Token) = {
    token.kind match {
      case td: TokenKind.TagDirective =>
        directives.update(td.handle.value, td.prefix.value)
        in.popToken()
        productions.prepend(ParseDocumentStartOpt) // call self once again
      case _: TokenKind.DocumentStart.type =>
        productions.prepend(ParseDocumentStartOpt)
        productions.prepend(ParseDocumentStart)
      case _: TokenKind.MappingStart.type | _: TokenKind.Scalar | _: TokenKind.SequenceStart.type |
          _: TokenKind.FlowMappingStart.type | _: TokenKind.FlowSequenceStart.type |
          _: TokenKind.Anchor | _: TokenKind.Tag =>
        productions.prepend(ParseDocumentStartOpt)
        productions.prepend(ParseDocumentStart)
      case _ =>
    }
    getNextEventImpl()
  }

  private def parseDocumentEnd(token: Token) = {
    clearDirectives()
    new Right(
      Event(
        EventKind.DocumentEnd(explicit = {
          (token.kind eq TokenKind.DocumentEnd) && {
            in.popToken()
            true
          }
        }),
        token.range
      )
    )
  }

  private def parseMappingEnd(token: Token) =
    if (token.kind eq TokenKind.BlockEnd) {
      in.popToken()
      new Right(Event(EventKind.MappingEnd, token.range))
    } else new Left(ParseError.from(TokenKind.BlockEnd, token))

  private def parseMappingEntry(token: Token) =
    if (token.kind eq TokenKind.MappingKey) {
      in.popToken()
      productions.prepend(ParseMappingEntryOpt)
      productions.prepend(ParseMappingValue)
      productions.prepend(ParseScalar)
      getNextEventImpl()
    } else new Left(ParseError.from(TokenKind.MappingKey, token))

  private def parseMappingValue(token: Token) =
    if (token.kind eq TokenKind.MappingValue) {
      in.popToken()
      productions.prepend(ParseMappingValueNode)
      getNextEventImpl()
    } else new Left(ParseError.from(TokenKind.MappingValue, token))

  private def parseMappingValueNode(token: Token) =
    if (token.kind eq TokenKind.SequenceValue) {
      productions.prepend(ParseMappingSequenceEnd)
      productions.prepend(ParseSequenceEntry)
      new Right(Event(EventKind.SequenceStart(), token.range))
    } else parseNode(token)

  private def parseMappingSequenceEnd(token: Token) =
    new Right(Event(EventKind.SequenceEnd, token.range))

  private def parseMappingEntryOpt(token: Token) = {
    if (token.kind eq TokenKind.MappingKey) productions.prepend(ParseMappingEntry)
    getNextEventImpl()
  }

  private def parseSequenceEnd(token: Token) =
    if (token.kind eq TokenKind.BlockEnd) {
      in.popToken()
      new Right(Event(EventKind.SequenceEnd, token.range))
    } else new Left(ParseError.from(TokenKind.BlockEnd, token))

  private def parseSequenceEntry(token: Token) =
    if (token.kind eq TokenKind.SequenceValue) {
      in.popToken()
      productions.prepend(ParseSequenceEntryOpt)
      productions.prepend(ParseNode)
      getNextEventImpl()
    } else new Left(ParseError.from(TokenKind.SequenceValue, token))

  private def parseSequenceEntryOpt(token: Token) = {
    if (token.kind eq TokenKind.SequenceValue) productions.prepend(ParseSequenceEntry)
    getNextEventImpl()
  }

  private def parseFlowMappingEnd(token: Token) =
    if (token.kind eq TokenKind.FlowMappingEnd) {
      in.popToken()
      new Right(Event(EventKind.MappingEnd, token.range))
    } else new Left(ParseError.from(TokenKind.FlowMappingEnd, token))

  private def parseFlowMappingEntry(token: Token) = {
    if (token.kind eq TokenKind.MappingKey) {
      in.popToken()
      productions.prepend(ParseFlowMappingComma)
      productions.prepend(ParseMappingValue)
      productions.prepend(ParseScalar)
    }
    getNextEventImpl()
  }

  private def parseFlowMappingEntryOpt(token: Token) = token.kind match {
    case _: TokenKind.Scalar | _: TokenKind.Anchor =>
      productions.prependAll(Array(ParseFlowNode, ParseFlowMappingComma, ParseFlowMappingEntry))
      parseFlowNode(token)
    case k =>
      if (k eq TokenKind.MappingKey)
        productions.prepend(
          ParseFlowMappingEntry
        ) // flow mapping start right after flow mapping start{>>{
      else if (k eq TokenKind.FlowMappingStart)
        productions.prepend(ParseFlowNode) // flow sequence start right after flow mapping start{>>[
      else if (k eq TokenKind.FlowSequenceStart) productions.prepend(ParseFlowNode)
      getNextEventImpl()
  }

  private def parseFlowMappingComma(token: Token) = {
    if (token.kind eq TokenKind.Comma) {
      in.popToken()
      productions.prepend(ParseFlowMappingEntryOpt)
    }
    getNextEventImpl()
  }

  private def parseFlowSeqEnd(token: Token) =
    if (token.kind eq TokenKind.FlowSequenceEnd) {
      in.popToken()
      new Right(Event(EventKind.SequenceEnd, token.range))
    } else new Left(ParseError.from(TokenKind.FlowSequenceEnd, token))

  private def parseFlowSeqEntry(token: Token) = {
    productions.prepend(ParseFlowSeqComma)
    if (token.kind eq TokenKind.MappingKey) {
      productions.prepend(ParseFlowSeqPairKey)
      new Right(Event(EventKind.MappingStart(), token.range))
    } else {
      productions.prepend(ParseFlowNode)
      getNextEventImpl()
    }
  }

  private def parseFlowSeqEntryOpt(token: Token) = {
    token.kind match {
      case _: TokenKind.FlowMappingStart.type | _: TokenKind.FlowSequenceStart.type |
          _: TokenKind.Scalar | _: TokenKind.Alias | _: TokenKind.Anchor |
          _: TokenKind.MappingKey.type =>
        productions.prepend(ParseFlowSeqEntry)
      case _ =>
    }
    getNextEventImpl()
  }

  private def parseFlowPairKey(token: Token) =
    if (token.kind eq TokenKind.MappingKey) {
      in.popToken()
      productions.prepend(ReturnEvent(t => Event(EventKind.MappingEnd, t.range)))
      productions.prepend(ParseFlowSeqPairValue)
      productions.prepend(ParseFlowNode)
      getNextEventImpl()
    } else new Left(ParseError.from(TokenKind.MappingKey, token))

  private def parseFlowPairValue(token: Token) =
    if (token.kind eq TokenKind.MappingValue) {
      in.popToken()
      productions.prepend(ParseFlowNode)
      getNextEventImpl()
    } else new Left(ParseError.from(TokenKind.MappingValue, token))

  private def parseFlowSeqComma(token: Token) = {
    if (token.kind eq TokenKind.Comma) {
      in.popToken()
      productions.prepend(ParseFlowSeqEntryOpt)
    }
    getNextEventImpl()
  }

  private def parseScalar(token: Token) =
    parseNodeAttributes(Right(token)).flatMap { case (metadata, nextToken) =>
      nextToken.kind match {
        case TokenKind.Scalar(value, style) =>
          in.popToken()
          new Right(Event(EventKind.Scalar(value, style, metadata), token.range))
        case TokenKind.Alias(alias) =>
          if (metadata.anchor.isDefined) {
            new Left(ParseError.from("Alias cannot have an anchor", nextToken))
          } else {
            in.popToken()
            new Right(Event(EventKind.Alias(Anchor(alias)), nextToken.range))
          }
        case _ =>
          new Left(ParseError.from(TokenKind.Scalar.toString, token))
      }
    }

  private def parseFlowNode(token: Token) =
    parseNode(token, couldParseBlockCollection = false)

  private def parseNode(
      token: Token,
      couldParseBlockCollection: Boolean = true
  ): Either[YamlError, Event] = parseNodeAttributes(new Right(token)) match {
    case Right((metadata, nextToken)) =>
      nextToken.kind match {
        case a: TokenKind.Alias =>
          if (metadata.anchor.isEmpty) {
            in.popToken()
            new Right(Event(EventKind.Alias(Anchor(a.value)), nextToken.range))
          } else new Left(ParseError.from("Alias cannot have an anchor", nextToken))
        case _: TokenKind.MappingStart.type if couldParseBlockCollection =>
          in.popToken()
          productions.prepend(ParseMappingEnd)
          productions.prepend(ParseMappingEntry)
          new Right(Event(EventKind.MappingStart(metadata), nextToken.range))
        case _: TokenKind.SequenceStart.type if couldParseBlockCollection =>
          in.popToken()
          productions.prepend(ParseSequenceEnd)
          productions.prepend(ParseSequenceEntry)
          new Right(Event(EventKind.SequenceStart(metadata), nextToken.range))
        case _: TokenKind.FlowMappingStart.type =>
          in.popToken()
          productions.prepend(ParseFlowMappingEnd)
          productions.prepend(ParseFlowMappingEntryOpt)
          new Right(Event(EventKind.MappingStart(metadata), nextToken.range))
        case _: TokenKind.FlowSequenceStart.type =>
          in.popToken()
          productions.prepend(ParseFlowSeqEnd)
          productions.prepend(ParseFlowSeqEntryOpt)
          new Right(Event(EventKind.SequenceStart(metadata), nextToken.range))
        case s: TokenKind.Scalar =>
          in.popToken()
          new Right(Event(EventKind.Scalar(s.value, s.scalarStyle, metadata), nextToken.range))
        case _ =>
          new Right(
            Event(
              EventKind.Scalar("", ScalarStyle.Plain, metadata.withTag(Tag.nullTag)),
              nextToken.range
            )
          )
      }
    case err => err.asInstanceOf[Either[YamlError, Event]]
  }

  private def parseNodeAttributes(
      tokenE: Either[YamlError, Token],
      metadata: NodeEventMetadata = NodeEventMetadata.empty
  ): Either[YamlError, (NodeEventMetadata, Token)] = tokenE match {
    case Right(token) =>
      token.kind match {
        case a: TokenKind.Anchor =>
          in.popToken()
          parseNodeAttributes(in.peekToken(), metadata.withAnchor(new Anchor(a.value)))
        case t: TokenKind.Tag =>
          in.popToken()
          t.value match {
            case _: TagValue.NonSpecific.type =>
              parseNodeAttributes(in.peekToken(), metadata)
            case v: TagValue.Verbatim =>
              parseNodeAttributes(in.peekToken(), metadata.withTag(new CustomTag(v.value)))
            case s: TagValue.Shorthand =>
              val handleKey = s.handle.value
              directives.get(handleKey) match {
                case Some(prefix) =>
                  val tagValue = prefix + s.rest
                  val tag =
                    if (Tag.coreSchemaValues.contains(tagValue)) new CoreSchemaTag(tagValue)
                    else new CustomTag(tagValue)
                  parseNodeAttributes(in.peekToken(), metadata.withTag(tag))
                case _ =>
                  new Left(ParseError.NoRegisteredTagDirective(handleKey, token))
              }
          }
        case _ => Right(metadata, token)
      }
    case err => err.asInstanceOf[Either[YamlError, (NodeEventMetadata, Token)]]
  }

  private def getNextEventImpl(): Either[YamlError, Event] = in.peekToken() match {
    case Right(token) =>
      val p = productions.removeHead()
      if (p eq ParseStreamStart) parseStreamStart(token)
      else if (p eq ParseStreamEnd) new Right(Event(EventKind.StreamEnd, token.range))
      else if (p eq ParseDocumentStart) parseDocumentStart(token)
      else if (p eq ParseDocumentEnd) parseDocumentEnd(token)
      else if (p eq ParseDocumentStartOpt) parseDocumentStartOpt(token)
      else if (p eq ParseNode) parseNode(token)
      else if (p eq ParseScalar) parseScalar(token)
      else if (p eq ParseMappingEnd) parseMappingEnd(token)
      else if (p eq ParseMappingEntry) parseMappingEntry(token)
      else if (p eq ParseMappingValue) parseMappingValue(token)
      else if (p eq ParseMappingValueNode) parseMappingValueNode(token)
      else if (p eq ParseMappingSequenceEnd) parseMappingSequenceEnd(token)
      else if (p eq ParseMappingEntryOpt) parseMappingEntryOpt(token)
      else if (p eq ParseSequenceEnd) parseSequenceEnd(token)
      else if (p eq ParseSequenceEntry) parseSequenceEntry(token)
      else if (p eq ParseSequenceEntryOpt) parseSequenceEntryOpt(token)
      else if (p eq ParseFlowNode) parseFlowNode(token)
      else if (p eq ParseFlowMappingEnd) parseFlowMappingEnd(token)
      else if (p eq ParseFlowMappingEntry) parseFlowMappingEntry(token)
      else if (p eq ParseFlowMappingEntryOpt) parseFlowMappingEntryOpt(token)
      else if (p eq ParseFlowMappingComma) parseFlowMappingComma(token)
      else if (p eq ParseFlowSeqEnd) parseFlowSeqEnd(token)
      else if (p eq ParseFlowSeqEntry) parseFlowSeqEntry(token)
      else if (p eq ParseFlowSeqEntryOpt) parseFlowSeqEntryOpt(token)
      else if (p eq ParseFlowSeqComma) parseFlowSeqComma(token)
      else if (p eq ParseFlowSeqPairKey) parseFlowPairKey(token)
      else if (p eq ParseFlowSeqPairValue) parseFlowPairValue(token)
      else new Right(p.asInstanceOf[ReturnEvent].produceEvent(token))
    case err => err.asInstanceOf[Either[YamlError, Event]]
  }
}

object ParserImpl {
  def apply(in: Tokenizer): ParserImpl = new ParserImpl(in)
}
