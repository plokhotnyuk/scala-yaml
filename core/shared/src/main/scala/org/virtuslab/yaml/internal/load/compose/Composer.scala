package org.virtuslab.yaml.internal.load.compose

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.immutable.ListMap

import org.virtuslab.yaml.ComposerError
import org.virtuslab.yaml.Node
import org.virtuslab.yaml.Range
import org.virtuslab.yaml.Tag
import org.virtuslab.yaml.YamlError
import org.virtuslab.yaml.internal.load.parse.Anchor
import org.virtuslab.yaml.internal.load.parse.Event
import org.virtuslab.yaml.internal.load.parse.EventKind

/**
 * Composing takes a series of serialization events and produces a representation graph.
 * It can fail due to any of several reasons e.g. unexpected event.
 * Returns either [[YamlError]] or [[Node]](s)
 */
trait Composer {
  def fromEvents(events: List[Event]): Either[YamlError, Node]
  def multipleFromEvents(events: List[Event]): Either[YamlError, List[Node]]
}

object ComposerImpl extends Composer {
  // A lightweight mutable wrapper avoiding `Result` allocation per event.
  private class Context(var events: List[Event])

  override def fromEvents(events: List[Event]): Either[YamlError, Node] = events match {
    case Nil => new Left(ComposerError("No events available"))
    case _   => composeNode(new Context(events), mutable.Map.empty)
  }

  override def multipleFromEvents(events: List[Event]): Either[YamlError, List[Node]] = {
    val aliases = mutable.Map.empty[Anchor, Node]
    val ctx     = new Context(events)

    @tailrec
    def go(out: mutable.ListBuffer[Node]): Either[YamlError, List[Node]] =
      ctx.events match {
        case e :: tail =>
          e.kind match {
            case _: EventKind.StreamEnd.type =>
              ctx.events = tail
              new Right(out.toList)
            case _: EventKind.StreamStart.type | _: EventKind.DocumentEnd =>
              ctx.events = tail
              go(out)
            case _ =>
              composeNode(ctx, aliases) match {
                case Right(node) =>
                  out.addOne(node)
                  go(out)
                case err => err.asInstanceOf[Either[YamlError, List[Node]]]
              }
          }
        case Nil => new Right(out.toList)
      }

    go(new mutable.ListBuffer[Node])
  }

  private def composeNode(
      ctx: Context,
      aliases: mutable.Map[Anchor, Node]
  ): Either[YamlError, Node] = ctx.events match {
    case head :: tail =>
      // Advance the pointer so that recursive calls see the remaining sequence
      ctx.events = tail
      head.kind match {
        case s: EventKind.Scalar =>
          val tag: Tag = s.metadata.tag.getOrElse(Tag.resolveTag(s.value, new Some(s.style)))
          val node     = new Node.ScalarNode(s.value, tag, head.pos)
          s.metadata.anchor.foreach(anchor => aliases.put(anchor, node))
          new Right(node)
        case ss: EventKind.SequenceStart =>
          composeSequenceNode(ctx, ss.metadata.anchor, aliases)
        case ms: EventKind.MappingStart =>
          composeMappingNode(ctx, ms.metadata.anchor, aliases)
        case a: EventKind.Alias =>
          aliases.get(a.id) match {
            case Some(node) => new Right(node)
            case None       => new Left(ComposerError(s"There is no anchor for ${a.id} alias"))
          }
        case _: EventKind.StreamStart.type | _: EventKind.DocumentStart =>
          composeNode(ctx, aliases)
        case event => new Left(ComposerError(s"Expected YAML node, but found: $event"))
      }
    case Nil => new Left(ComposerError("No events available"))
  }

  private def composeSequenceNode(
      ctx: Context,
      anchorOpt: Option[Anchor],
      aliases: mutable.Map[Anchor, Node]
  ): Either[YamlError, Node.SequenceNode] = {

    @tailrec
    def parseChildren(
        children: mutable.ListBuffer[Node],
        firstChildPos: Option[Range] = None
    ): Either[YamlError, Node.SequenceNode] = {
      ctx.events match {
        case e :: tail =>
          e.kind match {
            case _: EventKind.SequenceEnd.type =>
              ctx.events = tail
              val sequence = new Node.SequenceNode(children.toList, Tag.seq, firstChildPos)
              if (anchorOpt.isDefined) aliases.put(anchorOpt.get, sequence)
              new Right(sequence)
            case _ =>
              composeNode(ctx, aliases) match {
                case Right(node) =>
                  children.addOne(node)
                  val nextPos =
                    if (firstChildPos.isEmpty) node.pos
                    else firstChildPos
                  parseChildren(children, nextPos)
                case err => err.asInstanceOf[Either[YamlError, Node.SequenceNode]]
              }
          }
        case Nil => new Left(ComposerError("Not found SequenceEnd event for sequence"))
      }
    }

    parseChildren(new mutable.ListBuffer[Node]())
  }

  private def composeMappingNode(
      ctx: Context,
      anchorOpt: Option[Anchor],
      aliases: mutable.Map[Anchor, Node]
  ): Either[YamlError, Node.MappingNode] = {

    @tailrec
    def parseMappings(
        mappingsBuffer: mutable.ListBuffer[(Node, Node)],
        firstChildPos: Option[Range] = None
    ): Either[YamlError, Node.MappingNode] = {
      ctx.events match {
        case e :: tail =>
          e.kind match {
            case _: EventKind.MappingEnd.type =>
              ctx.events = tail
              val mapping =
                new Node.MappingNode(ListMap.from(mappingsBuffer), Tag.map, firstChildPos)
              if (anchorOpt.isDefined) aliases.put(anchorOpt.get, mapping)
              Right(mapping)
            case _: EventKind.StreamStart.type | _: EventKind.StreamEnd.type |
                _: EventKind.DocumentStart | _: EventKind.DocumentEnd =>
              Left(ComposerError(s"Invalid event, got: ${e.kind}, expected Node"))

            case _ =>
              composeNode(ctx, aliases) match {
                case Right(keyNode) =>
                  composeNode(ctx, aliases) match {
                    case Right(vNode) =>
                      mappingsBuffer.addOne((keyNode, vNode))
                      parseMappings(mappingsBuffer, keyNode.pos)
                    case Left(err) => Left(err)
                  }
                case Left(err) => Left(err)
              }
          }
        case Nil => Left(ComposerError("Not found MappingEnd event for mapping"))
      }
    }

    parseMappings(new mutable.ListBuffer[(Node, Node)])
  }
}
