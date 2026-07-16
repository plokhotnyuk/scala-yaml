package org.virtuslab.yaml.internal.load.reader.token

sealed abstract class ScalarStyle(indicator: Char)
object ScalarStyle {
  case object Plain        extends ScalarStyle(' ')
  case object DoubleQuoted extends ScalarStyle('"')
  case object SingleQuoted extends ScalarStyle('\'')
  case object Folded       extends ScalarStyle('>')
  case object Literal      extends ScalarStyle('|')

  def escapeSpecialCharacter(scalar: String, scalarStyle: ScalarStyle): String =
    if (
      (scalarStyle eq ScalarStyle.DoubleQuoted) ||
      (scalarStyle eq ScalarStyle.SingleQuoted) ||
      (scalarStyle eq ScalarStyle.Literal)
    ) scalar
    else scalar.replace("\\", "\\\\").replace("\n", "\\n")

  def escapeSpecialCharacterDoubleQuote(scalar: String): String = scalar.replace("\n", "")
}
