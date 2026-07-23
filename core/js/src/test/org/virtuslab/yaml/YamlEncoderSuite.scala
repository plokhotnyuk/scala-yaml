package org.virtuslab.yaml

import org.virtuslab.yaml.*
import org.virtuslab.yaml.Node.*
import org.virtuslab.yaml.internal.load.reader.token.ScalarStyle

class YamlEncoderSpec extends munit.FunSuite:
  val newline = System.lineSeparator()

  test("sequence of sequences") {
    val data = Seq(
      Seq(1, 2),
      Seq(3, 4)
    )

    val expected =
      s"""- 
         |  - 1
         |  - 2
         |- 
         |  - 3
         |  - 4
         |""".stripMargin

    assertEquals(data.asYaml, expected)
  }

  test("mapping of sequences") {
    case class Data(ints: Seq[Int], doubles: Seq[Double]) derives YamlCodec
    val data = Data(Seq(1, 2), Seq(3.0, 4.0))

    val expected =
      s"""ints: 
         |  - 1
         |  - 2
         |doubles: 
         |  - 3.0
         |  - 4.0
         |""".stripMargin

    assertEquals(data.asYaml, expected)
  }

  test("mapping of mappings (deep nesting)") {
    case class Data(map: Map[String, Map[String, String]]) derives YamlCodec
    val data = Data(Map("outer" -> Map("inner1" -> "val1", "inner2" -> "val2")))

    val expected =
      s"""map:
         |  outer:
         |    inner1: val1
         |    inner2: val2
         |""".stripMargin

    assertEquals(data.asYaml, expected)
  }

  test("sequence of mappings") {
    case class Data(seq: Seq[Map[String, String]]) derives YamlCodec
    val data = Data(Seq(Map("k1" -> "v1"), Map("k2" -> "v2")))

    val expected =
      s"""seq:
         |  -
         |    k1: v1
         |  -
         |    k2: v2
         |""".stripMargin

    assertEquals(data.asYaml, expected)
  }

  test("strings requiring double quoting - special prefix/suffix & edge cases") {
    val data = Seq(
      "",           // length 0
      "-",          // length 1
      "?",          // length 1
      ":",          // length 1
      "- ",         // starts with - and space
      "? ",         // starts with ? and space
      ": ",         // starts with : and space
      " a",         // starts with whitespace
      "a ",         // ends with whitespace
      ",a", "[a", "]a", "{a", "}a", "#a", "&a", "*a", "!a", "|a", ">a", "'a", "\"a", "%a", "@a", "`a"
    )

    val expected =
      s"""- ""
         |- "-"
         |- "?"
         |- ":"
         |- "- "
         |- "? "
         |- ": "
         |- " a"
         |- "a "
         |- ",a"
         |- "[a"
         |- "]a"
         |- "{a"
         |- "}a"
         |- "#a"
         |- "&a"
         |- "*a"
         |- "!a"
         |- "|a"
         |- ">a"
         |- "'a"
         |- "\\"a"
         |- "%a"
         |- "@a"
         |- "`a"
         |""".stripMargin

    assertEquals(data.asYaml, expected)
  }

  test("strings requiring double quoting - special substrings & control characters") {
    val data = Seq(
      " a",          // starts from " "
      "a:",          // trailing colon
      "true ",       // trailing whitespace
      "a: b",        // contains ": "
      "a # b",       // contains " #"
      "a\u001fb",    // contains c < 32
      "a\u007fb"     // contains c == 127
    )

    val expected =
      s"""- " a"
         |- "a: "
         |- "true "
         |- "a: b"
         |- "a # b"
         |- "a\\u001fb"
         |- "a\\u007fb"
         |""".stripMargin

    assertEquals(data.asYaml, expected)
  }

  test("strings requiring double quoting - yaml patterns (booleans, numbers, nulls)") {
    val data = Seq(
      "null", "Null", "~",
      "false", "False", "true", "True",
      "-1", "-1.1", "-.inf",
      "+1", "+1.1", "+.inf",
      "0", "0o7", "0x1A",
      "123", "1.23",
      ".NaN", ".inf"
    )

    val expected =
      s"""- "null"
         |- "Null"
         |- "~"
         |- "false"
         |- "False"
         |- "true"
         |- "True"
         |- "-1"
         |- "-1.1"
         |- "-.inf"
         |- "+1"
         |- "+1.1"
         |- "+.inf"
         |- "0"
         |- "0o7"
         |- "0x1A"
         |- "123"
         |- "1.23"
         |- ".NaN"
         |- ".inf"
         |""".stripMargin

    assertEquals(data.asYaml, expected)
  }

  test("escape characters in double quoted strings") {
    // String starts with space to enforce double quoting logic fallback,
    // thereby hitting the escapeDoubleQuoted switch branch exactly.
    val data = Seq(" \n\r\t\b\f\"\\")

    val expected =
      s"""- " \\n\\r\\t\\b\\f\\"\\\\"
         |""".stripMargin

    assertEquals(data.asYaml, expected)
  }

  test("primitives serialization (plain styles)") {
    case class Primitives(b: Boolean, i: Int, f: Double) derives YamlCodec
    val data = Primitives(true, 42, 3.14)

    val expected =
      s"""b: true
         |i: 42
         |f: 3.14
         |""".stripMargin

    assertEquals(data.asYaml, expected)
  }

  test("single quoted string encoding") {
    // Force Strings to be encoded using SingleQuoted style for this test
    given singleQuotedEncoder: YamlEncoder[String] = new YamlEncoder[String]:
    override def asNode(obj: String): Node =
      Node.ScalarNode(obj, Tag.str, ScalarStyle.SingleQuoted)

    val newline = System.lineSeparator()
    assertEquals(Seq("").asYaml, s"- ''$newline")
    assertEquals(Seq("plain text").asYaml, s"- 'plain text'$newline")
    assertEquals(Seq("'").asYaml, s"- ''''$newline")
    assertEquals(Seq("O'Connor").asYaml, s"- 'O''Connor'$newline")
    assertEquals(Seq("'''").asYaml, s"- '''''''''$newline")
    assertEquals(Seq("'quotes around'").asYaml, s"- '''quotes around'''$newline")
    assertEquals(Map("key" -> "value's").asYaml, s"key: 'value''s'$newline")
    assertEquals(Map("let's" -> "go").asYaml, s"'let''s': 'go'$newline")
    assertEquals(Seq("\"double quotes\"").asYaml, s"- '\"double quotes\"'$newline")
    assertEquals(Seq("\\backslashes\\").asYaml, s"- '\\backslashes\\'$newline")
  }

  test("literal string encoding - chomping and indentation") {
    // Force Strings to be encoded using Literal style for this test
    given literalEncoder: YamlEncoder[String] = new YamlEncoder[String]:
    override def asNode(obj: String): Node =
      Node.ScalarNode(obj, Tag.str, ScalarStyle.Literal)

    assertEquals("".asYaml, s"|-$newline$newline")
    assertEquals("\n".asYaml, s"|$newline\n$newline")
    assertEquals("\n\n".asYaml, s"|+$newline\n\n$newline")
    assertEquals("abc\ndef".asYaml, s"|-$nlabc\ndef$newline")
    assertEquals("abc\n".asYaml, s"|$nlabc\n$newline")
    assertEquals(Map("empty" -> "").asYaml, s"empty: |-$newline$newline")
    assertEquals(Map("singleNl" -> "\n").asYaml, s"singleNl: |$newline\n$newline")
    assertEquals(Map("doubleNl" -> "\n\n").asYaml, s"doubleNl: |+$newline\n\n$newline")
    assertEquals(Map("strip" -> "abc").asYaml, s"strip: |-$newline  abc$newline")
    assertEquals(Map("clip" -> "abc\n").asYaml, s"clip: |$newline  abc\n$newline")
    assertEquals(Map("keep" -> "abc\n\n").asYaml, s"keep: |+$newline  abc\n\n$newline")
    assertEquals(Map("multiline" -> "abc\ndef").asYaml, s"multiline: |-$newline  abc\n  def$newline")
    assertEquals(Map("emptyLines" -> "abc\n\ndef").asYaml, s"emptyLines: |-$newline  abc\n\n  def$newline")
    assertEquals(Map("crlfClip" -> "abc\r\n").asYaml, s"crlfClip: |$newline  abc\r\n$newline")
    assertEquals(Map("crlfKeep" -> "abc\r\n\r\n").asYaml, s"crlfKeep: |+$newline  abc\r\n\r\n$newline")
    assertEquals(Map("crlfMultiline" -> "abc\r\ndef").asYaml, s"crlfMultiline: |-$newline  abc\r\n  def$newline")
  }

  test("folded string encoding - chomping and indentation") {
    // Force Strings to be encoded using Folded style for this test
    given foldedEncoder: YamlEncoder[String] = new YamlEncoder[String]:
    override def asNode(obj: String): Node =
      Node.ScalarNode(obj, Tag.str, ScalarStyle.Folded)

    assertEquals("".asYaml, s">-$newline$newline")
    assertEquals("\n".asYaml, s">$newline\n$newline")
    assertEquals("\n\n".asYaml, s">+$newline\n\n$newline")
    assertEquals("abc\ndef".asYaml, s">-$nlabc\ndef$newline")
    assertEquals("abc\n".asYaml, s">$nlabc\n$newline")
    assertEquals(Map("empty" -> "").asYaml, s"empty: >-$newline$newline")
    assertEquals(Map("singleNl" -> "\n").asYaml, s"singleNl: >$newline\n$newline")
    assertEquals(Map("doubleNl" -> "\n\n").asYaml, s"doubleNl: >+$newline\n\n$newline")
    assertEquals(Map("strip" -> "abc").asYaml, s"strip: >-$newline  abc$newline")
    assertEquals(Map("clip" -> "abc\n").asYaml, s"clip: >$newline  abc\n$newline")
    assertEquals(Map("keep" -> "abc\n\n").asYaml, s"keep: >+$newline  abc\n\n$newline")
    assertEquals(Map("multiline" -> "abc\ndef").asYaml, s"multiline: >-$newline  abc\n  def$newline")
    assertEquals(Map("emptyLines" -> "abc\n\ndef").asYaml, s"emptyLines: >-$newline  abc\n\n  def$newline")
    assertEquals(Map("crlfClip" -> "abc\r\n").asYaml, s"crlfClip: >$newline  abc\r\n$newline")
    assertEquals(Map("crlfKeep" -> "abc\r\n\r\n").asYaml, s"crlfKeep: >+$newline  abc\r\n\r\n$newline")
    assertEquals(Map("crlfMultiline" -> "abc\r\ndef").asYaml, s"crlfMultiline: >-$newline  abc\r\n  def$newline")
  }
