package org.virtuslab.yaml

import org.virtuslab.yaml.*
import org.virtuslab.yaml.Node.*
import org.virtuslab.yaml.internal.load.reader.token.ScalarStyle

class YamlEncoderSpec extends munit.FunSuite {
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
    case class Data(map: Map[String, Map[String, String]])
    derives YamlCodec

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
      "", // length 0
      "-", // length 1
      "?", // length 1
      ":", // length 1
      "- ", // starts with - and space
      "? ", // starts with ? and space
      ": ", // starts with : and space
      " a", // starts with whitespace
      "a ", // ends with whitespace
      ",a", "[a", "]a", "{a", "}a", "#a", "&a", "*a", "!a", "|a", ">a", "'a", "\"a", "%a", "@a", "`a"
    )
    val expected =
      s"""- !!null
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
      " a", // starts from " "
      "a:", // trailing colon
      "true ", // trailing whitespace
      "a: b", // contains ": "
      "a # b", // contains " #"
      "a\u001fb", // contains c < 32
      "a\u007fb" // contains c == 127
    )
    val expected =
      s"""- " a"
         |- "a:"
         |- "true "
         |- "a: b"
         |- "a # b"
         |- "a\\u001Fb"
         |- "a\\u007Fb"
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
}
