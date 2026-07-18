package org.virtuslab.yaml

import org.virtuslab.yaml.internal.load.reader.token.ScalarStyle

class TagSuite extends munit.FunSuite {
  test("resolveTag resolves mixed-case null to nullTag") {
    assertEquals(Tag.resolveTag(null), Tag.nullTag)
    assertEquals(Tag.resolveTag(""), Tag.nullTag)
    assertEquals(Tag.resolveTag("null"), Tag.nullTag)
    assertEquals(Tag.resolveTag("Null"), Tag.nullTag)
    assertEquals(Tag.resolveTag("NULL"), Tag.nullTag)
    assertEquals(Tag.resolveTag("~"), Tag.nullTag)
  }

  test("resolveTag resolves decimal integers to int") {
    assertEquals(Tag.resolveTag("0"), Tag.int)
    assertEquals(Tag.resolveTag("-0"), Tag.int)
    assertEquals(Tag.resolveTag("+0"), Tag.int)
    assertEquals(Tag.resolveTag("1"), Tag.int)
    assertEquals(Tag.resolveTag("+1"), Tag.int)
    assertEquals(Tag.resolveTag("-1"), Tag.int)
    assertEquals(Tag.resolveTag("0123456789"), Tag.int)
  }

  test("resolveTag resolves octal integers to int") {
    assertEquals(Tag.resolveTag("0o7"), Tag.int)
    assertEquals(Tag.resolveTag("0o01234567"), Tag.int)
  }

  test("resolveTag resolves hex integers to int") {
    assertEquals(Tag.resolveTag("0xFF"), Tag.int)
    assertEquals(Tag.resolveTag("0x3A"), Tag.int)
    assertEquals(Tag.resolveTag("0x0123456789ABCDEF"), Tag.int)
    assertEquals(Tag.resolveTag("0xabcdef"), Tag.int)
    assertEquals(Tag.resolveTag("0x1a2b3c"), Tag.int)
  }

  test("resolveTag resolves decimal numbers, infinities, and nans to float") {
    assertEquals(Tag.resolveTag("0.0"), Tag.float)
    assertEquals(Tag.resolveTag("-0.0"), Tag.float)
    assertEquals(Tag.resolveTag("+.5"), Tag.float)
    assertEquals(Tag.resolveTag("-.5"), Tag.float)
    assertEquals(Tag.resolveTag(".1"), Tag.float)
    assertEquals(Tag.resolveTag("1."), Tag.float)
    assertEquals(Tag.resolveTag("1.1"), Tag.float)
    assertEquals(Tag.resolveTag("+1.1"), Tag.float)
    assertEquals(Tag.resolveTag("-1.1"), Tag.float)
    assertEquals(Tag.resolveTag("1e1"), Tag.float)
    assertEquals(Tag.resolveTag("1E1"), Tag.float)
    assertEquals(Tag.resolveTag("1E+1"), Tag.float)
    assertEquals(Tag.resolveTag("1E-1"), Tag.float)
    assertEquals(Tag.resolveTag("-1e-5"), Tag.float)
    assertEquals(Tag.resolveTag("+1E+5"), Tag.float)
    assertEquals(Tag.resolveTag(".inf"), Tag.float)
    assertEquals(Tag.resolveTag(".Inf"), Tag.float)
    assertEquals(Tag.resolveTag(".INF"), Tag.float)
    assertEquals(Tag.resolveTag("+.inf"), Tag.float)
    assertEquals(Tag.resolveTag("+.Inf"), Tag.float)
    assertEquals(Tag.resolveTag("+.INF"), Tag.float)
    assertEquals(Tag.resolveTag("-.inf"), Tag.float)
    assertEquals(Tag.resolveTag("-.Inf"), Tag.float)
    assertEquals(Tag.resolveTag("-.INF"), Tag.float)
    assertEquals(Tag.resolveTag(".nan"), Tag.float)
    assertEquals(Tag.resolveTag(".NaN"), Tag.float)
    assertEquals(Tag.resolveTag(".NAN"), Tag.float)
  }

  test("resolveTag resolves mixed-case true to boolean") {
    assertEquals(Tag.resolveTag("true"), Tag.boolean)
    assertEquals(Tag.resolveTag("True"), Tag.boolean)
    assertEquals(Tag.resolveTag("TRUE"), Tag.boolean)
  }

  test("resolveTag resolves mixed-case false to boolean") {
    assertEquals(Tag.resolveTag("false"), Tag.boolean)
    assertEquals(Tag.resolveTag("False"), Tag.boolean)
    assertEquals(Tag.resolveTag("FALSE"), Tag.boolean)
  }

  test("resolveTag resolves string value to str") {
    assertEquals(Tag.resolveTag("", Some(ScalarStyle.DoubleQuoted)), Tag.str)
    assertEquals(Tag.resolveTag("abc", Some(ScalarStyle.DoubleQuoted)), Tag.str)
    assertEquals(Tag.resolveTag("123", Some(ScalarStyle.DoubleQuoted)), Tag.str)
    assertEquals(Tag.resolveTag("-123", Some(ScalarStyle.DoubleQuoted)), Tag.str)
    assertEquals(Tag.resolveTag("+123", Some(ScalarStyle.DoubleQuoted)), Tag.str)
    assertEquals(Tag.resolveTag("true", Some(ScalarStyle.DoubleQuoted)), Tag.str)
    assertEquals(Tag.resolveTag("null", Some(ScalarStyle.DoubleQuoted)), Tag.str)
    assertEquals(Tag.resolveTag("", Some(ScalarStyle.SingleQuoted)), Tag.str)
    assertEquals(Tag.resolveTag("abc", Some(ScalarStyle.SingleQuoted)), Tag.str)
    assertEquals(Tag.resolveTag("123", Some(ScalarStyle.SingleQuoted)), Tag.str)
    assertEquals(Tag.resolveTag("true", Some(ScalarStyle.SingleQuoted)), Tag.str)
    assertEquals(Tag.resolveTag("null", Some(ScalarStyle.SingleQuoted)), Tag.str)
    assertEquals(Tag.resolveTag("none"), Tag.str)
    assertEquals(Tag.resolveTag("Nil"), Tag.str)
    assertEquals(Tag.resolveTag("0ish"), Tag.str)
    assertEquals(Tag.resolveTag("1st"), Tag.str)
    assertEquals(Tag.resolveTag("2nd"), Tag.str)
    assertEquals(Tag.resolveTag("3rd"), Tag.str)
    assertEquals(Tag.resolveTag("4th"), Tag.str)
    assertEquals(Tag.resolveTag("5th"), Tag.str)
    assertEquals(Tag.resolveTag("6th"), Tag.str)
    assertEquals(Tag.resolveTag("7th"), Tag.str)
    assertEquals(Tag.resolveTag("8th"), Tag.str)
    assertEquals(Tag.resolveTag("9th"), Tag.str)
    assertEquals(Tag.resolveTag("test"), Tag.str)
    assertEquals(Tag.resolveTag("Turin"), Tag.str)
    assertEquals(Tag.resolveTag("fix"), Tag.str)
    assertEquals(Tag.resolveTag("France"), Tag.str)
    assertEquals(Tag.resolveTag(".com"), Tag.str)
    assertEquals(Tag.resolveTag("~3x times"), Tag.str)
    assertEquals(Tag.resolveTag("+.nan"), Tag.str)
    assertEquals(Tag.resolveTag("-.nan"), Tag.str)
    assertEquals(Tag.resolveTag("0o8"), Tag.str)   // '8' is invalid in octal
    assertEquals(Tag.resolveTag("0o"), Tag.str)    // Missing digits
    assertEquals(Tag.resolveTag("0xG"), Tag.str)   // 'G' is invalid in hex
    assertEquals(Tag.resolveTag("0x"), Tag.str)    // Missing digits
    assertEquals(Tag.resolveTag("1.1.1"), Tag.str) // Multiple decimal points
    assertEquals(Tag.resolveTag("1e"), Tag.str)    // Missing exponent digits
    assertEquals(Tag.resolveTag("1e1.5"), Tag.str) // Decimal inside exponent
    assertEquals(Tag.resolveTag(".e1"), Tag.str)   // Exponent without leading digits
    assertEquals(Tag.resolveTag(".-1"), Tag.str)   // Invalid negative after decimal
    assertEquals(Tag.resolveTag("nuLL"), Tag.str)  // Null strictness
    assertEquals(Tag.resolveTag("NulL"), Tag.str)  // Null strictness
    assertEquals(Tag.resolveTag("tRue"), Tag.str)  // Boolean strictness
    assertEquals(Tag.resolveTag("fAlse"), Tag.str) // Boolean strictness
    assertEquals(Tag.resolveTag("trueish"), Tag.str)
    assertEquals(Tag.resolveTag("falsehood"), Tag.str)
    assertEquals(Tag.resolveTag("nullo"), Tag.str)
    assertEquals(Tag.resolveTag("inf"), Tag.str)
    assertEquals(Tag.resolveTag("-inf"), Tag.str)
    assertEquals(Tag.resolveTag("+inf"), Tag.str)
    assertEquals(Tag.resolveTag("NaN"), Tag.str)
    assertEquals(Tag.resolveTag("-"), Tag.str)
    assertEquals(Tag.resolveTag("+"), Tag.str)
    assertEquals(Tag.resolveTag("."), Tag.str)
    assertEquals(Tag.resolveTag("-abc"), Tag.str)
    assertEquals(Tag.resolveTag("+abc"), Tag.str)
  }
}
