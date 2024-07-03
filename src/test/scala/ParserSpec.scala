import Json.JValue.*
import Json.*
import munit.*

class ParserSpec extends FunSuite {

  test("Parser is able to parse null") {
    val result = Parser.parse("null")
    assert(result == JNull)
  }
  test("Parser is able to parse true") {
    val result = Parser.parse("true")
    assert(result == JTrue)
  }
  test("Parser is able to parse false") {
    val result = Parser.parse("false")
    assert(result == JFalse)
  }

  test("Parser is able to parse simple int") {
    val result = Parser.parse("123")
    assert(result == JValue.JNumber(123))
  }
  test("Parser is able to parse simple negative int") {
    val result = Parser.parse("-123")
    assert(result == JValue.JNumber(-123))
  }
  test("Parser is able to parse decimal number") {
    val result = Parser.parse("-123.456")
    assert(result == JValue.JNumber(-123.456))
  }
  test("Parser is able to parse exp number") {
    val result = Parser.parse("-1e4")
    assert(result == JValue.JNumber(-1e4))
  }
  test("Parser is able to parse exp number with negative exponent") {
    val result = Parser.parse("-1e-4")
    assert(result == JValue.JNumber(-1e-4))
  }
  test("Parser is able to parse exp number with positive exponent") {
    val result = Parser.parse("-2.3e+4")
    assert(result == JValue.JNumber(-2.3e4))
  }
  test("Parser is able to parse a simple string") {
    val result = Parser.parse(""""abc"""")
    assert(result == JValue.JString("""abc"""))
  }
  test("Parser is able to parse a string with escaped chars") {
    val result = Parser.parse("\"abc\\\"/\b\f\n\r\tcd  d\"")
    assert(result == JValue.JString("abc\\\"/\b\f\n\r\tcd  d"))
  }
  test("Parser is able to parse an array") {
    val result = Parser.parse("[1,2,3,4,5]")
    assert(
      result == JArray(
        JNumber(1),
        JNumber(2),
        JNumber(3),
        JNumber(4),
        JNumber(5)
      )
    )
  }
  test("Parser is able to parse nested array") {
    val result = Parser.parse("[[1,2,3],[4,5],[]]")
    assert(
      result == JArray(
        JArray(JNumber(1), JNumber(2), JNumber(3)),
        JArray(JNumber(4), JNumber(5)),
        JArray()
      )
    )
  }
  test("Parser is able to parse an array with different element types") {
    val result = Parser.parse("[1,\"2\",true,false,null]")
    assert(result == JArray(JNumber(1), JString("2"), JTrue, JFalse, JNull))
  }
  test("Parser is able to parse an object") {
    val result = Parser.parse("""{"a":123}""")
    assert(result == JObject(Map("a" -> JNumber(123))))
  }
  test("Parser is able to parse a bigger object") {
    val result = Parser.parse("""{
        |"a":123,
        |"b":true,
        |"c":false,
        |"d":null,
        |"e":[ 1 , "2" , true , false , null , {"a":456}]
        |}""".stripMargin)
    assert(
      result == JObject(
        Map(
          "a" -> JNumber(123),
          "b" -> JTrue,
          "c" -> JFalse,
          "d" -> JNull,
          "e" -> JArray(
            JNumber(1),
            JString("2"),
            JTrue,
            JFalse,
            JNull,
            JObject(Map("a" -> JNumber(456)))
          )
        )
      )
    )
  }

  test("Parser fails when input is invalid") {
    intercept[IllegalArgumentException](Parser.parse("abc"))
    intercept[IllegalArgumentException](Parser.parse("{abc:def}"))
  }

}
