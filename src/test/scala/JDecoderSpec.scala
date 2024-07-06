import Json.JValue.*
import Json.*
import munit.*

class JDecoderSpec extends FunSuite {

  private def read[A](json: JValue)(using decoder: JDecoder[A]): A =
    decoder.read(json)

  test("byte is decoded correctly") {
    assertEquals(read[Byte](JNumber(Byte.MaxValue)), Byte.MaxValue)
    assertEquals(read[Byte](JNumber(Byte.MinValue)), Byte.MinValue)
  }

  test("Int is decoded correctly") {
    assertEquals(read[Int](JNumber(Int.MaxValue)), Int.MaxValue)
    assertEquals(read[Int](JNumber(Int.MinValue)), Int.MinValue)
  }
  test("Short is decoded correctly") {
    assertEquals(read[Short](JNumber(Short.MaxValue)), Short.MaxValue)
    assertEquals(read[Short](JNumber(Short.MinValue)), Short.MinValue)
  }
  test("Long is decoded correctly") {
    assertEquals(read[Long](JNumber(Long.MaxValue)), Long.MaxValue)
    assertEquals(read[Long](JNumber(Long.MinValue)), Long.MinValue)
  }
  test("Double is decoded correctly") {
    assertEquals(read[Double](JNumber(Double.MaxValue)), Double.MaxValue)
    assertEquals(read[Double](JNumber(Double.MinValue)), Double.MinValue)
  }
  test("Float is decoded correctly") {
    assertEquals(read[Float](JNumber(Float.MaxValue)), Float.MaxValue)
    assertEquals(read[Float](JNumber(Float.MinValue)), Float.MinValue)
  }
  test("BigInt is decoded correctly") {
    assertEquals(
      read[BigInt](JNumber(BigInt(Int.MaxValue))),
      BigInt(Int.MaxValue)
    )
    assertEquals(
      read[BigInt](JNumber(BigInt(Int.MinValue))),
      BigInt(Int.MinValue)
    )
  }
  test("BigDecimal is decoded correctly") {
    assertEquals(
      read[BigDecimal](JNumber(BigDecimal(Double.MaxValue))),
      BigDecimal(Double.MaxValue)
    )
    assertEquals(
      read[BigDecimal](JNumber(BigDecimal(Double.MinValue))),
      BigDecimal(Double.MinValue)
    )
  }
  test("String is decoded correctly") {
    assertEquals(
      read[String](JString("Some String Value")),
      "Some String Value"
    )
    assertEquals(
      read[String](JString("Some String Value with emojis 😄")),
      "Some String Value with emojis 😄"
    )
  }
  test("Char is decoded correctly") {
    assertEquals(read[Char](JString("V")), 'V')
  }

  test("Boolean is decoded correctly") {
    assertEquals(read[Boolean](JBoolean(true)), true)
    assertEquals(read[Boolean](JBoolean(false)), false)
  }

  test("Option is decoded correctly") {
    assertEquals(read[Option[Int]](JNumber(123)), Some(123))
    assertEquals(read[Option[Int]](JNull), None)
    assertEquals(read[Option[Int]](JNothing), None)
  }

  test("List is decoded correctly") {
    assertEquals(
      read[List[Int]](JArray(JNumber(1), JNumber(2), JNumber(3))),
      List(1, 2, 3)
    )
  }

  test("Map is decoded correctly") {
    assertEquals(
      read[Map[String, Int]](
        JObject(Map("a" -> JNumber(1), "b" -> JNumber(2), "c" -> JNumber(3)))
      ),
      Map("a" -> 1, "b" -> 2, "c" -> 3)
    )
  }
  test("case class is decoded correctly") {
    case class A(
        a: Int = 1,
        b: String,
        c: Boolean,
        d: Option[String],
        e: List[Int]
    ) derives JDecoder
    val a = A(123, "abc", true, Some("def"), List(1, 2, 3))
    assertEquals(
      read[A](
        JObject(
          Map(
            "a" -> JNumber(123),
            "b" -> JString("abc"),
            "c" -> JBoolean(true),
            "d" -> JString("def"),
            "e" -> JArray(JNumber(1), JNumber(2), JNumber(3))
          )
        )
      ),
      a
    )
    intercept[IllegalArgumentException] {
      read[A](JObject(Map("a" -> JNumber(123), "b" -> JString("abc"))))
    }
  }

  test("enum is decoded correctly") {
    enum Side derives JDecoder:
      case Left, Right
    assertEquals(
      read[Side](JObject(Map("_kind" -> JString("Left")))),
      Side.Left
    )
    assertEquals(
      read[Side](JObject(Map("_kind" -> JString("Right")))),
      Side.Right
    )
    intercept[IllegalArgumentException] {
      read[Side](JObject(Map("_kind" -> JNumber(1))))
    }
  }

}
