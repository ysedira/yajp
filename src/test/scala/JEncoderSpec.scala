import Json.JValue.*
import Json.*
import munit.*

class JEncoderSpec extends FunSuite {

  private def write[A](a: A)(using encoder: JEncoder[A]): JValue =
    encoder.write(a)

  test("byte is encoded correctly") {
    assertEquals(write[Byte](Byte.MaxValue), JNumber(Byte.MaxValue))
    assertEquals(write[Byte](Byte.MinValue), JNumber(Byte.MinValue))
  }

  test("Int is encoded correctly") {
    assertEquals(write[Int](Int.MaxValue), JNumber(Int.MaxValue))
    assertEquals(write[Int](Int.MinValue), JNumber(Int.MinValue))
  }
  test("Short is encoded correctly") {
    assertEquals(write[Short](Short.MaxValue), JNumber(Short.MaxValue))
    assertEquals(write[Short](Short.MinValue), JNumber(Short.MinValue))
  }
  test("Long is encoded correctly") {
    assertEquals(write[Long](Long.MaxValue), JNumber(Long.MaxValue))
    assertEquals(write[Long](Long.MinValue), JNumber(Long.MinValue))
  }
  test("Double is encoded correctly") {
    assertEquals(write[Double](Double.MaxValue), JNumber(Double.MaxValue))
    assertEquals(write[Double](Double.MinValue), JNumber(Double.MinValue))
  }
  test("Float is encoded correctly") {
    assertEquals(write[Float](Float.MaxValue), JNumber(Float.MaxValue))
    assertEquals(write[Float](Float.MinValue), JNumber(Float.MinValue))
  }
  test("BigInt is encoded correctly") {
    assertEquals(
      write[BigInt](BigInt(Int.MaxValue)),
      JNumber(BigInt(Int.MaxValue))
    )
    assertEquals(
      write[BigInt](BigInt(Int.MinValue)),
      JNumber(BigInt(Int.MinValue))
    )
  }
  test("BigDecimal is encoded correctly") {
    assertEquals(
      write[BigDecimal](BigDecimal(Double.MaxValue)),
      JNumber(BigDecimal(Double.MaxValue))
    )
    assertEquals(
      write[BigDecimal](BigDecimal(Double.MinValue)),
      JNumber(BigDecimal(Double.MinValue))
    )
  }
  test("String is encoded correctly") {
    assertEquals(
      write[String]("Some String Value"),
      JString("Some String Value")
    )
    assertEquals(
      write[String]("Some String Value with emojis 😄"),
      JString("Some String Value with emojis 😄")
    )
  }
  test("Char is encoded correctly") {
    assertEquals(write[Char]('V'), JString("V"))
  }

  test("Boolean is encoded correctly") {
    assertEquals(write[Boolean](true), JBoolean(true))
    assertEquals(write[Boolean](false), JBoolean(false))
  }

  test("Option is encoded correctly") {
    assertEquals(write[Option[Int]](Some(123)), JNumber(123))
    assertEquals(write[Option[Int]](None), JNull)
  }

  test("List is encoded correctly") {
    assertEquals(
      write[List[Int]](List(1, 2, 3)),
      JArray(JNumber(1), JNumber(2), JNumber(3))
    )
  }

  test("Map is encoded correctly") {
    assertEquals(
      write[Map[String, Int]](Map("a" -> 1, "b" -> 2, "c" -> 3)),
      JObject(Map("a" -> JNumber(1), "b" -> JNumber(2), "c" -> JNumber(3)))
    )
  }

}
