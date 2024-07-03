import Json.JValue.*
import Json.*
import munit.*

class JCodecSpec extends FunSuite {

  private def write[A](a: A)(using encoder: JEncoder[A]): JValue =
    encoder.write(a)

  private def read[A](json: JValue)(using decoder: JDecoder[A]): A =
    decoder.read(json)

  private def readWritten[A](a: A)(using codec: JCodec[A]): Unit =
    assertEquals(read(write(a)), a)

  test("byte is encoded & decoded correctly") {
    readWritten[Byte](Byte.MaxValue)
    readWritten[Byte](Byte.MinValue)
  }

  test("Int is encoded & decoded correctly") {
    readWritten[Int](Int.MaxValue)
    readWritten[Int](Int.MinValue)
  }
  test("Short is encoded & decoded correctly") {
    readWritten[Short](Short.MaxValue)
    readWritten[Short](Short.MinValue)
  }
  test("Long is encoded & decoded correctly") {
    readWritten[Long](Long.MaxValue)
    readWritten[Long](Long.MinValue)
  }
  test("Double is encoded & decoded correctly") {
    readWritten[Double](Double.MaxValue)
    readWritten[Double](Double.MinValue)
  }
  test("Float is encoded & decoded correctly") {
    readWritten[Float](Float.MaxValue)
    readWritten[Float](Float.MinValue)
  }
  test("BigInt is encoded & decoded correctly") {
    write[BigInt](BigInt(Int.MaxValue))
    write[BigInt](BigInt(Int.MinValue))
  }
  test("BigDecimal is encoded & decoded correctly") {
    readWritten[BigDecimal](BigDecimal(Double.MaxValue))
    readWritten[BigDecimal](BigDecimal(Double.MinValue))
  }
  test("String is encoded & decoded correctly") {
    readWritten[String]("Some String Value")
    readWritten[String]("Some String Value with emojis 😄")
  }
  test("Char is encoded & decoded correctly") {
    readWritten[Char]('V')
  }

  test("Boolean is encoded & decoded correctly") {
    readWritten[Boolean](true)
    readWritten[Boolean](false)
  }

  test("Option is encoded & decoded correctly") {
    readWritten[Option[Int]](Some(123))
    readWritten[Option[Int]](None)
  }

  test("List is encoded & decoded correctly") {
    readWritten[List[Int]](List(1, 2, 3))
  }

  test("Map is encoded & decoded correctly") {
    readWritten[Map[String, Int]](Map("a" -> 1, "b" -> 2, "c" -> 3))
  }
}
