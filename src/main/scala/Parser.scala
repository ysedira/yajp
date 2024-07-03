import Json.JValue.*
import Json.*

import java.io.{
  ByteArrayInputStream,
  InputStream,
  InputStreamReader,
  PushbackReader
}
import java.nio.charset.StandardCharsets
import scala.collection.immutable
import scala.reflect.ClassTag

object Parser {

  def parse(input: String): JValue = {
    parse(new ByteArrayInputStream(input.getBytes(StandardCharsets.UTF_8)))
  }

  def parse(input: InputStream): JValue = {

    val reader = new PushbackReader(new InputStreamReader(input))
    parseValue(reader)
  }

  private def parseValue(reader: PushbackReader) = {
    var jValue: JValue = JNothing
    var stop = false
    while (reader.ready() && !stop) {
      val c = reader.read().toChar
      c match
        case c if c.isWhitespace => ()
        case 'n'                 => jValue = parseNull(reader)
        case 't'                 => jValue = parseTrue(reader)
        case 'f'                 => jValue = parseFalse(reader)
        case n if n == '-' || n.isDigit =>
          reader.unread(n)
          jValue = parseNumber(reader)
        case '"' => jValue = parseString(reader)
        case '[' => jValue = parseArray(reader)
        case ']' =>
          reader.unread(']')
          stop = true
        case '{' => jValue = parseObject(reader)
        case '}' =>
          reader.unread('}')
          stop = true
        case ',' => stop = true
        case _   => throw new IllegalArgumentException(s"Can't parse! ${c}")
    }
    jValue
  }

  private def parseObject(reader: PushbackReader): JObject = {
    var stop = false
    val acc = immutable.Map.newBuilder[String, JValue]
    var key: String = null
    while (reader.ready() && !stop)
      val c = reader.read().toChar
      if (c == '}') {
        stop = true
      } else if (c.isWhitespace) {} else if (c == ',') {} else if (c == ':') {
        val value = parseValue(reader)
        acc.addOne(key -> value)
      } else if (c == '"') {
        key = parseString(reader).value
      } else {
        throw new IllegalArgumentException("Aaaaaaah")
      }
    JObject(acc.result())
  }

  private def parseArray(reader: PushbackReader): JArray = {
    var stop = false
    val acc = immutable.VectorBuilder[JValue]()
    while (reader.ready() && !stop)
      val c = reader.read().toChar
      if (c == ']') {
        stop = true
      } else if (c.isWhitespace) {} else if (c == ',') {} else {
        reader.unread(c)
        val value = parseValue(reader)
        acc.addOne(value)
      }

    JArray(acc.result()*)
  }

  private def parseNull(reader: PushbackReader): JNull.type = {
    parseSingleton("ull", reader, JNull)
  }

  private def parseTrue(reader: PushbackReader): JTrue.type = {
    parseSingleton("rue", reader, JTrue)
  }

  private def parseFalse(reader: PushbackReader): JFalse.type = {
    parseSingleton("alse", reader, JFalse)
  }

  private def parseNumber(reader: PushbackReader): JNumber = {
    val f = reader.read().toChar
    var decimalPointEncountered = false
    var exponentEncountered = false
    var exponentSignEncountered = false
    var prevIsE = false
    var stop = false
    val isNegativeNumber = f == '-'
    val acc = new StringBuffer(f.toString)
    while (reader.ready() && !stop)
      val c = reader.read().toChar

      c match
        case d if d.isDigit =>
          acc.append(d)
          prevIsE = false
        case '.' if !exponentEncountered && !decimalPointEncountered =>
          decimalPointEncountered = true
          prevIsE = false
          acc.append('.')
        case '.' =>
          throw new IllegalArgumentException("Already consumed a decimal point")
        case 'e' if !exponentEncountered =>
          prevIsE = true
          exponentEncountered = true
          acc.append('e')
        case 'e' if exponentEncountered =>
          throw new IllegalArgumentException("Already consumed a e")
        case '-' if prevIsE =>
          prevIsE = false
          exponentSignEncountered = true
          acc.append('-')
        case '+' if prevIsE =>
          prevIsE = false
          exponentSignEncountered = true
          acc.append('+')
        case ' ' => stop = true
        case ']' =>
          reader.unread(']')
          stop = true
        case '}' =>
          reader.unread('}')
          stop = true
        case ',' =>
          reader.unread(',')
          stop = true

    JNumber(
      if (exponentEncountered || decimalPointEncountered)
        (acc.toString.toDouble)
      else acc.toString.toLong
    )
  }

  private def parseString(reader: PushbackReader): JString = {
    val acc = new StringBuffer()
    var stop = false
    var escape = false
    while (reader.ready() && !stop)
      val c = reader.read().toChar
      c match
        case '\\' if escape =>
          escape = false
          acc.append('\\')
        case '\\' =>
          escape = true
          acc.append('\\')
        case '"' if escape =>
          escape = false
          acc.append('"')
        case '"' =>
          escape = false
          stop = true
        case s =>
          escape = false
          acc.append(s)
    JString(acc.toString)
  }

  private def parseSingleton[A: ClassTag](
      pattern: String,
      reader: PushbackReader,
      value: A
  ): A =
    val buffer = pattern.toBuffer
    while (reader.ready() && buffer.nonEmpty) {
      val c = reader.read().toChar
      c match
        case c if c == buffer.remove(0) => ()
        case c =>
          throw new IllegalArgumentException(
            s"Can't parse ${ClassTag[A].getClass.getName}! got ${c}"
          )
    }
    value
}
