import Json.JValue
import scala.CanEqual.derived
import scala.deriving.Mirror
import scala.compiletime.{erasedValue, summonInline, constValue}
import scala.compiletime.error
import scala.collection.AbstractIterable

trait JDecoder[A]:
  def read(json: JValue): A
end JDecoder

object JDecoder:
  given byteDecoder: JDecoder[Byte] = new JDecoder[Byte]:
    override def read(json: JValue): Byte = json match {
      case JValue.JNumber(i) => Byte.unbox(i)
      case _                 => throw IllegalArgumentException("Not a number")
    }
  given intDecoder: JDecoder[Int] = new JDecoder[Int]:
    override def read(json: JValue): Int = json match {
      case JValue.JNumber(i) => Int.unbox(i)
      case _                 => throw IllegalArgumentException("Not a number")
    }
  given shortDecoder: JDecoder[Short] = new JDecoder[Short]:
    override def read(json: JValue): Short = json match {
      case JValue.JNumber(i) => Short.unbox(i)
      case _                 => throw IllegalArgumentException("Not a number")
    }

  given longDecoder: JDecoder[Long] = new JDecoder[Long]:
    override def read(json: JValue): Long = json match {
      case JValue.JNumber(i) => Long.unbox(i)
      case _                 => throw IllegalArgumentException("Not a number")
    }

  given doubleDecoder: JDecoder[Double] = new JDecoder[Double]:
    override def read(json: JValue): Double = json match {
      case JValue.JNumber(i) => Double.unbox(i)
      case _                 => throw IllegalArgumentException("Not a number")
    }

  given floatDecoder: JDecoder[Float] = new JDecoder[Float]:
    override def read(json: JValue): Float = json match {
      case JValue.JNumber(i) => Float.unbox(i)
      case _                 => throw IllegalArgumentException("Not a number")
    }

  given bigIntDecoder: JDecoder[BigInt] = new JDecoder[BigInt]:
    override def read(json: JValue): BigInt = json match {
      case JValue.JNumber(i) => BigInt(i.toString)
      case _                 => throw IllegalArgumentException("Not a number")
    }

  given bigDecimalDecoder: JDecoder[BigDecimal] = new JDecoder[BigDecimal]:
    override def read(json: JValue): BigDecimal = json match {
      case JValue.JNumber(i) => BigDecimal(i.toString)
      case _                 => throw IllegalArgumentException("Not a number")
    }
  given stringDecoder: JDecoder[String] = new JDecoder[String]:
    override def read(json: JValue): String = json match {
      case JValue.JString(s) => s
      case _                 => throw IllegalArgumentException("Not a string")
    }

  given charDecoder: JDecoder[Char] = new JDecoder[Char]:
    override def read(json: JValue): Char = json match {
      case JValue.JString(s) if s.length == 1 => s.charAt(0)
      case _ => throw IllegalArgumentException("Not a string")
    }

  given booleanDecoder: JDecoder[Boolean] = new JDecoder[Boolean]:
    override def read(json: JValue): Boolean = json match {
      case JValue.JBoolean(b) => b
      case _                  => throw IllegalArgumentException("Not a boolean")
    }

  given optionDecoder[A](using decoder: JDecoder[A]): JDecoder[Option[A]] =
    new JDecoder[Option[A]]:
      override def read(json: JValue): Option[A] = json match {
        case JValue.JNull    => None
        case JValue.JNothing => None
        case _               => Some(decoder.read(json))
      }

  given listDecoder[A](using decoder: JDecoder[A]): JDecoder[List[A]] =
    new JDecoder[List[A]]:
      override def read(json: JValue): List[A] = json match {
        case JValue.JArray(values @ _*) => values.map(decoder.read).toList
        case _ => throw IllegalArgumentException("Not an array")
      }

  given mapDecoder[A](using decoder: JDecoder[A]): JDecoder[Map[String, A]] =
    new JDecoder[Map[String, A]]:
      override def read(json: JValue): Map[String, A] = json match {
        case JValue.JObject(values) =>
          values.map((k, v) => k -> decoder.read(v))
        case _ => throw IllegalArgumentException("Not an object")
      }

  inline def derived[A](using m: Mirror.Of[A]): JDecoder[A] =
    DerivationHelper.derived[A]

  private object DerivationHelper:
    inline def derived[A](using m: Mirror.Of[A]): JDecoder[A] =
      inline m match
        case s: Mirror.SumOf[A] =>
          derivedFromSum[A](s)
        case p: Mirror.ProductOf[A] =>
          derivedFromProduct[A](p)
    end derived

    inline def summonLabels[Elems <: Tuple]: List[String] =
      inline erasedValue[Elems] match
        case _: (elem *: elems) =>
          constValue[elem].asInstanceOf[String] :: summonLabels[elems]
        case _: EmptyTuple => Nil
    end summonLabels

    inline def summonInstances[A, Elems <: Tuple]: List[JDecoder[?]] =
      inline erasedValue[Elems] match
        case _: (elem *: elems) =>
          deriveOrSummon[A, elem] :: summonInstances[A, elems]
        case _: EmptyTuple => Nil
    end summonInstances

    inline def deriveOrSummon[A, Elem]: JDecoder[Elem] =
      inline erasedValue[Elem] match
        case _: A => deriveRec[A, Elem]
        case _    => summonInline[JDecoder[Elem]]

    inline def deriveRec[A, Elem]: JDecoder[Elem] =
      inline erasedValue[A] match
        case _: Elem => error("infinite recursive derivation")
        case _: A =>
          JDecoder.derived[Elem](using
            summonInline[Mirror.Of[Elem]]
          ) // recursive derivation

    def iterable[T](p: T): Iterable[(String, Any)] =
      new AbstractIterable[(String, Any)]:
        def iterator: Iterator[(String, Any)] =
          p.asInstanceOf[Product]
            .productElementNames
            .zip(p.asInstanceOf[Product].productIterator)

    inline def derivedFromSum[A](
        m: Mirror.SumOf[A]
    ): JDecoder[A] =
      val elemInstances = summonInstances[A, m.MirroredElemTypes]
      val elemLabels = summonLabels[m.MirroredElemLabels]
      new JDecoder[A]:
        override def read(json: JValue): A =
          json match
            case JValue.JObject(values) =>
              values.get("_kind") match
                case Some(JValue.JString(kind)) =>
                  val index = elemLabels.indexOf(kind)
                  elemInstances(index).asInstanceOf[JDecoder[A]].read(json)
                case _ => throw IllegalArgumentException("Not a valid object")
            case _ => throw IllegalArgumentException("Not an object")
    end derivedFromSum

    inline def derivedFromProduct[A](
        m: Mirror.ProductOf[A]
    ): JDecoder[A] =
      val elemInstances = summonInstances[A, m.MirroredElemTypes]
      val elemLabels = summonLabels[m.MirroredElemLabels]
      new JDecoder[A]:
        override def read(json: JValue): A =
          json match
            case JValue.JObject(values) =>
              val elems =
                elemInstances.zip(elemLabels).map { case (decoder, label) =>
                  values.get(label) match
                    case Some(v) => decoder.read(v)
                    case None =>
                      throw IllegalArgumentException("Not a valid object")
                }
              m.fromProduct(Tuple.fromArray(elems.toArray))
            case _ => throw IllegalArgumentException("Not an object")
    end derivedFromProduct
  end DerivationHelper

end JDecoder
