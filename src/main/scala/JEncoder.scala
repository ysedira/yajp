import Json.JValue
import scala.deriving.Mirror
import scala.deriving.Mirror.Sum
import scala.collection.AbstractIterable
import scala.compiletime.{erasedValue, summonInline, constValue}
import scala.compiletime.error

trait JEncoder[A]:
  def write(a: A): JValue
end JEncoder

object JEncoder:
  given byteEncoder: JEncoder[Byte] = new JEncoder[Byte]:
    override def write(a: Byte): JValue = JValue.JNumber(a)
  given intEncoder: JEncoder[Int] = new JEncoder[Int]:
    override def write(a: Int): JValue = JValue.JNumber(a)
  given shortEncoder: JEncoder[Short] = new JEncoder[Short]:
    override def write(a: Short): JValue = JValue.JNumber(a)

  given longEncoder: JEncoder[Long] = new JEncoder[Long]:
    override def write(a: Long): JValue = JValue.JNumber(a)

  given doubleEncoder: JEncoder[Double] = new JEncoder[Double]:
    override def write(a: Double): JValue = JValue.JNumber(a)

  given floatEncoder: JEncoder[Float] = new JEncoder[Float]:
    override def write(a: Float): JValue = JValue.JNumber(a)

  given bigIntEncoder: JEncoder[BigInt] = new JEncoder[BigInt]:
    override def write(a: BigInt): JValue = JValue.JNumber(a)
  given bigDecimalEncoder: JEncoder[BigDecimal] = new JEncoder[BigDecimal]:
    override def write(a: BigDecimal): JValue = JValue.JNumber(a)

  given stringEncoder: JEncoder[String] = new JEncoder[String]:
    override def write(a: String): JValue = JValue.JString(a)

  given charEncoder: JEncoder[Char] = new JEncoder[Char]:
    override def write(a: Char): JValue = JValue.JString(a.toString)

  given booleanEncoder: JEncoder[Boolean] = new JEncoder[Boolean]:
    override def write(a: Boolean): JValue = JValue.JBoolean(a)

  given optionEncoder[A](using encoder: JEncoder[A]): JEncoder[Option[A]] =
    new JEncoder[Option[A]]:
      override def write(a: Option[A]): JValue = a match {
        case Some(value) => encoder.write(value)
        case None        => JValue.JNull
      }

  given listEncoder[A](using encoder: JEncoder[A]): JEncoder[List[A]] =
    new JEncoder[List[A]]:
      override def write(a: List[A]): JValue =
        JValue.JArray(a.map(encoder.write)*)

  given mapEncoder[A](using encoder: JEncoder[A]): JEncoder[Map[String, A]] =
    new JEncoder[Map[String, A]]:
      override def write(a: Map[String, A]): JValue =
        JValue.JObject(a.map((k, v) => k -> encoder.write(v)))

  inline def derived[A](using m: Mirror.Of[A]): JEncoder[A] =
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

  inline def summonInstances[A, Elems <: Tuple]: List[JEncoder[?]] =
    inline erasedValue[Elems] match
      case _: (elem *: elems) =>
        deriveOrSummon[A, elem] :: summonInstances[A, elems]
      case _: EmptyTuple => Nil
  end summonInstances

  inline def deriveOrSummon[A, Elem]: JEncoder[Elem] =
    inline erasedValue[Elem] match
      case _: A => deriveRec[A, Elem]
      case _    => summonInline[JEncoder[Elem]]

  inline def deriveRec[A, Elem]: JEncoder[Elem] =
    inline erasedValue[A] match
      case _: Elem => error("infinite recursive derivation")
      case _: A =>
        JEncoder.derived[Elem](using
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
  ): JEncoder[A] =
    val elemInstances = summonInstances[A, m.MirroredElemTypes]
    val elemLabels = summonLabels[m.MirroredElemLabels]
    new JEncoder[A]:
      override def write(a: A): JValue =
        val index = m.ordinal(a)
        elemInstances(index).asInstanceOf[JEncoder[Any]].write(a) match
          case JValue.JNothing        => JValue.JNothing
          case JValue.JNull           => JValue.JNull
          case JValue.JBoolean(value) => JValue.JBoolean(value)
          case JValue.JNumber(value)  => JValue.JNumber(value)
          case JValue.JString(value)  => JValue.JString(value)
          case JValue.JArray(values*) => JValue.JArray(values*)
          case JValue.JObject(values) =>
            JValue.JObject(
              values.updated("_kind", JValue.JString(elemLabels(index)))
            )

  end derivedFromSum

  inline def derivedFromProduct[A](
      m: Mirror.ProductOf[A]
  ): JEncoder[A] =
    val elemInstances = summonInstances[A, m.MirroredElemTypes]
    new JEncoder[A]:
      override def write(a: A): JValue = {
        JValue.JObject {
          iterable(a)
            .lazyZip(elemInstances)
            .map { case ((label, value), encoder) =>
              (label, encoder.asInstanceOf[JEncoder[Any]].write(value))
            }
            .toMap
        }
      }
  end derivedFromProduct
end JEncoder
