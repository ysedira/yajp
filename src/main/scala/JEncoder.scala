import Json.JValue

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
        JValue.JArray(a.map(encoder.write): _*)

  given mapEncoder[A](using encoder: JEncoder[A]): JEncoder[Map[String, A]] =
    new JEncoder[Map[String, A]]:
      override def write(a: Map[String, A]): JValue =
        JValue.JObject(a.map((k, v) => k -> encoder.write(v)))
