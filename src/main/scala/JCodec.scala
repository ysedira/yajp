import Json.JValue

trait JCodec[A] extends JDecoder[A] with JEncoder[A]

object JCodec:
  given from[A](using decoder: JDecoder[A], encoder: JEncoder[A]): JCodec[A] =
    new JCodec[A]:
      override def read(json: JValue): A = decoder.read(json)
      override def write(a: A): JValue = encoder.write(a)
