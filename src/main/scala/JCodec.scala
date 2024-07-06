import Json.JValue
import scala.deriving.Mirror

trait JCodec[A] extends JDecoder[A] with JEncoder[A]

object JCodec:
  given from[A](using decoder: JDecoder[A], encoder: JEncoder[A]): JCodec[A] =
    new JCodec[A]:
      override def read(json: JValue): A = decoder.read(json)
      override def write(a: A): JValue = encoder.write(a)

  inline def derived[A](using m: Mirror.Of[A]): JCodec[A] = 
    JCodec.from[A](using JDecoder.derived[A], JEncoder.derived[A])

end JCodec
