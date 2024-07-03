import Json.JValue
import Json.JValue.{JNumber, JObject, JString}

trait JR[A]:
  def read(json: JValue): A
end JR

trait JW[A]:
  def write(a: A): JValue
end JW

trait JRW[A] extends JR[A] with JW[A]


case class A(a: Int, b: String)

object A:
  implicit val jw: JW[A] = new JW[A]:
    override def write(a: A): JValue = {
      val aa: JW[Int] = ???
      val ab: JW[String] = ???
      JObject(
        Map("a" -> aa.write(a.a), "b" -> ab.write(a.b))
      )
    }
  end jw

end A


@main
def main(): Unit =
  println(Parser.parse(
    """{
      |"a":123,
      |"b":true,
      |"c":false,
      |"d":null,
      |"e":[ 1 , "2" , true , false , null , {"a":456}]
      |}""".stripMargin).toString)





