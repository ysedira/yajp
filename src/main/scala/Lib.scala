import Json.JValue
import Json.JValue.{JNumber, JObject, JString}
import JEncoder.given
import scala.reflect.ClassTag

@main
def main(): Unit =
  println(
    Parser
      .parse("""{
      |"a":123,
      |"b":true,
      |"c":false,
      |"d":null,
      |"e":[ 1 , "2" , true , false , null , {"a":456}]
      |}""".stripMargin)
      .toString
  )
