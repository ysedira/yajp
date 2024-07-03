import Json.JValue
import Json.JValue.{JNumber, JObject, JString}

// trait JEncoder[A]:
//   def read(json: JValue): A
// end JEncoder

// trait JW[A]:
//   def write(a: A): JValue
// end JW

// trait JRW[A] extends JEncoder[A] with JW[A]

// case class A(a: Int, b: String)

// object A:
//   given jw: JW[A] = new JW[A]:
//     override def write(a: A): JValue = {
//       val aa: JW[Int] = (i:Int)=>JNumber(i)
//       val ab: JW[String] = (s:String)=>JString(s)
//       JObject(
//         Map(
//           "a" -> aa.write(a.a),
//           "b" -> ab.write(a.b)
//         )
//       )
//     }

//   given jr: JEncoder[A] = new JEncoder[A]:
//     override def read(json: JValue): A = {
//       val ja: JEncoder[Int] = (j:JValue)=>j match {
//         case JNumber(i) => Int.unbox(i)
//         case _ => throw IllegalArgumentException("Not a number")
//       }
//       val jb: JEncoder[String] = (j:JValue)=>j match {
//         case JString(s) => s
//         case _ => throw IllegalArgumentException("Not a string")
//       }
//       json match {
//         case JObject(m) => A(ja.read(m("a")), jb.read(m("b")))
//         case _ => throw IllegalArgumentException("Not an object")
//       }
//     }

// end A

// def writeJson[T](t: T)(using jw: JW[T]): JValue =
//   jw.write(t)

// def readJson[T](json: JValue)(using jr: JEncoder[T]): T =
//   jr.read(json)

@main
def main(): Unit =
  // val a = A(1, "2")
  // val json = writeJson(a)
  // println(json)
  // val a2 = readJson[A](json)
  // println(a2)
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
