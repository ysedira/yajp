import scala.collection.immutable.TreeMap

object Json:
  enum JValue:
    case JNothing
    case JNull
    case JBoolean(value: Boolean)
    case JNumber(value: Number)
    case JString(value: String)
    case JArray(values: JValue*)
    case JObject(values: Map[String, JValue])

    override def toString: String = this match
      case JValue.JNothing        => ""
      case JValue.JNull           => s"null"
      case JValue.JBoolean(value) => s"${value.toString.toLowerCase()}"
      case JValue.JNumber(value)  => s"${value.toString.toLowerCase()}"
      case JValue.JString(value)  => s""""$value""""
      case JValue.JArray(values*) =>
        s"""${values
            .map(_.toString())
            .mkString("[ ", ", ", s" ]")}"""
      case JValue.JObject(values) =>
        s"""${values
            .map((k, v) => s""""$k": ${v.toString}""")
            .mkString("{ ", ", ", " }")}"""

  end JValue

  object JValue:
    val JTrue: JValue = JBoolean(true)
    val JFalse: JValue = JBoolean(false)
  end JValue

end Json
