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

    override def toString: String = this.toString()

    private def toString(ident: String = " "): String = this match
      case JValue.JNothing        => ""
      case JValue.JNull           => s"${ident}null"
      case JValue.JBoolean(value) => s"${ident}${value.toString.toLowerCase()}"
      case JValue.JNumber(value)  => s"${ident}${value.toString.toLowerCase()}"
      case JValue.JString(value)  => s"""${ident}"$value""""
      case JValue.JArray(values*) =>
        s"""${values
            .map(_.toString(ident + "  "))
            .mkString("[\n", ",\n", s"\n${ident}]")}"""
      case JValue.JObject(values) =>
        s"""${values
            .map((k, v) => s"""${ident + " "}"$k": ${v.toString}""")
            .mkString("{\n", ",", "\n}")}"""

  end JValue

  object JValue:
    val JTrue: JValue = JBoolean(true)
    val JFalse: JValue = JBoolean(false)
  end JValue

end Json
