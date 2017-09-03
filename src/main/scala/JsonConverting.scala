import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import spray.json.{DefaultJsonProtocol, JsonFormat, RootJsonFormat}


case class Tree(id: BigDecimal, left: Option[Tree], right: Option[Tree])
case class TreesSet(id: String, trees:Array[Tree])
case class TreesSetAnswer(id: String, links:Array[List[List[BigDecimal]]])
case class TreesResult(id: String, results:Array[Boolean])

trait JsonConverting extends SprayJsonSupport with DefaultJsonProtocol {

  implicit val treeFormat: JsonFormat[Tree] = lazyFormat(jsonFormat(Tree, "id", "left", "right"))
  implicit val treesSetFormat: RootJsonFormat[TreesSet] = jsonFormat2(TreesSet)
  implicit val treesSetAnswerFormat: RootJsonFormat[TreesSetAnswer] = jsonFormat2(TreesSetAnswer)
  implicit val treesResultFormat: RootJsonFormat[TreesResult] = jsonFormat2(TreesResult)

}
