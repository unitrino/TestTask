import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import akka.stream.ActorMaterializer

import scala.concurrent.Future
import scala.io.StdIn
import scala.util.{Failure, Success}
import spray.json._
import akka.http.scaladsl.unmarshalling.Unmarshal


object WebServer extends JsonConverting {
  def main(args: Array[String]) {

    implicit val system = ActorSystem("my-system")
    implicit val materializer = ActorMaterializer()
    implicit val executionContext = system.dispatcher

    def goneThrowTree(level:BigDecimal, elemIn: Tree, answ:List[(BigDecimal, BigDecimal)]): List[(BigDecimal, BigDecimal)] = {

      val rightAnsw:List[(BigDecimal, BigDecimal)] = elemIn.right match {
        case Some(elem1) => {
          val right:List[(BigDecimal, BigDecimal)] = answ :+ (level, elem1.id)
          goneThrowTree(level+1, elem1, right)
        }
        case None => answ
      }

      val leftAnsw:List[(BigDecimal, BigDecimal)] = elemIn.left match {
        case Some(elem2) => {
          val left:List[(BigDecimal, BigDecimal)] = answ :+ (level, elem2.id)
          goneThrowTree(level+1, elem2, (rightAnsw ++ left).distinct)
        }
        case None => rightAnsw
      }
      leftAnsw
    }

    def startProcessingTree(t: TreesSet): Array[List[(BigDecimal, BigDecimal)]] = {
      t.trees.map{
        tree =>
          goneThrowTree(BigDecimal(0), tree, List.empty[(BigDecimal, BigDecimal)])
      }
    }


    def evaluateTree(t:TreesSet): TreesSetAnswer = {
      val calculatedList: Array[List[(BigDecimal, BigDecimal)]] = startProcessingTree(t)
      val answerList: Array[List[List[BigDecimal]]] = calculatedList.map(oneTreeResult =>
        oneTreeResult.groupBy(_._1).map(elem => elem._2.map(e2 => e2._2)).toList
        .flatMap(elem => elem.sliding(2))
        .filter(list => list.length > 1)
      )
      TreesSetAnswer(t.id, answerList)
    }

    val route =
      path("test") {
        get {
          val tree:Future[TreesSet] = Http().singleRequest(HttpRequest(uri ="http://ftt.elsyton.com/")).flatMap(response => Unmarshal(response.entity).to[TreesSet])
          onComplete(tree) {
              case Success(isCorrect) => {
                val answer = evaluateTree(isCorrect)
                val isAllCorrect = Http().singleRequest(HttpRequest(method = HttpMethods.POST, uri = "http://ftt.elsyton.com/", entity = HttpEntity(ContentTypes.`application/json`, answer.toJson.toString))).flatMap(response => Unmarshal(response.entity).to[TreesResult])
                onComplete(isAllCorrect) {
                  case Success(checking) => complete("Correct answers " + checking.results.count(_ == true) + " of " + checking.results.length)
                  case Failure(error) => complete("An error has occured: " + error.getMessage)
                }
              }
            case Failure(error) => complete("An error has occured: " + error.getMessage)
          }
        }
      }

    val bindingFuture = Http().bindAndHandle(route, "localhost", 8080)

    println(s"Server online at http://localhost:8080/\nPress RETURN to stop...")
    StdIn.readLine()
    bindingFuture
      .flatMap(_.unbind())
      .onComplete(_ => system.terminate())
  }
}