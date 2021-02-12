package code.restService

import code.lib.AppAux._
import code.model.Entities._

import net.liftweb.json.DefaultFormats
import net.liftweb.json._

object RestClient {

  implicit val formats = DefaultFormats

  def reposByOrganization(organization: Organization): List[Repository] = {
    val resp = processResponseBody(s"https://api.github.com/orgs/$organization/repos") { responsePage =>
      val full_name_RE = s""","full_name":"$organization/([^"]+)",""".r
      val full_name_L = (for (full_name_RE(full_name) <- full_name_RE.findAllIn(responsePage)) yield full_name).toList
      full_name_L.map(Repository(_))
    }
    logger.info(s"# of repos=${resp.length}")
    resp
  }

  def contributorsByRepo(organization: Organization, repo: Repository): List[Contributor] = {
    val resp = processResponseBody(s"https://api.github.com/repos/$organization/${repo.name}/contributors") { responsePage =>
      for (contribJSON <- parse(responsePage).children) yield {
        val contrib = contribJSON.extract[Contribution]
        Contributor(repo.name, contrib.login, contrib.contributions)
      }
    }
    logger.info(s"repo='${repo.name}', # of contributors=${resp.length}")
    resp
  }

  import akka.actor.ActorSystem
  import spray.http.StatusCodes
  import spray.client.pipelining.{Get, WithTransformation, addHeader, sendReceive}
  import spray.http.{HttpRequest, HttpResponse}

  import scala.annotation.tailrec
  import scala.concurrent.{Await, Future}

  private def processResponseBody[T](url: String) (processPage: Body => List[T]): List[T] = {

    @tailrec
    def processResponsePage(processedPages: List[T], pageNumber: Int): List[T] = {
      val eitherPageBody = getResponseBody(s"$url?page=$pageNumber&per_page=100")
      eitherPageBody match {
        case Right(pageBody) if pageBody.length > 2 =>
            val processedPage = processPage(pageBody)
            processResponsePage(processedPages ++ processedPage, pageNumber + 1)
        case Right(_) =>
            processedPages
        case Left(error) =>
          logger.info(s"processResponseBody error - $error")
          processedPages
      }
    }

    processResponsePage(List.empty[T], 1)
  }

  implicit val system = ActorSystem()
  import system.dispatcher

  val pipeline: HttpRequest => Future[HttpResponse] = sendReceive

  private def getResponseBody(url: String): Either[Error, Body] = {
    val request =
      if (gh_token != null) Get(url) ~> addHeader("Authorization", gh_token)
      else Get(url)
    val response = Await.result(pipeline(request), timeout)
    response.status match {
      case StatusCodes.OK =>
        Right(response.entity.asString.trim)
      case StatusCodes.Forbidden =>
        Left("API rate limit exceeded")
      case StatusCodes.NotFound =>
        Left("Non-existent organization")
      case _ =>
        Left(s"Unexpected StatusCode ${response.status.intValue}")
    }
  }

}
