package code.restService

import net.liftweb.http.rest.RestHelper
import net.liftweb.http._
import net.liftweb.json.JsonDSL._
import code.lib.AppAux._
import code.model.Entities._
import code.restService.RestClient.{contributorsByRepo, reposByOrganization}

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success, Try}
import java.time.{Duration, Instant}
import java.util.Date

object RestServer extends RestHelper {

  serve({
    case Req("org" :: organization :: "contributors" :: Nil, _, GetRequest) =>
      val groupLevel = S.param("group-level").openOr("organization")
      val minContribs_S = S.param("min-contribs").openOr("NA")
      val minContribs = Try(minContribs_S.toInt) match {
        case Success(i) => i
        case Failure(_) => 0
      }
      logger.info(s"groupLevel='$groupLevel', minContribs=$minContribs")
      listContributors(organization, groupLevel, minContribs)
  })

  private def listContributors(organization: String, groupLevel: String, minContribs: Int): LiftResponse = {
    val response: List[Contributor] = contributorsByOrganization(organization, groupLevel, minContribs)
    JsonResponse (response.map(_.asJson))
  }

  private def contributorsByOrganization(organization: Organization, groupLevel: String, minContribs: Int): List[Contributor] = {
    val sdf = new java.text.SimpleDateFormat("dd-MM-yyyy hh:mm:ss")
    val initialInstant = Instant.now
    logger.info(s"Starting ContribsGH-P REST API call at ${sdf.format(Date.from(initialInstant))} - organization='$organization'")

    val repos = reposByOrganization(organization)

    // parallel retrieval of contributors by repo using futures
    val contributorsDetailed: List[Contributor] = RestServerAux.contributorsDetailedFuture(organization, repos)

    // grouping, sorting
    val (contributorsGroupedAboveMin, contributorsGroupedBelowMin) = contributorsDetailed.
      map(c => if (groupLevel == "repo") c else c.copy(repo=s"All $organization repos")).
      groupBy(c => (c.repo, c.contributor)).
      mapValues(_.foldLeft(0)((acc, elt) => acc + elt.contributions)).
      map(p => Contributor(p._1._1, p._1._2, p._2)).
      partition(_.contributions >= minContribs)

    val contributorsGrouped =
      (
      contributorsGroupedAboveMin
      ++
      contributorsGroupedBelowMin.
        map(c => c.copy(contributor = "Other contributors")).
        groupBy(c => (c.repo, c.contributor)).
        mapValues(_.foldLeft(0)((acc, elt) => acc + elt.contributions)).
        map(p => Contributor(p._1._1, p._1._2, p._2))
      ).toList.sortWith { (c1: Contributor, c2: Contributor) =>
        if (c1.repo != c2.repo) c1.repo < c2.repo
        else if (c1.contributor == "Other contributors") false
        else if (c1.contributions != c2.contributions) c1.contributions >= c2.contributions
        else c1.contributor < c2.contributor
      }

    val finalInstant = Instant.now
    logger.info(s"Finished ContribsGH-P REST API call at ${sdf.format(Date.from(finalInstant))} - organization='$organization'")
    logger.info(f"Time elapsed from start to finish: ${Duration.between(initialInstant, finalInstant).toMillis/1000.0}%3.2f seconds")

    contributorsGrouped
  }

}

object RestServerAux {

  def contributorsDetailedFuture(organization: Organization, repos: List[Repository]): List[Contributor] = {
    val contributorsDetailed_L_F: List[Future[List[Contributor]]] = repos.map { repo =>
      Future { contributorsByRepo(organization, repo) }
    }
    val contributorsDetailed_F_L: Future[List[List[Contributor]]] = Future.sequence(contributorsDetailed_L_F)
    val contributorsDetailed: List[Contributor] = Await.result(contributorsDetailed_F_L, timeout).flatten
    contributorsDetailed
  }

}
