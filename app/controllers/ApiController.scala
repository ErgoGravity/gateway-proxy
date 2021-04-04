package controllers

import javax.inject._
import network.{Client, Explorer}
import play.api.Logger
import play.api.mvc._
import helpers.{Configs, Utils}
import io.circe.Json
import play.api.libs.circe.Circe

import scala.concurrent.ExecutionContext

/**
 * Controller of Gravity-Ergo-Proxy.
 */
class ApiController @Inject()(controllerComponents: ControllerComponents,
                              client: Client, explorer: Explorer, utils: Utils)
                             (implicit ec: ExecutionContext) extends AbstractController(controllerComponents) with Circe {

  private val logger: Logger = Logger(this.getClass)

  def exception(e: Throwable): Result = {
    logger.error(s"error in controller ${utils.getStackTraceStr(e)}")
    BadRequest(s"""{"success": false, "message": "${e.getMessage}"}""").as("application/json")
  }

  /**
   * @return current height of the blockchain
   */
  def height: Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    try {
      Ok(
        s"""{
           |  "success": true,
           |  "height": ${client.getHeight}
           |}""".stripMargin
      ).as("application/json")

    } catch {
      case e: Throwable => exception(e)
    }
  }

  /**
   * @param txId transaction id
   * @return -1 if tx does not exist, 0 if it is unconfirmed, otherwise, confirmation num
   */
  def confNum(txId: String): Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    try {
      Ok(
        s"""{
           |  "success": true,
           |  "numConfirmations": ${explorer.getConfNum(txId)}
           |}""".stripMargin
      ).as("application/json")

    } catch {
      case e: Throwable => exception(e)
    }
  }

  /**
   * signs the message
   * expects msg, sk field
   *
   * @return signed message
   */
  def sign: Action[Json] = Action(circe.json) { implicit request =>
    try {
      val msg = request.body.hcursor.downField("msg").as[String].getOrElse(throw new Throwable("msg field must exist"))
      val sk = request.body.hcursor.downField("sk").as[String].getOrElse(throw new Throwable("sk field must exist"))
      val signed = client.sign(msg, sk)
      Ok(
        s"""{
           |  "success": true,
           |  "signed": {
           |    "a": "${signed._1}",
           |    "z": "${signed._2}"
           |  }
           |}""".stripMargin
      ).as("application/json")

    } catch {
      case e: Throwable => exception(e)
    }
  }

  /**
   * verifies the signature against msg and pk
   * expects msg, signed {"a", "z"}, pk field
   *
   * @return verification result
   */
  def verify: Action[Json] = Action(circe.json) { implicit request =>
    try {
      val msg = request.body.hcursor.downField("msg").as[String].getOrElse(throw new Throwable("msg field must exist"))
      val signed = request.body.hcursor.downField("signed").as[Json].getOrElse(throw new Throwable("signed fields must exist"))
      val signed_a = signed.hcursor.downField("a").as[String].getOrElse(throw new Throwable("signed field a must exist"))
      val signed_z = signed.hcursor.downField("z").as[String].getOrElse(throw new Throwable("signed field z must exist"))
      val pk = request.body.hcursor.downField("pk").as[String].getOrElse(throw new Throwable("pk field must exist"))
      Ok(
        s"""{
           |  "success": true,
           |  "verified": ${client.verify(msg, signed_a, signed_z, pk)}
           |}""".stripMargin
      ).as("application/json")

    } catch {
      case e: Throwable => exception(e)
    }
  }

  /**
   * gets pk from sk
   * expects sk field
   *
   * @return verification result
   */
  def getPk: Action[Json] = Action(circe.json) { implicit request =>
    try {
      val sk = request.body.hcursor.downField("sk").as[String].getOrElse(throw new Throwable("sk field must exist"))
      Ok(
        s"""{
           |  "success": true,
           |  "pk": "${client.getPkFromSk(sk)}"
           |}""".stripMargin
      ).as("application/json")

    } catch {
      case e: Throwable => exception(e)
    }
  }
}

