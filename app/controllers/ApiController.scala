package controllers

import javax.inject._
import network.{Client, Explorer}
import play.api.Logger
import play.api.mvc._
import helpers.Utils
import io.circe.Json
import io.circe.syntax._
import play.api.libs.circe.Circe
import gateway.Adaptor
import org.ergoplatform.appkit.JavaHelpers

import scala.concurrent.ExecutionContext

/**
 * Controller of Gravity-Ergo-Proxy.
 */
class ApiController @Inject()(controllerComponents: ControllerComponents,
                              client: Client, explorer: Explorer, utils: Utils, adaptor: Adaptor)
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
      val signed = adaptor.sign(msg, sk)
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
           |  "verified": ${adaptor.verify(msg, signed_a, signed_z, pk)}
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
           |  "pk": "${adaptor.getPkFromSk(sk)}"
           |}""".stripMargin
      ).as("application/json")

    } catch {
      case e: Throwable => exception(e)
    }
  }

  /**
   * @return Last Pulse Id
   */
  def getLastPulseId: Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    try {
      Ok(
        s"""{
           |  "success": true,
           |  "lastPulseId": ${adaptor.getLastPulseId}
           |}""".stripMargin
      ).as("application/json")

    } catch {
      case e: Throwable => exception(e)
    }
  }

  /**
   * @return Last oracles and bft value
   */
  def getPreAddPulseInfo: Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    try {
      val preAddPulseInfo = adaptor.getPreAddPulseInfo
      Ok(
        s"""{
           |  "success": true,
           |  "bft": ${preAddPulseInfo._2},
           |  "oracles": ${preAddPulseInfo._1.asJson}
           |}""".stripMargin
      ).as("application/json")

    } catch {
      case e: Throwable => exception(e)
    }
  }

  /**
   * Add a pulse TX
   * expects :
   *      {
   *        "hashData" : "f9e5ce5aa0d95f5d54a7bc89c46730d9662397067250aa18a0039631c0f5b809",
   *        "signs": {
   *          "a": ["9662397067250aa18a0039631c0f5b809", "9662397067250aa18a0039631c0f5b809", "", "9662397067250aa18a0039631c0f5b809", "9662397067250aa18a0039631c0f5b809"],
   *          "z": ["78132684712638457631278", "78132684712638457631278", "0", "78132684712638457631278", "78132684712638457631278"]
   *        }
   *      }
   *
   * @return tx id
   */
  def addPulse(): Action[Json] = Action(circe.json) { implicit request =>
    try {
      val hashData = request.body.hcursor.downField("hashData").as[String].getOrElse(throw new Throwable("hashData field must exist"))
      val signs = request.body.hcursor.downField("signed").as[Json].getOrElse(throw new Throwable("signs fields must exist"))
      val listSigns_a = signs.hcursor.downField("a").as[Seq[String]].getOrElse(throw new Throwable("signed fields a must exist"))
      val listSigns_z = signs.hcursor.downField("z").as[Seq[String]].getOrElse(throw new Throwable("signed fields z must exist")).map(BigInt(_).bigInteger)

      if (listSigns_a.size != 5 || listSigns_z.size != 5) throw new Throwable("in signed fields a and z must be 5 object")
      Ok(
        s"""{
           |  "success": true,
           |  "txId": "${adaptor.addPulse(utils.toByteArray(hashData), (listSigns_a.map(utils.hexToGroupElement).toArray, listSigns_z.map(JavaHelpers.SigmaDsl.BigInt(_)).toArray))}"
           |}""".stripMargin
      ).as("application/json")

    } catch {
      case e: Throwable => exception(e)
    }
  }

}

