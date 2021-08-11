package controllers

import javax.inject._
import network.{Explorer, NetworkIObject}
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
                              explorer: Explorer, utils: Utils, adaptor: Adaptor, networkIObject: NetworkIObject)
                             (implicit ec: ExecutionContext) extends AbstractController(controllerComponents) with Circe {

  private val logger: Logger = Logger(this.getClass)

  def exception(e: Throwable): Result = {
    logger.error(s"error in controller ${utils.getStackTraceStr(e)}")
    BadRequest(s"""{"success": false, "message": "${e.getMessage}"}""").as("application/json")
  }

  /**
   * @return current a text for root API
   */
  def index: Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    try {
      Ok(
        s"""{
           |  "success": true,
           |  "message": "Ergo Gateway Proxy"
           |}""".stripMargin
      ).as("application/json")

    } catch {
      case e: Throwable => exception(e)
    }
  }

  /**
   * @return current height of the blockchain
   */
  def height: Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    try {
      Ok(
        s"""{
           |  "success": true,
           |  "height": ${networkIObject.getHeight}
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
   * @return signs message
   */
  def sign: Action[Json] = Action(circe.json) { implicit request =>
    try {
      val msg = request.body.hcursor.downField("msg").as[String].getOrElse(throw new Throwable("msg field must exist"))
      val sk = request.body.hcursor.downField("sk").as[String].getOrElse(throw new Throwable("sk field must exist"))
      val signs = adaptor.sign(msg, sk)
      Ok(
        s"""{
           |  "success": true,
           |  "signs": {
           |    "a": "${signs._1}",
           |    "z": "${signs._2}"
           |  }
           |}""".stripMargin
      ).as("application/json")

    } catch {
      case e: Throwable => exception(e)
    }
  }

  /**
   * verifies the signature against msg and pk
   * expects msg, signs {"a", "z"}, pk field
   *
   * @return verification result
   */
  def verify: Action[Json] = Action(circe.json) { implicit request =>
    try {
      val msg = request.body.hcursor.downField("msg").as[String].getOrElse(throw new Throwable("msg field must exist"))
      val signs = request.body.hcursor.downField("signs").as[Json].getOrElse(throw new Throwable("signs fields must exist"))
      val signs_a = signs.hcursor.downField("a").as[String].getOrElse(throw new Throwable("signs field a must exist"))
      val signs_z = signs.hcursor.downField("z").as[String].getOrElse(throw new Throwable("signs field z must exist"))
      val pk = request.body.hcursor.downField("pk").as[String].getOrElse(throw new Throwable("pk field must exist"))
      Ok(
        s"""{
           |  "success": true,
           |  "verified": ${adaptor.verify(msg, signs_a, signs_z, pk)}
           |}""".stripMargin
      ).as("application/json")

    } catch {
      case e: Throwable => exception(e)
    }
  }

  /**
   * gets pk and address from sk
   * expects sk field
   *
   * @return verification result
   */
  def getAddressDetail: Action[Json] = Action(circe.json) { implicit request =>
    try {
      val sk = request.body.hcursor.downField("sk").as[String].getOrElse(throw new Throwable("sk field must exist"))
      val addr_pk = adaptor.getPkFromSk(sk)
      Ok(
        s"""{
           |  "success": true,
           |  "address": "${addr_pk._1}",
           |  "pk": "${addr_pk._2}"
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
   * {
   * "hashData" : "f9e5ce5aa0d95f5d54a7bc89c46730d9662397067250aa18a0039631c0f5b809",
   * "signs": {
   * "a": ["9662397067250aa18a0039631c0f5b809", "9662397067250aa18a0039631c0f5b809", "", "9662397067250aa18a0039631c0f5b809", "9662397067250aa18a0039631c0f5b809"],
   * "z": ["78132684712638457631278", "78132684712638457631278", "0", "78132684712638457631278", "78132684712638457631278"]
   * }
   * }
   *
   * @return tx id
   */
  def addPulse(): Action[Json] = Action(circe.json) { implicit request =>
    try {
      val hashData = request.body.hcursor.downField("hashData").as[String].getOrElse(throw new Throwable("hashData field must exist"))
      val signs = request.body.hcursor.downField("signs").as[Json].getOrElse(throw new Throwable("signs fields must exist"))
      val listSigns_a = signs.hcursor.downField("a").as[Seq[String]].getOrElse(throw new Throwable("signs fields a must exist"))
      val listSigns_z = signs.hcursor.downField("z").as[Seq[String]].getOrElse(throw new Throwable("signs fields z must exist")).map(BigInt(_, 16).bigInteger)

      if (listSigns_a.size != 5 || listSigns_z.size != 5) throw new Throwable("in signs fields a and z must be 5 object")
      Ok(
        s"""{
           |  "success": true,
           |  "txId": "${adaptor.addPulse(utils.toByteArray(hashData), (listSigns_a.map(utils.hexToGroupElement).toArray, listSigns_z.map(JavaHelpers.SigmaDsl.BigInt(_)).toArray)).replaceAll("\"", "")}"
           |}""".stripMargin
      ).as("application/json")

    } catch {
      case e: Throwable => exception(e)
    }
  }

  /**
   * Send Value To Subs
   * expects :
   * {
   * "Value" : "",
   * "DataType": "0",
   * "PulseId": "1687467486549841687486468"
   * }
   *
   * @return tx id
   */
  def sendValueToSubs(): Action[Json] = Action(circe.json) { implicit request =>
    try {
      val value = request.body.hcursor.downField("value").as[String].getOrElse(throw new Throwable("value field must exist"))
      val pulseId = request.body.hcursor.downField("pulseId").as[String].getOrElse(throw new Throwable("pulseId field must exist"))


      Ok(
        s"""{
           |  "success": true,
           |  "txId": "${adaptor.sendValueToSubs(utils.toByteArray(value), pulseId.toLong).replaceAll("\"", "")}"
           |}""".stripMargin
      ).as("application/json")

    } catch {
      case e: Throwable => exception(e)
    }
  }

  /**
   * @return extractor Data Type
   */
  def getDataType: Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    try {
      Ok(
        s"""{
           |  "success": true,
           |  "dataType": ${adaptor.getDataType}
           |}""".stripMargin
      ).as("application/json")

    } catch {
      case e: Throwable => exception(e)
    }
  }

  /**
   * @return list of current consuls
   */
  def getConsuls: Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    try {
      Ok(
        s"""{
           |  "success": true,
           |  "consuls": ${adaptor.getConsuls.asJson}
           |}""".stripMargin
      ).as("application/json")

    } catch {
      case e: Throwable => exception(e)
    }
  }

  /**
   * @return last round id
   */
  def lastRound: Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    try {
      Ok(
        s"""{
           |  "success": true,
           |  "lastRound": ${adaptor.getLastRound}
           |}""".stripMargin
      ).as("application/json")

    } catch {
      case e: Throwable => exception(e)
    }
  }

  /**
   * update consuls
   * expects :
   * {
   * "updateConsuls" : ["f9e5ce5aa0d95f5d54a7bc89c46730d9662397067250aa18a0039631c0f5b809", "f9e5ce5aa0d95f5d54a7bc89c46730d9662397067250aa18a0039631c0f5b809",
   *                    "f9e5ce5aa0d95f5d54a7bc89c46730d9662397067250aa18a0039631c0f5b809", "f9e5ce5aa0d95f5d54a7bc89c46730d9662397067250aa18a0039631c0f5b809",
   *                    "f9e5ce5aa0d95f5d54a7bc89c46730d9662397067250aa18a0039631c0f5b809"]
   * "signs": {
   * "a": ["9662397067250aa18a0039631c0f5b809", "9662397067250aa18a0039631c0f5b809", "", "9662397067250aa18a0039631c0f5b809", "9662397067250aa18a0039631c0f5b809"],
   * "z": ["78132684712638457631278", "78132684712638457631278", "0", "78132684712638457631278", "78132684712638457631278"]
   * }
   * }
   *
   * @return tx id
   */
  def updateConsuls(): Action[Json] = Action(circe.json) { implicit request =>
    try {
      val newConsuls = request.body.hcursor.downField("newConsuls").as[Seq[String]].getOrElse(throw new Throwable("newConsuls field must exist"))
      val signs = request.body.hcursor.downField("signs").as[Json].getOrElse(throw new Throwable("signs fields must exist"))
      val listSigns_a = signs.hcursor.downField("a").as[Seq[String]].getOrElse(throw new Throwable("signs fields a must exist"))
      val listSigns_z = signs.hcursor.downField("z").as[Seq[String]].getOrElse(throw new Throwable("signs fields z must exist")).map(BigInt(_, 16).bigInteger)
      val newRoundId = request.body.hcursor.downField("roundId").as[Long].getOrElse(throw new Throwable("roundId field must exist"))

      if (listSigns_a.size != 5 || listSigns_z.size != 5) throw new Throwable("in signs fields a and z must be 5 object")
      Ok(
        s"""{
           |  "success": true,
           |  "txId": "${adaptor.updateConsuls((listSigns_a.map(utils.hexToGroupElement).toArray, listSigns_z.map(JavaHelpers.SigmaDsl.BigInt(_)).toArray), newConsuls, newRoundId).replaceAll("\"", "")}"
           |}""".stripMargin
      ).as("application/json")

    } catch {
      case e: Throwable => exception(e)
    }
  }

  /**
   * update Oracles
   * expects :
   * {
   * "newOracles" : ["f9e5ce5aa0d95f5d54a7bc89c46730d9662397067250aa18a0039631c0f5b809", "f9e5ce5aa0d95f5d54a7bc89c46730d9662397067250aa18a0039631c0f5b809",
   *                 "f9e5ce5aa0d95f5d54a7bc89c46730d9662397067250aa18a0039631c0f5b809", "f9e5ce5aa0d95f5d54a7bc89c46730d9662397067250aa18a0039631c0f5b809",
   *                 "f9e5ce5aa0d95f5d54a7bc89c46730d9662397067250aa18a0039631c0f5b809"]
   * "signs": {
   * "a": ["9662397067250aa18a0039631c0f5b809", "9662397067250aa18a0039631c0f5b809", "", "9662397067250aa18a0039631c0f5b809", "9662397067250aa18a0039631c0f5b809"],
   * "z": ["78132684712638457631278", "78132684712638457631278", "0", "78132684712638457631278", "78132684712638457631278"]
   * }
   * }
   *
   * @return tx id
   */
  def updateOracles(): Action[Json] = Action(circe.json) { implicit request =>
    try {
      val newOracles = request.body.hcursor.downField("newOracles").as[Seq[String]].getOrElse(throw new Throwable("newOracles field must exist"))
      val signs = request.body.hcursor.downField("signs").as[Json].getOrElse(throw new Throwable("signs fields must exist"))
      val listSigns_a = signs.hcursor.downField("a").as[Seq[String]].getOrElse(throw new Throwable("signs fields a must exist"))
      val listSigns_z = signs.hcursor.downField("z").as[Seq[String]].getOrElse(throw new Throwable("signs fields z must exist")).map(BigInt(_, 16).bigInteger)

      if (listSigns_a.size != 5 || listSigns_z.size != 5) throw new Throwable("in signs fields a and z must be 5 object")
      Ok(
        s"""{
           |  "success": true,
           |  "txId": "${adaptor.updateOracles((listSigns_a.map(utils.hexToGroupElement).toArray, listSigns_z.map(JavaHelpers.SigmaDsl.BigInt(_)).toArray), newOracles).replaceAll("\"", "")}"
           |}""".stripMargin
      ).as("application/json")

    } catch {
      case e: Throwable => exception(e)
    }
  }

  def validateAddress(): Action[Json] = Action(circe.json) { implicit request =>
    try {
      val address = request.body.hcursor.downField("address").as[String].getOrElse(throw new Throwable("address field must exist"))

      Ok(
        s"""{
           |  "success": true,
           |  "isValid": "${adaptor.validateAddress(address)}"
           |}""".stripMargin
      ).as("application/json")

    } catch {
      case e: Throwable => exception(e)
    }
  }

}

