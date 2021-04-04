package network


import helpers.{Configs, Utils}
import javax.inject.{Inject, Singleton}

import org.ergoplatform.appkit.{ErgoClient, JavaHelpers, NetworkType, RestApiErgoClient}
import play.api.Logger
import sigmastate.interpreter.CryptoConstants.{dlogGroup, groupOrder}
import sigmastate.eval._
import special.sigma.GroupElement
import java.math.BigInteger

@Singleton
class Client @Inject()(utils: Utils) {
  private val logger: Logger = Logger(this.getClass)
  private val defaultHeader: Seq[(String, String)] = Seq[(String, String)](("Content-Type", "application/json"), ("api_key", Configs.nodeApiKey))
  private var client: ErgoClient = _

  private val secureRandom = new java.security.SecureRandom

  /**
   * Generate a secure random bigint
   *
   * @return BigInt
   */
  def randBigInt: BigInt = new BigInteger(256, secureRandom)

  /**
   * Sets client for the entire app when the app starts
   *
   * @return current height of blockchain
   */
  def setClient(): Long = {
    try {
      client = RestApiErgoClient.create(Configs.nodeUrl, NetworkType.MAINNET, Configs.nodeApiKey)
      client.execute(ctx => {
        ctx.getHeight
      })

    } catch {
      case e: Throwable =>
        logger.error(s"Could not set client! ${e.getMessage}.")
        0L
    }
  }

  /**
   * @return current height of the blockchain
   */
  def getHeight: Long = {
    client.execute(ctx => ctx.getHeight)
  }

  /**
   * signs input message
   *
   * @param msg message to sign String
   * @param sk  secret key
   * @return tuple sign message
   *
   * @note for convert second section sign (BigInt) to ErgoValue, use Type `special.sigma.BigInt` and function JavaHelpers.SigmaDsl.BigInt(z.bigInteger)
   */
  def sign(msg: String, sk: String): (String, String) = {
    val toSignBytes = utils.toByteArray(msg)
    val r = randBigInt
    val g: GroupElement = dlogGroup.generator
    val a: GroupElement = g.exp(r.bigInteger)
    val z = (r + BigInt(sk, 16) * BigInt(scorex.crypto.hash.Blake2b256(toSignBytes))) % groupOrder
    (utils.toHexString(a.getEncoded.toArray), z.toString(16))
  }

  /**
   * verifies signature against msg and pk
   *
   * @param msg    message
   * @param signStringA first section of signature (a: GroupElement)
   * @param signStringZ second section of signature (z: BigInt)
   * @param pkString     public key
   * @return result of verification
   */

  def verify(msg: String, signStringA: String, signStringZ: String, pkString: String): Boolean = {
    val pk = JavaHelpers.decodeStringToGE(pkString)
    val e: Array[Byte] = scorex.crypto.hash.Blake2b256(utils.toByteArray(msg)) // weak Fiat-Shamir
    val eInt = BigInt(e) // challenge as big integer
    val g: GroupElement = dlogGroup.generator
    val l = g.exp(BigInt(signStringZ, 16).bigInteger)
    val r = JavaHelpers.decodeStringToGE(signStringA).multiply(pk.exp(eInt.bigInteger))
    if (l == r) true else false
  }

  /**
   * get public key from secret key
   *
   * @param sk secret key
   * @return public key
   */
  def getPkFromSk(sk: String): String = {
    val skBig = BigInt(sk, 16)
    val pk = dlogGroup.exponentiate(dlogGroup.generator, skBig.bigInteger)
    utils.toHexString(JavaHelpers.SigmaDsl.GroupElement(pk).getEncoded.toArray)
  }
}
