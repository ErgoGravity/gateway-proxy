package network


import gateway.GatewayContracts
import helpers.{Configs, Utils}
import javax.inject.{Inject, Singleton}
import org.ergoplatform.appkit.{Address, BlockchainContext, ErgoClient, InputBox, RestApiErgoClient}

import scala.collection.JavaConverters._
import play.api.Logger

@Singleton
class Client @Inject()(utils: Utils) {
  private val logger: Logger = Logger(this.getClass)
  private val defaultHeader: Seq[(String, String)] = Seq[(String, String)](("Content-Type", "application/json"))
  private var client: ErgoClient = _

  var gatewayContractsInterface: Option[GatewayContracts] = None

  /**
   * Sets client for the entire app when the app starts
   *
   * @return current height of blockchain
   */
  def setClient(): Long = {
    try {
      client = RestApiErgoClient.create(Configs.nodeUrl, Configs.networkType, Configs.nodeApiKey)
      client.execute(ctx => {
        gatewayContractsInterface = Some(new GatewayContracts(ctx))
        ctx.getHeight
      })

    } catch {
      case e: Throwable =>
        logger.error(s"Could not set client! ${e.getMessage}.")
        0L
    }
  }

  def getClient: ErgoClient = {
    client
  }

  /**
   * @return current height of the blockchain
   */
  def getHeight: Long = {
    client.execute(ctx => ctx.getHeight)
  }

  /**
   * @param address :Address get a valid address
   * @return List of input address boxes
   */
  def getUnspentBox(address: Address): List[InputBox] = {
    client.execute(ctx =>
      ctx.getUnspentBoxesFor(address).asScala.toList
    )
  }

}
