package network

import helpers.Configs
import io.circe.Json
import io.circe.parser.parse
import javax.inject.Singleton
import scalaj.http.Http


@Singleton
class Explorer() {
  private val baseUrl = s"${Configs.explorerUrl}/api/v0"
  private val tx = s"$baseUrl/transactions"
  private val unconfirmedTx = s"$baseUrl/transactions/unconfirmed"

  private val defaultHeader: Seq[(String, String)] = Seq[(String, String)](("Content-Type", "application/json"))

  /**
   * @param txId transaction id
   * @return transaction if it is unconfirmed
   */
  def getUnconfirmedTx(txId: String): Json = {
    val unconfirmed = Http(s"$unconfirmedTx/$txId").headers(defaultHeader).asString
    if (unconfirmed.isError) Json.Null
    else parse(unconfirmed.body).getOrElse(Json.Null)
  }

  /**
   * @param txId transaction id
   * @return transaction if it is confirmed (mined)
   */
  def getConfirmedTx(txId: String): Json = {
    val confirmed = Http(s"$tx/$txId").headers(defaultHeader).asString
    if (confirmed.isError) Json.Null
    else parse(confirmed.body).getOrElse(Json.Null)
  }

  /**
   * @param txId transaction id
   * @return -1 if tx does not exist, 0 if it is unconfirmed, otherwise, confirmation num
   */
  def getConfNum(txId: String): Int = {
    val unc = getUnconfirmedTx(txId)
    if (unc != Json.Null) 0
    else {
      val conf = getConfirmedTx(txId)
      if (conf != Json.Null) conf.hcursor.downField("summary").as[Json].getOrElse(Json.Null)
        .hcursor.downField("confirmationsCount").as[Int].getOrElse(-1)
      else -1
    }
  }
}