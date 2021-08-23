package helpers

import java.math.BigInteger

import org.ergoplatform.ErgoAddressEncoder
import org.ergoplatform.appkit.{Address, NetworkType, RestApiErgoClient}

object Configs extends ConfigHelper {
  lazy val nodeUrl: String = readKey("node.url")
  lazy val proxySecret: BigInteger = BigInt(readKey("proxy.secret"), 16).bigInteger
  lazy val proxyAddress: Address = Address.create(readKey("proxy.address"))
  lazy val networkType: NetworkType = if (readKey("node.networkType").toLowerCase.equals("mainnet")) NetworkType.MAINNET else NetworkType.TESTNET
  lazy val addressEncoder = new ErgoAddressEncoder(networkType.networkPrefix)
  private lazy val explorerUrlConf = readKey("explorer.url", "")
  lazy val explorerUrl: String = if (explorerUrlConf.isEmpty) RestApiErgoClient.getDefaultExplorerUrl(Configs.networkType) else explorerUrlConf
  lazy val signalBoxValue: Long =  1000000L
  lazy val defaultTxFee: Long =  1000000L
  lazy val gravityTokenId: String = readKey("tokens.gravityTokenId")
  lazy val oracleTokenId: String = readKey("tokens.oracleTokenId")
  lazy val pulseTokenId: String = readKey("tokens.pulseTokenId")
  lazy val tokenRepoTokenId: String = readKey("tokens.tokenRepoTokenId")
}
