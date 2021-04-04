package helpers

object Configs extends ConfigHelper {
  lazy val nodeUrl: String = readKey("node.url")
  lazy val nodeApiKey: String = readKey("node.apiKey")

  lazy val explorerUrl: String = readKey("explorer.url")
}
