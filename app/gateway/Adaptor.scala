package gateway

import javax.inject.Inject
import play.api.Logger
import java.security.SecureRandom

import scala.collection.JavaConverters._
import network.{Client, Explorer}
import sigmastate.interpreter.CryptoConstants.{dlogGroup, groupOrder}
import sigmastate.eval._
import special.sigma.GroupElement
import org.ergoplatform.appkit.{Address, ErgoToken, ErgoType, ErgoValue, InputBox, JavaHelpers, OutBox}
import helpers.{Configs, Utils}
import org.ergoplatform.appkit.impl.ErgoTreeContract
import special.collection.Coll


class Adaptor @Inject()(client: Client, explorer: Explorer, utils: Utils){

  private val logger: Logger = Logger(this.getClass)
  private val gatewayAddresses = client.gatewayContractsInterface.get
  private def selectRandomBox(seq: Seq[InputBox]): Option[InputBox] = {
    val random = new SecureRandom()
    new scala.util.Random(random).shuffle(seq).headOption
  }

  private def getSpecBox(typeBox: String, random: Boolean = false): InputBox = {
    val boxData = typeBox match {
      case "pulse" =>
        ("pulse", gatewayAddresses.pulseAddress, GatewayContracts.pulseTokenId)
      case "oracle" =>
        ("oracle", gatewayAddresses.oracleAddress, GatewayContracts.oracleTokenId)
      case "tokenRepo" =>
        ("tokenRepo", gatewayAddresses.tokenRepoAddress, GatewayContracts.tokenRepoTokenId)
      case "gravity" =>
        ("gravity", gatewayAddresses.gravityAddress, GatewayContracts.gravityTokenId)
      case "proxy" =>
        ("proxy", Configs.proxyAddress.getErgoAddress.toString, "")
    }

    val boxes = client.getUnspentBox(Address.create(boxData._2))
    val box = if (boxData._1.equals("proxy"))
      boxes.filter(box => box.getValue > Configs.defaultTxFee * 2)
    else
      boxes.filter(box => box.getTokens.size() > 0 && box.getTokens.get(0).getId.toString.equals(boxData._3) && (if (boxData._1.equals("tokenRepo")) box.getTokens.get(0).getValue >= 2 else true))

    if (random) selectRandomBox(box).orNull else box.headOption.orNull
  }

  def getLastPulseId: Long = {
    try {
      getSpecBox("pulse").getRegisters.get(3).getValue.asInstanceOf[Long]
    }
    catch {
      case _: Throwable => 0L
    }
  }

  def getPreAddPulseInfo: (Array[String], Int) = {
    val oracleBox = getSpecBox("oracle")
    val oracles = oracleBox.getRegisters.get(1).getValue.asInstanceOf[Coll[Coll[Byte]]].map(oracle => utils.toHexString(oracle.toArray)).toArray
    val bft = oracleBox.getRegisters.get(0).getValue.asInstanceOf[Int]
    (oracles, bft)
  }

  def addPulse(hashData: Array[Byte], signs: (Array[GroupElement], Array[special.sigma.BigInt]), sendTransaction: Boolean = false): String = {
    val lastOracleBox = getSpecBox("oracle")
    val lastPulseBox = getSpecBox("pulse")
    val tokenRepoBox = getSpecBox("tokenRepo", random = true)
    val proxyBox = getSpecBox("proxy", random = true)

    def createPulseBox(lastPulseBox: InputBox, hashData: Array[Byte], signs: (Array[GroupElement], Array[special.sigma.BigInt])): OutBox = {
      client.getClient.execute(ctx => {
        val txB = ctx.newTxBuilder()
        var newPulseBox = txB.outBoxBuilder()
        newPulseBox = newPulseBox.value(lastPulseBox.getValue)
        newPulseBox = newPulseBox.tokens(lastPulseBox.getTokens.asScala.toList: _*)
        val regs = Seq(
          ErgoValue.of(hashData),
          ErgoValue.of(signs._1, ErgoType.groupElementType), ErgoValue.of(signs._2, ErgoType.bigIntType),
          ErgoValue.of(lastPulseBox.getRegisters.get(3).getValue.asInstanceOf[Long] + 1),
          ErgoValue.of(0))
        newPulseBox = newPulseBox.registers(regs: _*)
        newPulseBox.contract(new ErgoTreeContract(Address.create(gatewayAddresses.pulseAddress).getErgoAddress.script))
        newPulseBox.build()
      })
    }

    def createTokenRepoBox(lastRepoBox: InputBox): OutBox = {
      client.getClient.execute(ctx => {
        val txB = ctx.newTxBuilder()
        var newTokenRepoBox = txB.outBoxBuilder()
        newTokenRepoBox = newTokenRepoBox.value(lastRepoBox.getValue - Configs.signalBoxValue)
        newTokenRepoBox = newTokenRepoBox.tokens(new ErgoToken(lastRepoBox.getTokens.get(0).getId, lastRepoBox.getTokens.get(0).getValue - 1))
        newTokenRepoBox.contract(new ErgoTreeContract(Address.create(gatewayAddresses.tokenRepoAddress).getErgoAddress.script))
        newTokenRepoBox.build()
      })
    }

    /*
       TODO: this function can, used in add value to sub
     */
    @deprecated
    def createSignalBox(lastRepoBox: InputBox, hashData: Array[Byte]): OutBox = {
      client.getClient.execute(ctx => {
        var newSignalBox = ctx.newTxBuilder().outBoxBuilder()
        newSignalBox = newSignalBox.value(Configs.signalBoxValue)
        newSignalBox = newSignalBox.tokens(new ErgoToken(lastRepoBox.getTokens.get(0).getId, 1))
        newSignalBox = newSignalBox.registers(ErgoValue.of(hashData))
        newSignalBox.contract(new ErgoTreeContract(Address.create(gatewayAddresses.signalAddress).getErgoAddress.script))
        newSignalBox.build()
      })
    }

    def createProxyBox(proxyBox: InputBox): OutBox = {
      client.getClient.execute(ctx => {
        val txB = ctx.newTxBuilder()
        var newProxyBox = txB.outBoxBuilder()
        newProxyBox = newProxyBox.value(proxyBox.getValue - Configs.defaultTxFee)
        newProxyBox = newProxyBox.tokens(proxyBox.getTokens.asScala.toList: _*)
        newProxyBox.contract(new ErgoTreeContract(Configs.proxyAddress.getErgoAddress.script))
        newProxyBox.build()
      })
    }

    val signalCreated = lastPulseBox.getRegisters.get(5).getValue.asInstanceOf[Int]

    if (signalCreated == 1) {
      client.getClient.execute(ctx => {
        val prover = ctx.newProverBuilder()
          .withDLogSecret(Configs.proxySecret)
          .build()
        val outputs: Seq[OutBox] = Seq(createPulseBox(lastPulseBox, hashData, signs), createTokenRepoBox(tokenRepoBox), createProxyBox(proxyBox))
        val txB = ctx.newTxBuilder()
        val tx = txB.boxesToSpend(Seq(lastPulseBox, tokenRepoBox, proxyBox).asJava)
          .fee(Configs.defaultTxFee)
          .outputs(outputs: _*)
          .sendChangeTo(Configs.proxyAddress.getErgoAddress)
          .withDataInputs(Seq(lastOracleBox).toList.asJava)
          .build()
        val signed = prover.sign(tx)
        logger.debug(s"pulseTx data ${signed.toJson(false)}")
        val pulseTxId = if (sendTransaction) ctx.sendTransaction(signed) else ""
        logger.info(s"sending pulse tx ${pulseTxId}")
        pulseTxId
      })
    }
    else {
      logger.info(s"there is an active pulse box, can't create a new pulse box")
      throw new Throwable("there is an active pulse box, can't create a new pulse box")
    }
  }

  def getConsuls: Array[String] = {
    val gravityBox = getSpecBox("gravity")
    gravityBox.getRegisters.get(1).getValue.asInstanceOf[Coll[Coll[Byte]]].map(consul => utils.toHexString(consul.toArray)).toArray
  }

  def getLastRound: Long = {
    val gravityBox = getSpecBox("gravity")
    gravityBox.getRegisters.get(5).getValue.asInstanceOf[Long]
  }

  def updateConsuls(signs: (Array[GroupElement], Array[special.sigma.BigInt]), newConsuls: Seq[String], sendTransaction: Boolean= false): String = {
    val lastGravityBox = getSpecBox("gravity")
    val proxyBox = getSpecBox("proxy", random = true)

    def createNewGravityBox(lastGravityBox: InputBox, signs: (Array[GroupElement], Array[special.sigma.BigInt]), consuls: Seq[String]): OutBox = {
      client.getClient.execute(ctx => {
        val txB = ctx.newTxBuilder()
        val bftValue = ErgoValue.of(3.toLong)
        val consulsAddress = consuls.map(Address.create(_).getPublicKeyGE.getEncoded)
        val consulsValue = ErgoValue.of(IndexedSeq(consulsAddress: _*).toArray, ErgoType.collType(ErgoType.byteType))
        val signs_a = ErgoValue.of(signs._1, ErgoType.groupElementType)
        val signs_z = ErgoValue.of(signs._2, ErgoType.bigIntType)
        val newRoundId = ErgoValue.of(lastGravityBox.getRegisters.get(5).getValue.asInstanceOf[Long] + 1)

        txB.outBoxBuilder
          .value(lastGravityBox.getValue)
          .tokens(new ErgoToken(lastGravityBox.getTokens.get(0).getId, 1))
          .registers(bftValue, consulsValue, signs_a, signs_z, newRoundId)
          .contract(new ErgoTreeContract(Address.create(gatewayAddresses.gravityAddress).getErgoAddress.script))
          .build()
      })
    }

    def createProxyBox(feeBox: InputBox): OutBox = {
      client.getClient.execute(ctx => {
        val txB = ctx.newTxBuilder()
        var newProxyBox = txB.outBoxBuilder()
        newProxyBox = newProxyBox.value(feeBox.getValue - Configs.defaultTxFee)
        newProxyBox = newProxyBox.tokens(feeBox.getTokens.asScala.toList: _*)
        newProxyBox.contract(new ErgoTreeContract(Configs.proxyAddress.getErgoAddress.script))
        newProxyBox.build()
      })
    }

    client.getClient.execute(ctx => {
      val prover = ctx.newProverBuilder()
        .withDLogSecret(Configs.proxySecret)
        .build()

      val txB = ctx.newTxBuilder()
      val tx = txB.boxesToSpend(Seq(lastGravityBox, proxyBox).asJava)
      .outputs(createNewGravityBox(lastGravityBox, signs, newConsuls), createProxyBox(proxyBox))
      .fee(Configs.defaultTxFee)
      .sendChangeTo(Configs.proxyAddress.getErgoAddress)
      .build()

      val signed = prover.sign(tx)
      logger.debug(s"consulsTx data ${signed.toJson(false)}")
      val consulsTxId = if (sendTransaction) ctx.sendTransaction(signed) else ""
      logger.info(s"sending consuls tx ${consulsTxId}")
      consulsTxId
    })
  }

  def updateOracles(signs: (Array[GroupElement], Array[special.sigma.BigInt]), newOracles: Seq[String], sendTransaction: Boolean= false): String = {
    val lastOracleBox = getSpecBox("oracle")
    val lastConsulsBox = getSpecBox("gravity")
    val proxyBox = getSpecBox("proxy", random = true)

    def createNewOracleBox(lastOracleBox: InputBox, signs: (Array[GroupElement], Array[special.sigma.BigInt]), oracles: Seq[String]): OutBox = {
      client.getClient.execute(ctx => {
        val txB = ctx.newTxBuilder()
        val bftValue = ErgoValue.of(3.toLong)
        val oraclesAddress = oracles.map(Address.create(_).getPublicKeyGE.getEncoded)
        val oraclesValue = ErgoValue.of(IndexedSeq(oraclesAddress: _*).toArray, ErgoType.collType(ErgoType.byteType))
        val signs_a = ErgoValue.of(signs._1, ErgoType.groupElementType)
        val signs_z = ErgoValue.of(signs._2, ErgoType.bigIntType)

        txB.outBoxBuilder
          .value(lastOracleBox.getValue)
          .tokens(new ErgoToken(lastOracleBox.getTokens.get(0).getId, 1))
          .registers(bftValue, oraclesValue, signs_a, signs_z)
          .contract(new ErgoTreeContract(Address.create(gatewayAddresses.oracleAddress).getErgoAddress.script))
          .build()
      })
    }

    def createProxyBox(feeBox: InputBox): OutBox = {
      client.getClient.execute(ctx => {
        val txB = ctx.newTxBuilder()
        var newProxyBox = txB.outBoxBuilder()
        newProxyBox = newProxyBox.value(feeBox.getValue - Configs.defaultTxFee)
        newProxyBox = newProxyBox.tokens(feeBox.getTokens.asScala.toList: _*)
        newProxyBox.contract(new ErgoTreeContract(Configs.proxyAddress.getErgoAddress.script))
        newProxyBox.build()
      })
    }

    client.getClient.execute(ctx => {
      val prover = ctx.newProverBuilder()
        .withDLogSecret(Configs.proxySecret)
        .build()

      val txB = ctx.newTxBuilder()
      val tx = txB.boxesToSpend(Seq(lastOracleBox, proxyBox).asJava)
      .outputs(createNewOracleBox(lastOracleBox, signs, newOracles), createProxyBox(proxyBox))
      .fee(Configs.defaultTxFee)
      .withDataInputs(Seq(lastConsulsBox).asJava)
      .sendChangeTo(Configs.proxyAddress.getErgoAddress)
      .build()

      val signed = prover.sign(tx)
      logger.debug(s"oraclesTx data ${signed.toJson(false)}")
      val oraclesTxId = if (sendTransaction) ctx.sendTransaction(signed) else ""
      logger.info(s"sending oracles tx ${oraclesTxId}")
      oraclesTxId
    })
  }

  /**
   * sign input message
   *
   * @param msg message to sign String
   * @param sk  secret key
   * @return tuple sign message
   *
   * @note for convert second section sign (BigInt) to ErgoValue, use Type `special.sigma.BigInt` and function JavaHelpers.SigmaDsl.BigInt(z.bigInteger)
   */
  def sign(msg: String, sk: String): (String, String) = {
    while(true) {
      val toSignBytes = utils.toByteArray(msg)
      val r = utils.randBigInt
      val g: GroupElement = dlogGroup.generator
      val a: GroupElement = g.exp(r.bigInteger)
      val z = (r + BigInt(sk, 16) * BigInt(scorex.crypto.hash.Blake2b256(toSignBytes))) % groupOrder
      if (z.bigInteger.bitLength() < 256) {
        return (utils.toHexString(a.getEncoded.toArray), z.toString(16))
      }
    }
    ("", "")
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
   * @return Address and Public key
   */
  def getPkFromSk(sk: String): (String, String) = {
    val skBig = BigInt(sk, 16)
    val address: Address = utils.getAddressFromSk(skBig.bigInteger)
    (address.toString, utils.toHexString(address.getPublicKey.pkBytes))
  }

}
