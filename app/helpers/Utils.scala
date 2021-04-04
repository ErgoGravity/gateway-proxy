package helpers

import java.io.{PrintWriter, StringWriter}

import javax.inject.{Inject, Singleton}
import scorex.util.encode.Base16

@Singleton
class Utils @Inject()() {
  def getStackTraceStr(e: Throwable): String = {
    val sw = new StringWriter
    val pw = new PrintWriter(sw)
    e.printStackTrace(pw)
    sw.toString
  }

  def toHexString(array: Array[Byte]): String = Base16.encode(array)

  def toByteArray(s: String): Array[Byte] = Base16.decode(s).get
}
