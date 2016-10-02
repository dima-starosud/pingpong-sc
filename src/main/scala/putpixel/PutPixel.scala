package putpixel

import java.io._
import java.util.concurrent.ConcurrentLinkedQueue

import scala.sys.process._

trait PixelConsumer extends (Pixel => Unit) with Closeable

object PutPixel {
  val DEFAULT_BIN_PATH = "../putpixel/bin/putpixel"

  def start(bin: String = DEFAULT_BIN_PATH, width: Int, height: Int, scale: Int = 1): PixelConsumer =
    new PutPixel(bin, width, height, scale)
}

final class PutPixel(bin: String, width: Int, height: Int, scale: Int) extends PixelConsumer {
  private val pos = new PipedOutputStream()
  private val pis = new PipedInputStream(pos)
  private val messages = new ConcurrentLinkedQueue[String]()
  private val process = (Process(Seq(bin, width, height, scale).map(_.toString)) #< pis).run(ProcessLogger(messages.add(_)))

  override def apply(pixel: Pixel): Unit = {
    val values = Seq(
      pixel.position.x,
      pixel.position.y,
      pixel.color.r,
      pixel.color.g,
      pixel.color.b
    ).mkString("", " ", "\n").getBytes()
    try {
      pos.write(values)
    } catch {
      case e: IOException if e.getMessage == "Pipe closed" =>
        val code = process.exitValue()
        throw new RuntimeException(s"Failed to put pixel. Exit code $code. Messages: $messages")
    }
    pos.flush()
  }

  override def close(): Unit = {
    pos.close()
  }
}
