package demo

import com.google.common.base.Charsets
import com.google.protobuf.ByteString

import java.nio.ByteBuffer
import java.nio.file.{Files, Paths}
import java.util.UUID
import scala.jdk.CollectionConverters.IterableHasAsScala

object Main {

  object DatomTypes {
    val STRING = 1
    val NUMBER = 2
  }

  object DatomTypeBytesConverters {
    implicit def stringToBytes(str: String): ByteString = ByteString.copyFrom(str.getBytes(Charsets.UTF_8))

    implicit def intToBytes(number: Int): ByteString = {
      ByteString.copyFrom(ByteBuffer.allocate(4).putInt(number).flip())
    }

    implicit def doubleToBytes(number: Double): ByteString = {
      ByteString.copyFrom(ByteBuffer.allocate(8).putDouble(number).flip())
    }
  }

  def main(args: Array[String]): Unit = {

    import DatomTypeBytesConverters._

    val movieData = Files.readAllLines(Paths.get("movies_with_id.csv")).asScala.toSeq
    val data = movieData.slice(1, movieData.length).map(_.split(",").toSeq).filter(_.length == 5)

    val eavtData = data.map { info =>
      val t = System.nanoTime()
      val id = UUID.randomUUID().toString
      val name = EAVT.of(e = id, a = "name", v = info(1), t = t, valid = true, DatomTypes.STRING)

      val year = EAVT.of(e = id, a = "year", v = info(2).toInt, t = t, valid = true, DatomTypes.NUMBER)

      val description = EAVT.of(e = id, a = "description", v = info(3), t = t, valid = true, DatomTypes.STRING)

      val rating = EAVT.of(e = id, a = "rating", v = info(4).toDouble, t = t, valid = true, DatomTypes.NUMBER)

      (name, year, description, rating)
    }

    println()
  }

}
