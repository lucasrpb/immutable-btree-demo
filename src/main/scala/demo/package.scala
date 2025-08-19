import com.google.common.base.Charsets
import com.google.protobuf.ByteString
import jdk.jshell.spi.ExecutionControl.NotImplementedException

import java.nio.ByteBuffer

package object demo {

  object DatomTypes {
    val STRING = 1
    val NUMBER = 2
    val BOOLEAN = 3
  }

  object DatomTypeBytesConverters {
    implicit def stringToBytes(str: String): ByteString = ByteString.copyFrom(str.getBytes(Charsets.UTF_8))

    implicit def intToBytes(number: Int): ByteString = {
      ByteString.copyFrom(ByteBuffer.allocate(4).putInt(number).flip())
    }

    implicit def longToBytes(number: Long): ByteString = {
      ByteString.copyFrom(ByteBuffer.allocate(8).putLong(number).flip())
    }

    implicit def doubleToBytes(number: Double): ByteString = {
      ByteString.copyFrom(ByteBuffer.allocate(8).putDouble(number).flip())
    }

    implicit def booleanToBytes(flag: Boolean): ByteString = {
      ByteString.copyFrom(ByteBuffer.allocate(1).put(if(flag) 1.toByte else 0.toByte).flip())
    }
  }

  def eavtToAny(d: EAVT): Any = d.tpe match {
    case DatomTypes.NUMBER => d.v.asReadOnlyByteBuffer().getDouble()
    case DatomTypes.STRING => d.v.toStringUtf8
    case DatomTypes.BOOLEAN => if(d.v.asReadOnlyByteBuffer().get() == 1.toByte) true else false
    case _ => throw new NotImplementedException("not implemented!")
  }

  /*implicit def caseClassToEAVT(o: Datom): EAVT = {
    import DatomTypeBytesConverters._

    o.value match {
      case v: Int => EAVT.of(o.e, o.a, v, o.timestamp, o.valid, DatomTypes.NUMBER)
      case v: Long => EAVT.of(o.e, o.a, v, o.timestamp, o.valid, DatomTypes.NUMBER)
      case v: Double => EAVT.of(o.e, o.a, v, o.timestamp, o.valid, DatomTypes.NUMBER)
      case v: String => EAVT.of(o.e, o.a, v, o.timestamp, o.valid, DatomTypes.STRING)
      case v: Boolean => EAVT.of(o.e, o.a, v, o.timestamp, o.valid, DatomTypes.BOOLEAN)
      case _ => throw new NotImplementedException("not implemented!")
    }
  }*/

  case class Datom(e: String, a: String, tpe: Int, value: Any, t: Long, valid: Boolean = true)

  val ordering: Ordering[Datom] = new Ordering[Datom] {
    override def compare(x: Datom, y: Datom): Int = {
      var comp = x.e.compareTo(y.e)

      if(comp != 0) return comp

      comp = x.a.compareTo(y.a)

      if(comp != 0) return comp

      comp = x.tpe match {
        case DatomTypes.NUMBER => x.value.asInstanceOf[Double].compareTo(y.value.asInstanceOf[Double])
        case DatomTypes.STRING => x.value.asInstanceOf[String].compareTo(y.value.asInstanceOf[String])
        case DatomTypes.BOOLEAN => x.value.asInstanceOf[Boolean].compareTo(y.value.asInstanceOf[Boolean])
        case _ => 0
      }

      if(comp != 0) return comp

      comp = x.t.compareTo(y.t)

      if(comp != 0) return comp

      x.valid.compareTo(y.valid)
    }
  }

}
