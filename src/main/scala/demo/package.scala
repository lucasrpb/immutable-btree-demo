import com.google.common.base.Charsets
import demo.IndexBuilder.IndexBuilt
import com.google.protobuf.{ByteString, any => protobufany}

import java.nio.ByteBuffer
import java.util.concurrent.atomic.AtomicLong
import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}

package object demo {

  val TimeCounter = new AtomicLong(0L)

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

  object Serializers {

    class NodeSerializer[K: ClassTag](builder: IndexBuilt[K]) extends Serializer[Node[K]] {
      override def serialize(node: Node[K]): Array[Byte] = node match {
        case dataNode: DataNode[K] => protobufany.Any.pack(SerializableDataNode.of(dataNode.id, "", dataNode.data
          .map(k => ByteString.copyFrom(builder.keySerializer.serialize(k))))).toByteArray

        case metaNode: MetaNode[K] => protobufany.Any.pack(SerializableMetaNode.of(metaNode.id, "", metaNode.links
          .map { case (k, link) => SerializableLink.of(ByteString.copyFrom(builder.keySerializer.serialize(k)), link)
          })).toByteArray
      }

      override def deserialize(buffer: Array[Byte]): Try[Node[K]] = {
        val parsed = protobufany.Any.parseFrom(buffer)

        if(parsed.is(SerializableDataNode)){
          val dataNode = parsed.unpack(SerializableDataNode)
          val node = new DataNode[K](dataNode.id)(builder)

          node.data = dataNode.data.map { sk =>
            builder.keySerializer.deserialize(sk.toByteArray).get
          }.toIndexedSeq

          return Success(node)
        }

        if(parsed.is(SerializableMetaNode)){
          val dataNode = parsed.unpack(SerializableMetaNode)
          val node = new MetaNode(dataNode.id)(builder)

          node.links = dataNode.links.map { slink =>
            val k = slink.key
            val link = slink.link

            builder.keySerializer.deserialize(k.toByteArray).get -> link
          }.toArray

          return Success(node)
        }

        Failure(new RuntimeException("Error on deserializing node!"))
      }
    }

  }

  case class Datom(e: String, a: String, tpe: Int, value: Any, t: Long, valid: Boolean = true)

  val comparator = new Ordering[Datom] {

    println(s"comparator id: ${this.hashCode()}")

    override def compare(x: Datom, y: Datom): Int = {
      var comp = x.e.compareTo(y.e)

      if(comp != 0) return comp

      comp = x.a.compareTo(y.a)

      if(comp != 0) return comp

      comp = x.t.compareTo(y.t)

      if(comp != 0) return comp

      /*comp = x.valid.compareTo(y.valid)

      if(comp != 0) return comp

      comp = x.tpe match {
        case DatomTypes.NUMBER => x.value.asInstanceOf[Double].compareTo(y.value.asInstanceOf[Double])
        case DatomTypes.STRING => x.value.asInstanceOf[String].compareTo(y.value.asInstanceOf[String])
        case DatomTypes.BOOLEAN => x.value.asInstanceOf[Boolean].compareTo(y.value.asInstanceOf[Boolean])
        case _ => 0
      }*/

      comp
    }
  }

  val ordering: Ordering[Datom] = new Ordering[Datom] {

    println(s"ordering id: ${this.hashCode()}")

    override def compare(x: Datom, y: Datom): Int = {
      var comp = x.e.compareTo(y.e)

      if(comp != 0) return comp

      comp = x.a.compareTo(y.a)

      if(comp != 0) return comp

      comp = x.t.compareTo(y.t)

      if(comp != 0) return comp

      comp = x.valid.compareTo(y.valid)

      if(comp != 0) return comp

      comp = x.tpe match {
        case DatomTypes.NUMBER => x.value.asInstanceOf[Double].compareTo(y.value.asInstanceOf[Double])
        case DatomTypes.STRING => x.value.asInstanceOf[String].compareTo(y.value.asInstanceOf[String])
        case DatomTypes.BOOLEAN => x.value.asInstanceOf[Boolean].compareTo(y.value.asInstanceOf[Boolean])
        case _ => 0
      }

      comp
    }
  }

  implicit val intSerializer: Serializer[Int] = new Serializer[Int] {
    override def serialize(o: Int): Array[Byte] = ByteBuffer.allocate(4).putInt(o).flip().array()
    override def deserialize(buffer: Array[Byte]): Try[Int] = Success(ByteBuffer.wrap(buffer).getInt)
  }

  implicit val datomSerializer: Serializer[Datom] = new Serializer[Datom] {
    override def serialize(d: Datom): Array[Byte] = {
      com.google.protobuf.any.Any.pack(SerializableDatom.of(d.e, d.a, d.tpe match {
        case DatomTypes.STRING => DatomTypeBytesConverters.stringToBytes(d.value.asInstanceOf[String])
        case DatomTypes.NUMBER => DatomTypeBytesConverters.doubleToBytes(d.value.asInstanceOf[Double])
        case DatomTypes.BOOLEAN => DatomTypeBytesConverters.booleanToBytes(d.value.asInstanceOf[Boolean])
        case _ => throw new RuntimeException(s"no converter found for type ${d.tpe}!")
      }, d.t, d.valid, d.tpe)).toByteArray
    }

    override def deserialize(buffer: Array[Byte]): Try[Datom] = {
      val sd = com.google.protobuf.any.Any.parseFrom(buffer).unpack(SerializableDatom)

      Success(
        Datom(sd.e, sd.a, sd.tpe, sd.tpe match {
          case DatomTypes.STRING => sd.v.toStringUtf8.asInstanceOf[Any]
          case DatomTypes.NUMBER => sd.v.asReadOnlyByteBuffer().getDouble.asInstanceOf[Any]
          case DatomTypes.BOOLEAN => (if (sd.v.asReadOnlyByteBuffer().get().toInt <= 0)
            false else true).asInstanceOf[Any]
        }, sd.t, sd.valid)
      )
    }
  }

}
