package demo

import scala.util.Try

trait Serializer[T] {
  def serialize(o: T): Array[Byte]
  def deserialize(buffer: Array[Byte]): Try[T]
}
