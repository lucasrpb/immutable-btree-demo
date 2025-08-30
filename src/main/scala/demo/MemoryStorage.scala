package demo

import scala.collection.concurrent.TrieMap
import scala.concurrent.Future

class MemoryStorage extends Storage {
  protected val indexes = TrieMap.empty[String, SerializableIndexContext]
  protected val nodes = TrieMap.empty[String, Array[Byte]]

  override def get(id: String): Future[Array[Byte]] = {
    Future.successful(nodes(id))
  }

  override def save(context: SerializableIndexContext, list: Seq[(String, Array[Byte])]): Future[Boolean] = {
    Future.successful {
      indexes.put(context.id, context)
      list.foreach(node => nodes.addOne(node))
      true
    }
  }

  override def getIndex(id: String): Future[Option[SerializableIndexContext]] = Future.successful {
    indexes.get(id)
  }

  override def close(): Unit = {}

  override def closeAsync(): Future[Unit] = Future.successful({})
}
