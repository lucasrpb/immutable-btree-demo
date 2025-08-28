package demo

import scala.collection.concurrent.TrieMap
import scala.concurrent.Future

class MemoryStorage extends Storage {
  protected val nodes = TrieMap.empty[String, Array[Byte]]

  override def get(id: String): Future[Array[Byte]] = {
    Future.successful(nodes(id))
  }

  override def save(list: Seq[(String, Array[Byte])]): Future[Boolean] = {
    Future.successful {
      list.foreach(node => nodes.put(node._1, node._2))
      true
    }
  }
}
