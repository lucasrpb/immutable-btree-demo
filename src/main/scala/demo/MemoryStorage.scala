package demo

import scala.collection.concurrent.TrieMap
import scala.concurrent.Future

class MemoryStorage extends Storage {
  protected val nodes = TrieMap.empty[String, Node]

  override def get(id: String): Future[Option[Node]] = {
    Future.successful(nodes.get(id))
  }

  override def save(list: Seq[Node]): Future[Boolean] = {
    Future.successful {
      nodes.foreach(node => nodes.put(node._1, node._2))
      true
    }
  }
}
