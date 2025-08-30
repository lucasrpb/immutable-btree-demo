package demo

import demo.IndexBuilder.IndexBuilt

import java.util.UUID
import scala.collection.concurrent.TrieMap
import scala.concurrent.Future
import scala.reflect.ClassTag
import scala.util.{Failure, Success}

class IndexContext[K: ClassTag](val builder: IndexBuilt[K]) {
  import builder._

  var root: Option[String] = None
  var numElements: Long = 0L
  val parents = TrieMap.empty[String, Option[(String, Int)]]
  var levels = 0

  val newBlocksReferences = TrieMap.empty[String, Node[K]]

  def getNode(id: String): Future[Node[K]] = {
    val opt = newBlocksReferences.get(id)

    if(opt.isDefined) return Future.successful(opt.get)

    storage.get(id).map(builder.serializer.deserialize(_)).flatMap {
      case Success(node) => Future.successful(node)
      case Failure(ex) => Future.failed(ex)
    }
  }

  def getDataNode(id: String): Future[DataNode[K]] = getNode(id).map(_.asInstanceOf[DataNode[K]])
  def getMetaNode(id: String): Future[MetaNode[K]] = getNode(id).map(_.asInstanceOf[MetaNode[K]])

  def createDataNode(): DataNode[K] = {
    val node = new DataNode[K](id = UUID.randomUUID().toString)(builder)
    parents.put(node.id, None)
    newBlocksReferences.put(node.id, node)
    node
  }

  def incrementLevels(): Int = {
    levels += 1
    levels
  }

  def decrementLevels(): Int = {
    levels -= 1
    levels
  }

  def createMetaNode(): MetaNode[K] = {
    val node = new MetaNode[K](id = UUID.randomUUID().toString)(builder)
    parents.put(node.id, None)
    newBlocksReferences.put(node.id, node)
    node
  }

  def setParent(id: String, parent: Option[(String, Int)] = None): Unit = {
    parents.put(id, parent)
  }

}
