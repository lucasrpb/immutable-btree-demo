package demo

import demo.IndexBuilder.IndexBuilt

import java.util.UUID
import scala.collection.concurrent.TrieMap
import scala.concurrent.Future
import scala.util.{Failure, Success}

class IndexContext(val builder: IndexBuilt) {
  import builder._

  var root: Option[String] = None
  var numElements: Long = 0L
  val parents = TrieMap.empty[String, Option[(String, Int)]]
  var levels = 0

  val newBlocksReferences = TrieMap.empty[String, Node]

  def getNode(id: String): Future[Node] = {
    val opt = newBlocksReferences.get(id)

    if(opt.isDefined) return Future.successful(opt.get)

    storage.get(id).map(builder.serializer.deserialize(_)).flatMap {
      case Success(node) => Future.successful(node)
      case Failure(ex) => Future.failed(ex)
    }
  }

  def getDataNode(id: String): Future[DataNode] = getNode(id).map(_.asInstanceOf[DataNode])
  def getMetaNode(id: String): Future[MetaNode] = getNode(id).map(_.asInstanceOf[MetaNode])

  def createDataNode(): DataNode = {
    val node = new DataNode(id = UUID.randomUUID().toString)(builder)
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

  def createMetaNode(): MetaNode = {
    val node = new MetaNode(id = UUID.randomUUID().toString)(builder)
    parents.put(node.id, None)
    newBlocksReferences.put(node.id, node)
    node
  }

  def setParent(id: String, parent: Option[(String, Int)] = None): Unit = {
    parents.put(id, parent)
  }

}
