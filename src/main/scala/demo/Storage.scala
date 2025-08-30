package demo

import scala.concurrent.Future

trait Storage {

  def getIndex(id: String): Future[Option[SerializableIndexContext]]
  def save(context: SerializableIndexContext, nodes: Seq[(String, Array[Byte])]): Future[Boolean]
  def get(id: String): Future[Array[Byte]]

  def close(): Unit
  def closeAsync(): Future[Unit]

}
