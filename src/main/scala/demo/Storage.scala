package demo

import scala.concurrent.Future

trait Storage {

  def save(nodes: Seq[(String, Array[Byte])]): Future[Boolean]
  def get(id: String): Future[Array[Byte]]

}
