package demo

import scala.concurrent.Future

trait Storage {

  def save(nodes: Seq[Node]): Future[Boolean]
  def get(id: String): Future[Option[Node]]

}
