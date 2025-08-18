package demo

import demo.IndexBuilder.IndexBuilt
import scala.collection.Searching._
import scala.util.{Failure, Success, Try}

class DataNode(val id: String)(val builder: IndexBuilt) extends Node {

  import builder._

  override val MIN: Int = builder.MIN
  override val MAX: Int = builder.MAX

  protected var data = Array.empty[Datom]

  def insert(list: Seq[Datom]): Try[Int] = {
    val existing = list.filter(data.search(_).isInstanceOf[Found])

    if(!existing.isEmpty) return Failure(new RuntimeException(s"Elements already exist in this node: ${existing}"))

    val size = Math.min(list.length, remaning)
    val slice = list.slice(0, size)

    data = (data ++ slice).sorted

    Success(size)
  }

  override def copy()(implicit ctx: IndexContext): DataNode = {
    if(isNew) return this

    val copy = ctx.createDataNode()

    ctx.setParent(copy.id, ctx.parents(id))

    copy.data = data
    copy
  }

  override def split()(implicit ctx: IndexContext): DataNode = {
    val right = ctx.createDataNode()

    right.data = data.slice(data.length/2, length)
    data = data.slice(0, data.length/2)

    right
  }

  override def lastKey: Datom = data.last

  def inOrder(): Seq[Datom] = data
}
