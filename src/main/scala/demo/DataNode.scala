package demo

import demo.IndexBuilder.IndexBuilt

import scala.collection.Searching._
import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}

class DataNode[K: ClassTag](val id: String)(val builder: IndexBuilt[K]) extends Node[K] {

  import builder._

  override val MIN: Int = builder.MIN
  override val MAX: Int = builder.MAX

  var data = IndexedSeq.empty[K]

  def insert(list: Seq[K]): Try[Int] = {
    assert(!list.isEmpty, "List should not be empty!")

    if(isFull) return Failure(new RuntimeException("Data Node is full!"))

    val existing = list.filter(data.search(_).isInstanceOf[Found])

    if(!existing.isEmpty) {
      val existingInData = existing.map(y => y -> data.find(x => ordering.compare(y, x) == 0))
      return Failure(new RuntimeException(s"Elements already exist in this node: ${existing}"))
    }

    val size = Math.min(list.length, remaning)

    val slice = list.slice(0, size)
    data = (data ++ slice).sorted

    Success(size)
  }

  def binSearch(k: K, start: Int = 0, end: Int = data.length - 1)(implicit comparator: Ordering[K]): (Boolean, Int) = {
    if(start > end) return false -> start

    val pos = start + (end - start)/2
    val c = comparator.compare(k, data(pos))

    if(c == 0) return true -> pos
    if(c < 0) return binSearch(k, start, pos - 1)(comparator)

    binSearch(k, pos + 1, end)(comparator)
  }

  /*def get(k: Datom)(implicit comparator: Ordering[Datom] = ordering): Option[Datom] = {
    val (found, pos) = binSearch(k)(comparator)
    if(found) Some(data(pos)) else None
  }*/

  override def copy()(implicit ctx: IndexContext[K]): DataNode[K] = {
    if(ctx.isNew(this)) return this

    val copy = ctx.createDataNode()

    ctx.setParent(copy.id, ctx.parents(id))

    copy.data = data
    copy
  }

  override def split()(implicit ctx: IndexContext[K]): DataNode[K] = {
    val right = ctx.createDataNode()

    right.data = data.slice(data.length/2, length)
    data = data.slice(0, data.length/2)

    right
  }

  override def lastKey: K = data.last

  def inOrder(): Seq[K] = data

  override def length: Int = data.length
}
