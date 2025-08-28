package demo

import demo.IndexBuilder.IndexBuilt
import scala.collection.Searching._
import scala.util.{Failure, Success, Try}

class DataNode(val id: String)(val builder: IndexBuilt) extends Node {

  import builder._

  override val MIN: Int = builder.MIN
  override val MAX: Int = builder.MAX

  var data = IndexedSeq.empty[Datom]

  def insert(list: Seq[Datom]): Try[Int] = {
    assert(!list.isEmpty, "List should not be empty!")

    if(isFull) return Failure(new RuntimeException("Data Node is full!"))

    val existing = list.filter(data.search(_).isInstanceOf[Found])

    if(!existing.isEmpty) {
      val existingInData = existing.map(y => y -> data.find(x => ordering.compare(y, x) == 0))
      return Failure(new RuntimeException(s"Elements already exist in this node: ${existing}"))
    }

    val size = Math.min(list.length, remaning)

    val slice = list.slice(0, size)

    val t0 = System.nanoTime()
    data = (data ++ slice).sorted
    val t1 = System.nanoTime()

    val elapsed = t1 - t0

   // TimeCounter.addAndGet(elapsed)

    Success(size)
  }

  def binSearch(k: Datom, start: Int = 0, end: Int = data.length - 1)(implicit comparator: Ordering[Datom]): (Boolean, Int) = {
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

  def get(k: Datom)(implicit comparator: Ordering[Datom] = ordering): Option[Datom] = {
    data.findLast(e => e.e.compareTo(k.e) == 0 && e.a.compareTo(k.a) == 0 && e.t <= k.t)
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

  override def length: Int = data.length
}
