package demo

import demo.IndexBuilder.IndexBuilt
import scala.util.{Failure, Success, Try}

class MetaNode[K](val id: String)(val builder: IndexBuilt[K]) extends Node[K] {

  import builder._

  override val MIN: Int = builder.MIN
  override val MAX: Int = builder.MAX

  var links = Array.empty[(K, String)]

  def binSearch(k: K, start: Int = 0, end: Int = links.length - 1)(implicit comparator: Ordering[K]): (Boolean, Int) = {
    if(start > end) return false -> start

    val pos = start + (end - start)/2
    val c = ordering.compare(k, links(pos)._1)

    if(c == 0) return true -> pos
    if(c < 0) return binSearch(k, start, pos - 1)(comparator)

    binSearch(k, pos + 1, end)(comparator)
  }

  def findPath(k: K)(implicit comparator: Ordering[K]): String = {
    val (_, pos) = binSearch(k)(comparator)
    val idx = if(pos < links.length) pos else pos - 1
    links(idx)._2
  }

  def setPointer(node: Node[K], pos: Int)(implicit ctx: IndexContext[K]): Unit = {
    links(pos) = node.lastKey -> node.id
    ctx.setParent(node.id, Some(id -> pos))
  }

  def setPointers()(implicit ctx: IndexContext[K]): Unit = {
    for(i<-0 until links.length){
      val (k, c) = links(i)
      ctx.setParent(c, Some(id -> i))
    }
  }

  def insert(list: Seq[(K, String)])(implicit ctx: IndexContext[K]): Try[Int] = {
    //val existing = list.filter(links.contains(_))
    //if (!existing.isEmpty) return Failure(new RuntimeException(s"Elements already exist in this node: ${existing}"))

    val size = Math.min(list.length, remaning)
    val slice = list.slice(0, size)

    links = (links ++ slice).sortBy(_._1)

    setPointers()

    Success(size)
  }

  override def copy()(implicit ctx: IndexContext[K]): MetaNode[K] = {
    if(ctx.isNew(this)) return this

    val copy = ctx.createMetaNode()

    ctx.setParent(copy.id, ctx.parents(id))

    copy.links = links
    copy.setPointers()
    copy
  }

  override def split()(implicit ctx: IndexContext[K]): MetaNode[K] = {
    val right = ctx.createMetaNode()

    right.links = links.slice(links.length / 2, length)
    links = links.slice(0, links.length / 2)

    setPointers()
    right.setPointers()

    right
  }

  override def lastKey: K = links.last._1

  override def length: Int = links.length

  def inOrder(): Seq[(K, String)] = links
}
