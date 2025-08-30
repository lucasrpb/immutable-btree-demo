package demo

import demo.IndexBuilder.IndexBuilt

import java.util.UUID
import java.util.concurrent.atomic.AtomicLong
import scala.collection.concurrent.TrieMap
import scala.concurrent.Future
import scala.reflect.ClassTag
import scala.util.Try
import scala.util.{Failure, Success}

class Index[K: ClassTag](val context: SerializableIndexContext)(val builder: IndexBuilt[K]) {

  import builder._
  val $this = this

  implicit val ctx: IndexContext[K] = new IndexContext[K](context, builder)

  def findLeaf(node: Node[K], k: K)(implicit comparator: Ordering[K]): Future[Option[DataNode[K]]] = {
    node match {
      case leaf: DataNode[K] =>
        Future.successful(Some(leaf))
      case meta: MetaNode[K] =>

        meta.setPointers()
        val nextNodeId = meta.findPath(k)(comparator)

        ctx.getNode(nextNodeId).flatMap(findLeaf(_, k)(comparator))
    }
  }

  def findLeaf(k: K)(implicit comparator: Ordering[K]): Future[Option[DataNode[K]]] = {
    ctx.root match {
      case None => Future.successful(None)
      case Some(id) => ctx.getNode(id).flatMap(findLeaf(_, k)(comparator))
    }
  }

  protected def fixRoot(p: Node[K]): Future[Boolean] = {
    p match {
      case p: MetaNode[K] =>

        if(p.length == 1){
          val c = p.links(0)._2

          ctx.decrementLevels()

          ctx.getNode(c).map { block =>
            //block.level = ctx.levels

            val copy = block.copy()

            ctx.root = Some(copy.id)
            ctx.setParent(copy.id, None)

            true
          }
        } else {
          ctx.root = Some(p.id)
          ctx.setParent(p.id, None)

          Future.successful(true)
        }

      case p: DataNode[K] =>
        ctx.root = Some(p.id)
        ctx.setParent(p.id, None)

        Future.successful(true)
    }
  }

  protected def recursiveCopy(node: Node[K]): Future[Boolean] = {
    ctx.parents(node.id) match {
      case None => fixRoot(node)
      case Some((pid, pos)) => ctx.getMetaNode(pid).flatMap { p =>
        val parent = p.copy()

        parent.links(pos) = node.lastKey -> node.id
        ctx.setParent(node.id, Some(parent.id -> pos))

        parent.setPointers()

        recursiveCopy(parent)
      }
    }
  }

  def insertEmpty(list: Seq[K]): Try[Int] = {
    val leaf = ctx.createDataNode()

    ctx.root = Some(leaf.id)
    ctx.incrementLevels()

    leaf.insert(list)
  }

  protected def insertParent(left: MetaNode[K], prev: Node[K]): Future[Int] = {
    if(left.isFull){
      val right = left.split()

      if(ordering.gt(prev.lastKey, left.lastKey)){
        return right.insert(Seq(prev.lastKey -> prev.id)) match {
          case Success(n) => recursiveCopy(right).flatMap(_ => handleParent(left, right))
          case Failure(exception) => Future.failed(exception)
        }
      }

      return left.insert(Seq(prev.lastKey -> prev.id)) match {
        case Success(n) => recursiveCopy(left).flatMap(_ => handleParent(left, right))
        case Failure(exception) => Future.failed(exception)
      }
    }

    left.insert(Seq(prev.lastKey -> prev.id)) match {
      case Success(n) => recursiveCopy(left).map(_ => n)
      case Failure(exception) => Future.failed(exception)
    }
  }

  protected def handleParent(left: Node[K], right: Node[K]): Future[Int] = {
    val parent = ctx.parents(left.id)

    parent match {
      case None =>
        val meta = ctx.createMetaNode()

        meta.insert(Seq(
          left.lastKey -> left.id,
          right.lastKey -> right.id
        ))

        ctx.incrementLevels()

        ctx.root = Some(meta.id)

        Future.successful(0)

      case Some((pid, pos)) => ctx.getMetaNode(pid).flatMap { node =>
        val parent = node.copy()

        parent.links(pos) = left.lastKey -> left.id
        ctx.setParent(left.id, Some(parent.id -> pos))

        insertParent(parent, right)
      }
    }
  }

  protected def splitDataNode(left: DataNode[K], data: Seq[K]): Future[Int] = {
    val right = left.split()

    val k = data(0)
    val leftLast = left.lastKey
    val rightLast = right.lastKey

    var list = data

    // Avoids searching for the path again! :)
    if(builder.ordering.gt(k, leftLast)){

      if(!builder.ordering.gt(k, rightLast)){
        list = list.takeWhile{k => builder.ordering.lt(k, rightLast)}
      }

      return right.insert(list) match {
        case Success(rn) => handleParent(left, right).map(_ => rn)
        case Failure(ex) => Future.failed(ex)
      }
    }

    left.insert(list.takeWhile{k => builder.ordering.lt(k, leftLast)}) match {
      case Success(ln) => handleParent(left, right).map{_ => ln}
      case Failure(ex) => Future.failed(ex)
    }
  }

  def insertDataNode(left: DataNode[K], list: Seq[K]): Future[Int] = {
    if(left.isFull){
      //val right = left.split()
      //return handleParent(left, right).map(_ => 0)
      return splitDataNode(left, list)
    }

    left.insert(list) match {
      case Success(n) => recursiveCopy(left).map(_ => n)
      case Failure(exception) => Future.failed(exception)
    }
  }

  protected def insertOrdered(list: Seq[K]): Future[Long] = {

    val sorted = list.sorted

    val length: Int = list.length
    var pos: Int = 0

    def insert(): Future[Long] = {
      if(pos == length) return Future.successful(length)

      var list = sorted.slice(pos, length)
      val k = list(0)

      TimeProfiler.snap()

      findLeaf(k).flatMap {
        case None =>

          TimeProfiler.snap()


          insertEmpty(list) match {
          case Failure(ex) => Future.failed(ex)
          case Success(n) => Future.successful(n)
        }
        case Some(leaf) =>

          TimeProfiler.snap()

         val idx = list.indexWhere{d => ordering.gt(d, leaf.lastKey)}
          if(idx > 0) list = list.slice(0, idx)

          insertDataNode(leaf.copy(), list)
      }.flatMap { n =>
        pos += n
        ctx.numElements += 1
        insert()
      }
    }

    insert()
  }

  def insert(list: Seq[K]): Future[Long] = {
    val it = list.grouped(MAX)
    val n = new AtomicLong(0L)

    def insert(): Future[Long] = {
      if(it.hasNext){
        return insertOrdered(it.next()).flatMap { inserted =>
          n.addAndGet(inserted)
          insert()
        }.recoverWith {
          case ex: Throwable => Future.failed(ex)
        }
      }

      Future.successful(n.get())
    }
    
    insert()
  }

  protected def inOrder(root: Node[K]): Future[Seq[K]] = {
    root match {
      case leaf: DataNode[K] => Future.successful(leaf.inOrder())
      case meta: MetaNode[K] => Future.sequence(meta.links.map { case (k, link) =>
        ctx.getNode(link).flatMap(inOrder(_))
      }.toSeq).map(_.flatten)
    }
  }

  def inOrder(): Future[Seq[K]] = {
    ctx.root match {
      case None => Future.successful(Seq.empty[K])
      case Some(root) => ctx.getNode(root).flatMap(inOrder(_))
    }
  }

  def getLeftMost(start: Option[Node[K]]): Future[Option[DataNode[K]]] = {
    start match {
      case None => Future.successful(None)
      case Some(b) => b match {
        case b: DataNode[K] => Future.successful(Some(b))
        case b: MetaNode[K] =>

          b.setPointers()

          ctx.getNode(b.links(0)._2).flatMap(b => getLeftMost(Some(b)))
      }
    }
  }

  def getRightMost(start: Option[Node[K]]): Future[Option[DataNode[K]]] = {
    start match {
      case None => Future.successful(None)
      case Some(b) => b match {
        case b: DataNode[K] => Future.successful(Some(b))
        case b: MetaNode[K] =>

          b.setPointers()

          ctx.getNode(b.links(b.links.length - 1)._2).flatMap(b => getRightMost(Some(b)))
      }
    }
  }

  def first(): Future[Option[DataNode[K]]] = {
    if(ctx.root.isEmpty) return Future.successful(None)

    val root = ctx.root.get

    ctx.getNode(root).flatMap{ b =>
      ctx.setParent(b.id, None)
      getLeftMost(Some(b))
    }
  }

  def last(): Future[Option[DataNode[K]]] = {
    if(ctx.root.isEmpty) return Future.successful(None)

    val root = ctx.root.get

    ctx.getNode(root).flatMap{ b =>
      ctx.setParent(b.id, None)
      getRightMost(Some(b))
    }
  }

  def next(current: Option[String]): Future[Option[DataNode[K]]] = {

    def nxt(b: Node[K]): Future[Option[DataNode[K]]] = {

      val opt = ctx.parents(b.id)

      opt match {
        case None => Future.successful(None)
        case Some((pid, pos)) => ctx.getMetaNode(pid).flatMap { parent =>

          val pointers = parent.links
          val len = pointers.length

          parent.setPointers()

          if (pos == len - 1) {
            nxt(parent)
          } else {
            ctx.getNode(pointers(pos + 1)._2).flatMap(b => getLeftMost(Some(b)))
          }
        }
      }
    }

    current match {
      case None => first()
      case Some(current) => ctx.getNode(current).flatMap {nxt(_)}
    }
  }

  def prev(current: Option[String]): Future[Option[DataNode[K]]] = {

    def prv(b: Node[K]): Future[Option[DataNode[K]]] = {

      val opt = ctx.parents(b.id)

      opt match {
        case None => Future.successful(None)
        case Some((pid, pos)) => ctx.getMetaNode(pid).flatMap { parent =>
          parent.setPointers()

          if (pos == 0) {
            prev(Some(parent.id))
          } else {
            ctx.getNode(parent.links(pos - 1)._2).flatMap(b => getRightMost(Some(b)))
          }
        }
      }
    }

    current match {
      case None => first()
      case Some(current) => ctx.getNode(current).flatMap {prv(_)}
    }
  }

  def inOrder(f: K => Boolean = _ => true): AsyncIndexIterator[Seq[K]] = new RichAsyncIndexIterator[K](f) {
    override def hasNext(): Future[Boolean] = {
      if (!firstTime) return Future.successful(ctx.root.isDefined)
      Future.successful(cur.isDefined)
    }

    override def next(): Future[Seq[K]] = {
      if (!firstTime) {
        firstTime = true

        return first().map {
          case None =>
            cur = None
            Seq.empty[K]

          case Some(b) =>
            cur = Some(b)
            b.inOrder().filter(f)
        }
      }

      $this.next(cur.map(_.id)).map {
        case None =>
          cur = None
          Seq.empty[K]

        case Some(b) =>
          cur = Some(b)
          b.inOrder().filter(f)
      }
    }
  }

  def all(it: AsyncIndexIterator[Seq[K]] = inOrder()): Future[Seq[K]] = {
    it.hasNext().flatMap {
      case true => it.next().flatMap { list =>
        all(it).map {
          list ++ _
        }
      }
      case false => Future.successful(Seq.empty[K])
    }
  }
}
