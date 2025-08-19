package demo

import demo.IndexBuilder.IndexBuilt

import java.util.concurrent.atomic.AtomicLong
import scala.collection.concurrent.TrieMap
import scala.concurrent.Future
import scala.util.Try
import scala.util.{Failure, Success}

class Index(val builder: IndexBuilt) {

  import builder._
  val $this = this

  implicit val ctx: IndexContext = new IndexContext(builder)

  def findLeaf(node: Node, k: Datom): Future[Option[DataNode]] = {
    node match {
      case leaf: DataNode => Future.successful(Some(leaf))
      case meta: MetaNode => ctx.getNode(meta.findPath(k)).flatMap(findLeaf(_, k))
    }
  }

  def findLeaf(k: Datom): Future[Option[DataNode]] = {
    ctx.root match {
      case None => Future.successful(None)
      case Some(id) => ctx.getNode(id).flatMap(findLeaf(_, k))
    }
  }

  protected def fixRoot(p: Node): Future[Boolean] = {
    p match {
      case p: MetaNode =>

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

      case p: DataNode =>
        ctx.root = Some(p.id)
        ctx.setParent(p.id, None)

        Future.successful(true)
    }
  }

  protected def recursiveCopy(node: Node): Future[Boolean] = {
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

  def insertEmpty(list: Seq[Datom]): Try[Int] = {
    val leaf = ctx.createDataNode()

    ctx.root = Some(leaf.id)
    ctx.numElements += 1L
    ctx.incrementLevels()

    leaf.insert(list)
  }

  protected def insertParent(left: MetaNode, prev: Node): Future[Int] = {
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

  protected def handleParent(left: Node, right: Node): Future[Int] = {
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

  def insertDataNode(left: DataNode, list: Seq[Datom]): Future[Int] = {
    if(left.isFull){
      val right = left.split()
      return handleParent(left, right).map(_ => 0)
    }

    left.insert(list) match {
      case Success(n) => recursiveCopy(left).map(_ => n)
      case Failure(exception) => Future.failed(exception)
    }
  }

  protected def insertOrdered(list: Seq[Datom]): Future[Long] = {
    val sorted = list.sorted

    val length: Int = list.length
    var pos: Int = 0

    def insert(): Future[Long] = {
      if(pos == length) return Future.successful(length)

      var list = sorted.slice(pos, length)
      val k = list(0)

      findLeaf(k).flatMap {
        case None => insertEmpty(list) match {
          case Failure(ex) => Future.failed(ex)
          case Success(n) => Future.successful(n)
        }
        case Some(leaf) =>

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

  def insert(list: Seq[Datom]): Future[Long] = {
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

  protected def inOrder(root: Node): Future[Seq[Datom]] = {
    root match {
      case leaf: DataNode => Future.successful(leaf.inOrder())
      case meta: MetaNode => Future.sequence(meta.links.map { case (k, link) =>
        ctx.getNode(link).flatMap(inOrder(_))
      }.toSeq).map(_.flatten)
    }
  }

  def inOrder(): Future[Seq[Datom]] = {
    ctx.root match {
      case None => Future.successful(Seq.empty[Datom])
      case Some(root) => ctx.getNode(root).flatMap(inOrder(_))
    }
  }

  def getLeftMost(start: Option[Node]): Future[Option[DataNode]] = {
    start match {
      case None => Future.successful(None)
      case Some(b) => b match {
        case b: DataNode => Future.successful(Some(b))
        case b: MetaNode =>

          b.setPointers()

          ctx.getNode(b.links(0)._2).flatMap(b => getLeftMost(Some(b)))
      }
    }
  }

  def getRightMost(start: Option[Node]): Future[Option[DataNode]] = {
    start match {
      case None => Future.successful(None)
      case Some(b) => b match {
        case b: DataNode => Future.successful(Some(b))
        case b: MetaNode =>

          b.setPointers()

          ctx.getNode(b.links(b.links.length - 1)._2).flatMap(b => getRightMost(Some(b)))
      }
    }
  }

  def first(): Future[Option[DataNode]] = {
    if(ctx.root.isEmpty) return Future.successful(None)

    val root = ctx.root.get

    ctx.getNode(root).flatMap{ b =>
      ctx.setParent(b.id, None)
      getLeftMost(Some(b))
    }
  }

  def last(): Future[Option[DataNode]] = {
    if(ctx.root.isEmpty) return Future.successful(None)

    val root = ctx.root.get

    ctx.getNode(root).flatMap{ b =>
      ctx.setParent(b.id, None)
      getRightMost(Some(b))
    }
  }

  def next(current: Option[String]): Future[Option[DataNode]] = {

    def nxt(b: Node): Future[Option[DataNode]] = {

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

  def prev(current: Option[String]): Future[Option[DataNode]] = {

    def prv(b: Node): Future[Option[DataNode]] = {

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

  def inOrder(f: Datom => Boolean = _ => true): AsyncIndexIterator[Seq[Datom]] = new RichAsyncIndexIterator[Datom](f) {

    override def hasNext(): Future[Boolean] = {
      if(!firstTime) return Future.successful(ctx.root.isDefined)
      Future.successful(cur.isDefined)
    }

    override def next(): Future[Seq[Datom]] = {
      if(!firstTime){
        firstTime = true

        return first().map {
          case None =>
            cur = None
            Seq.empty[Datom]

          case Some(b) =>
            cur = Some(b)
            b.inOrder().filter(f)
        }
      }

      $this.next(cur.map(_.id)).map {
        case None =>
          cur = None
          Seq.empty[Datom]

        case Some(b) =>
          cur = Some(b)
          b.inOrder().filter(f)
      }
    }
  }

  def all(it: AsyncIndexIterator[Seq[Datom]] = inOrder()): Future[Seq[Datom]] = {
    it.hasNext().flatMap {
      case true => it.next().flatMap { list =>
        all(it).map {
          list ++ _
        }
      }
      case false => Future.successful(Seq.empty[Datom])
    }
  }

  def inOrder2(t: Long = Long.MaxValue, f: Datom => Boolean = _ => true): AsyncIndexIterator[Seq[Datom]] =
    new RichAsyncIndexIterator[Datom](f) {

      var lastKey: Option[Datom] = None

      protected def filterDatoms(datoms: IndexedSeq[Datom]): Seq[Datom] = {
        if(datoms.isEmpty) {
          if(lastKey.isEmpty) IndexedSeq.empty[Datom] else IndexedSeq(lastKey.get)
        }

        val grouped = TrieMap.from(datoms.groupBy{d => (d.e, d.a, d.value)}.map { case (k, values) =>
          k -> values.sortBy(_.t)
        })

        if(lastKey.isDefined && grouped.isDefinedAt((lastKey.get.e, lastKey.get.a, lastKey.get.value))){
          val k = (lastKey.get.e, lastKey.get.a, lastKey.get.value)
          grouped.put(k, (grouped(k) :+ lastKey.get).sortBy(_.t))
        } else if(lastKey.isDefined){
          grouped.put((lastKey.get.e, lastKey.get.a, lastKey.get.value), IndexedSeq(lastKey.get))
        }

        val curLast = datoms.last

        grouped.remove((curLast.e, curLast.a, curLast.value))

        lastKey = Some(curLast)

        grouped.map{case (_, values) => values.last}.filter(_.valid).toSeq.sorted(builder.ordering)
      }

      override def hasNext(): Future[Boolean] = {
        if(!firstTime) return Future.successful(ctx.root.isDefined)
        Future.successful(cur.isDefined)
      }

      override def next(): Future[Seq[Datom]] = {
        if(!firstTime){
          firstTime = true

          return first().map {
            case None =>
              cur = None
              IndexedSeq.empty[Datom]

            case Some(b) =>
              cur = Some(b)
              val filtered = b.data.filter(f)
              filterDatoms(filtered.filter(_.t <= t).toIndexedSeq)
          }
        }

        $this.next(cur.map(_.id)).map {
          case None =>
            cur = None
            if(lastKey.isEmpty) IndexedSeq.empty[Datom] else IndexedSeq(lastKey.get)

          case Some(b) =>
            cur = Some(b)
            val filtered = b.data.filter(f)
            filterDatoms(filtered.filter(_.t <= t).toIndexedSeq)
        }
      }
    }


}
