package demo

import scala.collection.concurrent.TrieMap
import scala.concurrent.Future

class DatomIndex(override val builder: IndexBuilder.IndexBuilt[Datom]) extends Index[Datom](builder){

  import builder._

  def inOrder2(t: Long = Long.MaxValue, f: Datom => Boolean = _ => true): AsyncIndexIterator[Seq[Datom]] =
    new RichAsyncIndexIterator[Datom](f) {

      var lastKey: Option[Datom] = None

      protected def filterDatoms(datoms: IndexedSeq[Datom]): Seq[Datom] = {
        if(datoms.isEmpty) {
          return if(lastKey.isEmpty) IndexedSeq.empty[Datom] else IndexedSeq(lastKey.get)
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
            filterDatoms(filtered.filter(_.t <= t))
        }
      }
    }


}
