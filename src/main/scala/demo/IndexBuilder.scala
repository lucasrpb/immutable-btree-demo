package demo

import scala.concurrent.ExecutionContext
import scala.reflect.ClassTag

object IndexBuilder {

  case class IndexBuilt[K: ClassTag](order: Int,
                                     storage: Storage,
                                     implicit val ordering: Ordering[K],
                                     implicit val keySerializer: Serializer[K],
                                     implicit val ec: ExecutionContext) {
    val MIN: Int = order/2
    val MAX: Int = order

    val serializer = new Serializers.NodeSerializer[K](this)
  }

  protected class IndexBuilder[K: ClassTag](val order: Int,
                                  val ec: ExecutionContext,
                                  val keySerializer: Serializer[K],
                                  val ordering: Ordering[K]) {
    var storage: Option[Storage] = None

    def storage(storage: Storage): IndexBuilder[K] = {
      this.storage = Some(storage)
      this
    }

    def build(): IndexBuilt[K] = {
      assert(storage.isDefined)
      IndexBuilt(order, storage.get, ordering, keySerializer, ec)
    }
  }

  def builder[K: ClassTag](order: Int,
                 ec: ExecutionContext,
                 keySerializer: Serializer[K],
                 ordering: Ordering[K]): IndexBuilder[K] = {
    new IndexBuilder(order, ec, keySerializer, ordering)
  }
}
