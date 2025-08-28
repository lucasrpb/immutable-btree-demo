package demo

import scala.concurrent.ExecutionContext

object IndexBuilder {

  case class IndexBuilt(order: Int, storage: Storage, implicit val ec: ExecutionContext) {
    val MIN: Int = order/2
    val MAX: Int = order

    implicit val ordering: Ordering[Datom] = demo.ordering

    val serializer = new Serializers.NodeSerializer(this)
  }

  protected class IndexBuilder(val order: Int, val ec: ExecutionContext) {
    var storage: Option[Storage] = None

    def storage(storage: Storage): IndexBuilder = {
      this.storage = Some(storage)
      this
    }

    def build(): IndexBuilt = {
      assert(storage.isDefined)
      IndexBuilt(order, storage.get, ec)
    }
  }

  def builder(order: Int, ec: ExecutionContext): IndexBuilder = {
    new IndexBuilder(order, ec)
  }
}
