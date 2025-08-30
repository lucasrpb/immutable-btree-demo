package demo

import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicInteger
import scala.collection.concurrent.TrieMap
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

object ReadTree {

  val numbers = new AtomicInteger(0)

  def main(args: Array[String]): Unit = {

    //val storage = new MemoryStorage()
    val storage = new CassandraStorage()

    implicit val builder = IndexBuilder
      .builder[Int](1024, global, intSerializer, implicitly[Ordering[Int]])
      .storage(storage)
      .build()

    val id = "demo"

    val ctx = Await.result(storage.getIndex(id), Duration.Inf).getOrElse(SerializableIndexContext.of(id, None))
    val index = new Index[Int](ctx)(builder)

    val list = Await.result(index.all(), Duration.Inf)

    storage.close()

    println()
  }

}
