package demo

import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicInteger
import scala.collection.concurrent.TrieMap
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

object Main {

  val numbers = new AtomicInteger(0)

  def main(args: Array[String]): Unit = {

    //val storage = new MemoryStorage()
    val storage = new CassandraStorage()

    implicit val builder = IndexBuilder
      .builder[Int](1024, global, intSerializer, implicitly[Ordering[Int]])
      .storage(storage)
      .build()

    var data = TrieMap.empty[Int, Int]
    val id = "demo"

    //TimeProfiler.snap()

    for(i <- 0 until 1_000){

      val ctx = Await.result(storage.getIndex(id), Duration.Inf).getOrElse(SerializableIndexContext.of(id, None))
      val index = new Index[Int](ctx)(builder)

      var list = IndexedSeq.empty[Int]

      for(j<-0 until 1_000){
        val k = numbers.getAndIncrement()
        list = list :+ k
      }

      val toInsert = scala.util.Random.shuffle(list)

      TimeProfiler.snap()
      val result = Await.result(index.insert(toInsert).flatMap { r =>
        TimeProfiler.snap()
        index.ctx.save().map(_ -> r)}, Duration.Inf)

      if(result._2 != toInsert.length){
        assert(false)
      }

      data = data ++ list.map{x => x -> x}

      println(s"${Console.GREEN_B}insertion: "+i+s"${Console.RESET}")
    }

    //TimeProfiler.snap()

    println("insertion finished...")

    val elapsedInsertion = TimeUnit.MILLISECONDS.convert(TimeProfiler.calculate(), TimeUnit.NANOSECONDS)
    println(s"insertion time: ${elapsedInsertion} ms")

    val ctx = Await.result(storage.getIndex(id), Duration.Inf).getOrElse(SerializableIndexContext.of(id, None))
    val index = new Index[Int](ctx)(builder)

    val ref = data.values.toSeq.sorted
    val list = Await.result(index.all(), Duration.Inf)

    storage.close()

    if(list != ref){
      assert(false)
    }

    println()
  }

}
