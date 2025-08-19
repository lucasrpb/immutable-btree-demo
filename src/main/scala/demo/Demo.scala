package demo

import java.util.UUID
import java.util.concurrent.ThreadLocalRandom
import java.util.concurrent.atomic.AtomicLong
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

object Demo {

  val rand = ThreadLocalRandom.current()

  def main(args: Array[String]): Unit = {
    var data = Seq.empty[Datom]

    val timecounter = new AtomicLong(0L)

    val foods = (0 until 100).map { i =>
      "lasagna-"+i
    }

    val objects = (0 until 100).map { i =>
      "iphone-"+i
    }

    for(i<-0 until 100_000){
      val id = UUID.randomUUID().toString
      val tmp = timecounter.getAndIncrement()//System.nanoTime()

      val likes = Datom(id, "favorite-food", DatomTypes.STRING, foods(rand.nextInt(0, foods.length)), tmp)
      val owns = Datom(id, "owns", DatomTypes.STRING, objects(rand.nextInt(0, objects.length)), tmp)

      data = data ++ Seq(likes, owns)
    }

    println("filled...")

   // val datoms = data.map(caseClassToDatom(_))

    val storage = new MemoryStorage()

    val builder = IndexBuilder
      .builder(2048, global)
      .storage(storage)
      .build()

    val index = new Index(builder)

    val t0 = System.nanoTime()
    val list1 = data.sorted(builder.ordering)
    val insertion = Await.result(index.insert(data), Duration.Inf)

    val t1 = System.nanoTime()
    val elapsedInsertion = (t1 - t0)/1_000_000

    println(s"insertion: ${elapsedInsertion} ms")

    val t2 = System.nanoTime()
    val list2 = Await.result(index.all(), Duration.Inf)
    val t3 = System.nanoTime()
    val elapsedInOrder = (t3 - t2)/1_000_000

    val equal = list2 == list1

    println(s"inorder: ${elapsedInOrder} ms")

    println()
  }

}
