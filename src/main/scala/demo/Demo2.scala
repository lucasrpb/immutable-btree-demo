package demo

import java.util.UUID
import java.util.concurrent.ThreadLocalRandom
import java.util.concurrent.atomic.AtomicLong
import scala.collection.concurrent.TrieMap
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

object Demo2 {

  val rand = ThreadLocalRandom.current()

  def main(args: Array[String]): Unit = {

    val timecounter = new AtomicLong(0L)

    val foods = (0 until 100).map { i =>
      "lasagna-"+i
    }

    val objects = (0 until 100).map { i =>
      "iphone-"+i
    }

    // val datoms = data.map(caseClassToDatom(_))

    val storage = new MemoryStorage()

    val builder = IndexBuilder
      .builder(2048, global)
      .storage(storage)
      .build()

    val allData = TrieMap.empty[Datom, Datom]
    val index = new Index(builder)

    val t0 = System.nanoTime()

    for(j<-0 until 100){

      var data = Seq.empty[Datom]
      val tmp = timecounter.getAndIncrement()//System.nanoTime()

      for(i<-0 until 1_000){
        val id = UUID.randomUUID().toString
        val food = foods(rand.nextInt(0, foods.length))

        val likes = Datom(id, "favorite-food", DatomTypes.STRING, food, tmp)
        val owns = Datom(id, "owns", DatomTypes.STRING, objects(rand.nextInt(0, objects.length)), tmp)

        val newFavoriteFood = Datom(id, "favorite-food", DatomTypes.STRING, "other-food", tmp + 1L)
        val deletePrevious = Datom(likes.e, likes.a, DatomTypes.STRING, food, tmp + 1L, false)

        data = data ++ Seq(likes, owns, deletePrevious, newFavoriteFood)
      }

      println(s"insertion nbr: ${j}...")

      val insertion = Await.result(index.insert(data), Duration.Inf)

      assert(insertion > 0L)
    }

    println("filled...")

    //val list1 = data.sorted(builder.ordering)

    val t1 = System.nanoTime()
    val elapsedInsertion = (t1 - t0)/1_000_000

    println(s"insertion: ${elapsedInsertion} ms")

    val t2 = System.nanoTime()
    val list2 = Await.result(index.all(index.inOrder2()), Duration.Inf)
    val t3 = System.nanoTime()
    val elapsedInOrder = (t3 - t2)/1_000_000

    val list3 = Await.result(index.all(index.inOrder2(50)), Duration.Inf)

  //  val equal = list2 == list1

    println(s"inorder: ${elapsedInOrder} ms")

    println()
  }

}
