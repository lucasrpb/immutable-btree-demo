package demo

import java.util.UUID
import java.util.concurrent.{ExecutorService, Executors, ThreadLocalRandom}
import java.util.concurrent.atomic.AtomicLong
import scala.collection.concurrent.TrieMap
import scala.concurrent.{Await, ExecutionContext, Future}
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

    val storage = new MemoryStorage()
    //implicit val ioExecutionContext = ExecutionContext.fromExecutorService(ioThreadPool)

    val builder = IndexBuilder
      .builder[Datom](1024, global, datomSerializer, ordering)
      .storage(storage)
      .build()

    val allData = TrieMap.empty[Datom, Datom]
    val index = new DatomIndex(SerializableIndexContext.of("datom-demo", None), builder)

    val id = "demo-index"

    for(j<-0 until 1_000){

      var data = Seq.empty[Datom]
      val tmp = timecounter.getAndIncrement() //System.nanoTime()

      for(i<-0 until 1_000){
        val id = UUID.randomUUID().toString
        val food = foods(rand.nextInt(0, foods.length))

        val likes = Datom(id, "favorite-food", DatomTypes.STRING, food, tmp)
        val owns = Datom(id, "owns", DatomTypes.STRING, objects(rand.nextInt(0, objects.length)), tmp)

        val newFavoriteFood = Datom(id, "favorite-food", DatomTypes.STRING, "other-food", tmp + 1L)
        val deletePrevious = Datom(likes.e, likes.a, DatomTypes.STRING, food, tmp + 1L, false)

        data = data ++ Seq(likes, owns, deletePrevious)

        if(rand.nextBoolean()){
          data = data :+ newFavoriteFood
        }
      }

      println(s"insertion nbr: ${j}...")

      //TimeProfiler.snap()
      val insertion = Await.result(index.insert(data), Duration.Inf)
      //TimeProfiler.snap()

      data.foreach { d =>
        allData.put(d, d)
      }

      //TimeProfiler.addAndGet(t1 - t0)

      assert(insertion > 0L)
    }

    println("filled...")

    val t = 1L

    val list1 = allData.values.toSeq
      .filter(_.t <= t).filter(_.valid)
      .sorted(ordering)

    val elapsedInsertion = TimeProfiler.calculate()/1_000_000

    println(s"insertion: ${elapsedInsertion} ms")

    val t2 = System.nanoTime()
    val list2 = Await.result(index.all(index.inOrder2(t)), Duration.Inf)
    val t3 = System.nanoTime()
    val elapsedInOrder = (t3 - t2)/1_000_000

    println(s"inorder: ${elapsedInOrder} ms levels: ${index.ctx.levels}")

    if(list2 != list1){
      assert(false)
    }

    println()
  }

}
