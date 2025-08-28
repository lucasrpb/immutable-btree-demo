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
      .builder(4096, global)
      .storage(storage)
      .build()

    val allData = TrieMap.empty[Datom, Datom]
    val index = new Index(builder)

    for(j<-0 until 1_00){

      var data = Seq.empty[Datom]
      val tmp = timecounter.getAndIncrement()//System.nanoTime()

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

      TimeProfiler.snap()
      val insertion = Await.result(index.insert(data), Duration.Inf)
      TimeProfiler.snap()

      //TimeProfiler.addAndGet(t1 - t0)

      assert(insertion > 0L)
    }

    println("filled...")

    //val list1 = data.sorted(builder.ordering)

    val elapsedInsertion = TimeProfiler.calculate()/1_000_000

    println(s"insertion: ${elapsedInsertion} ms")

    val t2 = System.nanoTime()
    val list2 = Await.result(index.all(index.inOrder2()), Duration.Inf)
    val t3 = System.nanoTime()
    val elapsedInOrder = (t3 - t2)/1_000_000

    val list3 = Await.result(index.all(index.inOrder2(50)), Duration.Inf)

  //  val equal = list2 == list1

    println(s"inorder: ${elapsedInOrder} ms")

    val head = list2.head

    val r = Await.result(index.get(head.copy(a = "owns"))(comparator), Duration.Inf)

    println(s"time elapsed ${TimeCounter.get()/1_000_000_000.0} s")

    /*val node = index.ctx.newBlocksReferences.filter(_._2.isInstanceOf[MetaNode]).head._2.asInstanceOf[MetaNode]
    val buffer = builder.serializer.serialize(node)
    val dnode = builder.serializer.deserialize(buffer).get.asInstanceOf[MetaNode]

    assert(dnode.inOrder() == node.inOrder())*/

    println()
  }

}
