package demo

import java.util.concurrent.atomic.{AtomicLong, AtomicReference}

object TimeProfiler {

  val timer = new AtomicLong(0L)
  var t0: Option[Long] = None

  def snap(): Unit = synchronized {
    if(t0.isEmpty){
      t0 = Some(System.nanoTime())
    } else {
      val elapsed = System.nanoTime() - t0.get

      println(s"elapsed: ${elapsed}")

      timer.addAndGet(elapsed)
      t0 = None
    }
  }

  def calculate(): Long = {
    timer.get()
  }
}
