package demo

/**
 * This is a single threaded iterator that can be copied.
 *
 * @param filter
 * @param limit
 * @tparam K
 * @tparam V
 */
abstract class RichAsyncIndexIterator[K](var filter: K => Boolean = (_: K) => true,
                                            var limit: Int = -1) extends AsyncIndexIterator[Seq[K]] {

  assert(limit != 0, "Limit must be < 0 (infinite) or > 0!")

  protected var counter = 0
  protected var cur: Option[Node[K]] = None

  protected var firstTime = false
  protected var stop = false

  def checkCounter(filtered: Seq[K]): Seq[K] = {
    if(limit < 0) {
      counter += filtered.length
      return filtered
    }

    if(counter + filtered.length >= limit){
      stop = true
    }

    val n = Math.min(filtered.length, limit - counter)

    if(n <= 0) return Seq.empty[K]

    counter += n

    filtered.slice(0, n)
  }

}
