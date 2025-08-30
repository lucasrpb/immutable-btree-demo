package demo

trait Node[K] {
  val id: String
  val MIN: Int
  val MAX: Int

  def lastKey: K

  def copy()(implicit ctx: IndexContext[K]): Node[K]
  def split()(implicit ctx: IndexContext[K]): Node[K]

  def length: Int
  def remaning: Int = MAX - length
  def isFull: Boolean = length >= MAX
  def isEmpty: Boolean = length == 0

  var isNew: Boolean = true
}
