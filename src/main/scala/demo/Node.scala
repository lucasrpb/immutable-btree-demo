package demo

trait Node {
  val id: String
  val MIN: Int
  val MAX: Int

  def lastKey: Datom

  def copy()(implicit ctx: IndexContext): Node
  def split()(implicit ctx: IndexContext): Node

  def length: Int = 0
  def remaning: Int = MAX - length
  def isFull: Boolean = length == MAX
  def isEmpty: Boolean = length == 0

  var isNew: Boolean = true
}
