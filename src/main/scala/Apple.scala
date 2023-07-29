import scala.beans.BeanProperty
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class Apple(val position:(Int,Int)){
}
object Apple{
  def apply(size: Int): Apple = {
    val rand = new Random()
    val apple = new Apple((rand.nextInt(size),rand.nextInt(size)))
    apple
  }
}