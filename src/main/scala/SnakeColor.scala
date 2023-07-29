import javafx.scene.paint.Stop
import scalafx.scene.image.Image
import scalafx.scene.paint.{CycleMethod, LinearGradient}
enum SnakeColor:
  case Green, Blue, Orange, Purple

  def get_paint: scalafx.scene.paint.Color = {
    this match {
      case Green => scalafx.scene.paint.Color.Green
      case Purple => scalafx.scene.paint.Color.Purple
      case Orange => scalafx.scene.paint.Color.Orange
      case Blue => scalafx.scene.paint.Color.Blue
    }
  }
  private def get_stops: Array[Stop] = Array(new Stop(0,this.get_paint),new Stop(1,scalafx.scene.paint.Color.Black))

  private def lin_grad(horizontal: Boolean): LinearGradient = {
    val stops = get_stops
    if (horizontal)
      new LinearGradient(0,0,1,0,true,CycleMethod.NoCycle, stops)
    else
      new LinearGradient(0,0,0,1,true,CycleMethod.NoCycle, stops)
  }