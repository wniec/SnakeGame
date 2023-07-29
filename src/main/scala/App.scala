import javafx.css.Stylesheet
import javafx.event.Event
import javafx.scene.input.MouseEvent
import scalafx.Includes.when
import scalafx.scene.image.{Image, ImageView}
import scalafx.application.{JFXApp3, Platform}
import scalafx.beans.property.IntegerProperty
import scalafx.scene.{Node, Scene}
import scalafx.scene.paint.Color.*
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.control.{Button, Label}
import scalafx.scene.layout.{Background, BackgroundFill, BackgroundImage, BackgroundPosition, BackgroundRepeat, BackgroundSize, ColumnConstraints, GridPane, HBox, RowConstraints, VBox}
import scalafx.scene.paint.{Color, LinearGradient}
import scalafx.scene.shape.{Circle, Rectangle, Shape}
import scalafx.scene.text.Font
import scalafx.stage.Stage

import scala.collection.immutable.Map
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global


object App extends JFXApp3 {
  private var gameEnded = false
  private val size = 24
  private var board: Board = new Board(size)
  private val h = 640
  private val w = 640
  private var winnerSnake: Option[Snake] = None
  private def h_unit: Int = h / size
  private def w_unit: Int = w / size
  private def square(color: Color=Color.color(1,0.5,1)) =new Rectangle{
    fill = color
    width = w_unit
    height = h_unit
  }
  private def drawApple= {
    new Circle {
      fill = Red
      radius = h_unit / 2
    }
  }
  private def drawSnake(snake: Snake)={
    val color = snake.color.get_paint
    square(color)
    }
  private def makeBoard={
    val gridPane =new GridPane{}
    val aliveSnakes = board.snakes.filter((s: Snake) => s.alive)
    val snakesPositions: List[List[((Int, Int), Snake)]] = aliveSnakes.toList.map((snake: Snake) => snake.body.toList.map((pos: (Int, Int)) => (pos,snake)))
    val snakesTiles: List[((Int, Int), Snake)] = snakesPositions.reduce((a: List[((Int, Int), Snake)], b: List[((Int, Int), Snake)]) => a ::: b)
    val applesMap = board.apples.toList.map((a: Apple) => a.position).toSet
    val snakesMap = snakesTiles.toMap[(Int,Int),Snake]
    (0 until size).foreach((_: Int) => {
      val rc = new RowConstraints()
      val cc = new ColumnConstraints()
      cc.setPrefWidth(w_unit)
      rc.setPrefHeight(h_unit)
      gridPane.getRowConstraints.add(rc)
      gridPane.getColumnConstraints.add(cc)
    })
    for (xs <- 0 until size; ys <- 0 until size){
      val pos = (xs,ys)
      if(snakesMap.contains(pos)){
        val tile = drawSnake(snakesMap(pos))
        gridPane.add(tile, xs, ys)
      }else if (applesMap.contains(pos)){
        val tile = drawApple
        gridPane.add(tile, xs, ys)
      }else{
        val tile = square()
        gridPane.add(tile, xs, ys)
      }
    }
    gridPane.setAlignment(Pos.Center)
    gridPane.styleClass = List("grid-pane")
    gridPane.setBackground(Background.fill(Purple))
    gridPane.setGridLinesVisible(true)
    gridPane
  }
  private def update(): Unit = {
    board.snakes.foreach((s: Snake) => board.move(s))
    if(check)
      stage.getScene.setRoot(makeBoard)
    else {
      gameEnded = true
      val winner = board.snakes.filter((s: Snake) => s.alive)
      if (winner.length == 1)
        winnerSnake = Some(winner.head)
      gameOver()
    }
  }
  private def check: Boolean =board.snakes.count((s: Snake) => s.alive)>1
  private def gameLoop(update : () => Unit): Unit = {
    if(!gameEnded){
      Future {
        this.update()
        Thread.sleep(400)
      }.flatMap(_ => Future(gameLoop(update)))
    }
  }
  private def gameOver(): Unit ={
    var text:String =""
    var color = Color.White
    var textColor = Color.White
    if (winnerSnake.nonEmpty) {
      val snakeColor = winnerSnake.get.color
      text = snakeColor.toString + " won"
      color = snakeColor.get_paint
    } else {
      textColor = Color.Black
      text = "no one won"
    }
    val label: Label = new Label(text)
    label.setTextFill(textColor)
    val box = new HBox(){
      children = Seq(label)
      background = Background.fill(color)
    }
    stage.getScene.setRoot(box)
  }
  private def quit_button={
    val quit_button: Button = new Button("EXIT")
    quit_button.onMouseClicked = (_: MouseEvent) => {
      stage.close()
      stopApp()
    }
    quit_button
  }
  private def start_button = {
    val start_button: Button = new Button("START NEW GAME")
    start_button.onMouseClicked = (_: MouseEvent) => {
      board = new Board(size)
      stage.getScene.setRoot(makeBoard)
      gameLoop(update)
    }
    start_button
  }
  private def start_button_box ={
    val box = new VBox(10.0,start_button,quit_button)
    box
  }


  override def start(): Unit = {
    stage = new JFXApp3.PrimaryStage {
      title.value = "The Snake Game"
      fullScreen = true
      scene = new Scene {
        stylesheets = List("styles.css")
        resizable = true
        minWidth = 1280
        minHeight = 960
        onKeyPressed =key => key.getText match {
          case "q" => board.snakes.head.turn("left")
          case "e" => board.snakes.head.turn("right")
          case "t" => board.snakes(1).turn("left")
          case "u" => board.snakes(1).turn("right")
          case "d" => board.snakes(2).turn("left")
          case "g" => board.snakes(2).turn("right")
          case "b" => board.snakes(3).turn("left")
          case "m" => board.snakes(3).turn("right")
          case _ =>
        }
        root = new VBox{
          styleClass = List("background")
          children = Seq(start_button_box)
          alignment = Pos.Center
        }
      }
    }
  }
}