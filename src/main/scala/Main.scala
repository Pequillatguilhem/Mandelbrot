
import java.awt.event.MouseAdapter
import java.awt.{BasicStroke, Color, Font, Graphics, Graphics2D, event}
import javax.swing.{JFrame, JPanel}
import scala.annotation.tailrec
import scala.math.BigDecimal.double2bigDecimal
import scala.math.Numeric.BigDecimalAsIfIntegral.toDouble
import scala.math._
import fs2.Stream
import cats.effect.unsafe.implicits.global
import cats.effect.IO
import cats.implicits._

object Main extends App {

  val pixelSize = 1
  val height = 750
  val width = 750
  val nbPix = 750
  val maxIteration = 5000

  def toPixel(x: Double, y: Double, xReal: (Double, Double), yReal: (Double, Double)) : (Int, Int) = {
    (
      rint(((x - xReal._1)/(xReal._2 - xReal._1)) * width ).toInt,
      rint(((y - yReal._1)/(yReal._2 - yReal._1)) * height).toInt
    )
  }
  def toReal(x: Int, y: Int, xReal: (Double, Double), yReal: (Double, Double)) : (Double, Double) = {
    (
      (x.toDouble/width.toDouble) * (xReal._2 - xReal._1) + xReal._1 ,
      (y.toDouble/height.toDouble) * (yReal._2 - yReal._1) + yReal._1
    )
  }
  def addSquare(x: Int, y: Int, color: Color, g: Graphics) = {
    g.setColor(color)
    g.fillRect(x, y, pixelSize, pixelSize)
  }


  @tailrec
  def getNbIteration(x0: Double, y0: Double, x: Double = 0.0, y: Double = 0.0, iteration: Int = 0): Int = {
    if (x*x + y*y <= 2 * 2 && iteration < maxIteration) {
      val xnew = x * x - y * y + x0
      val ynew = 2 * x * y + y0
      getNbIteration(x0,y0, xnew, ynew,  iteration + 1)
    } else {
      iteration
    }
  }
  def toColor(iteration: Int) : Color = {
    iteration match {
      case i if i < 255 => new Color( i, 0, 0)
      case i if i < 510 => new Color(255,  i - 255, 0)
      case i if i < 766 => new Color(i - 510,  120,  i - 510)
      case i => new Color(i % 255,0,0)
    }
  }

  def getList(xReal: (Double, Double), yReal: (Double, Double)): List[(Double, Double)] = {
    val xList = (xReal._1 to xReal._2 by (xReal._2 - xReal._1)/nbPix).toList.map(toDouble)
    val yList = (yReal._1 to yReal._2 by (yReal._2 - yReal._1)/nbPix).toList.map(toDouble)
    for {
      x <- xList
      y <- yList
    } yield (x, y)
  }
  def toIteration(c : (Double, Double)) : IO[(Int, (Double, Double))] = {
      IO(getNbIteration(c._1, c._2),c)
  }
  def toIterationBatch(list :List[(Double, Double)]) : IO[List[(Int, (Double, Double))]] = {
    IO(list.map(c =>{

        (getNbIteration(c._1, c._2),c)
      }))
  }

  def displayConcurrentBatch(g: Graphics, xReal: (Double, Double), yReal: (Double, Double)) = {
    Stream.emits[IO, List[(Double, Double)]](getList(xReal, yReal).grouped((getList(xReal, yReal).length / 15).toInt).toList)
      .mapAsyncUnordered(16)(a=>toIterationBatch(a))
      .compile
      .toList
      .unsafeRunSync()
      .flatten
      .map(a => {
        val color = (toColor(a._1), a._2)
        val pixel = (color._1, toPixel(color._2._1, color._2._2, xReal, yReal))
        addSquare(pixel._2._1, pixel._2._2, pixel._1, g)
      })
  }

  def displayConcurent(g: Graphics, xReal: (Double, Double), yReal: (Double, Double)) = {
    Stream.emits[IO, (Double, Double)](getList(xReal, yReal))
      .mapAsyncUnordered(8)(a=>toIteration(a))
      .compile
      .toList
      .unsafeRunSync()
      .map(a => (toColor(a._1), a._2))
      .map(a => (a._1, toPixel(a._2._1, a._2._2, xReal, yReal)))
      .map(a => addSquare(a._2._1, a._2._2, a._1, g))
  }

  def display(g: Graphics, xReal: (Double, Double), yReal: (Double, Double)) = {
    val xList = (xReal._1 to xReal._2 by (xReal._2 - xReal._1)/nbPix).toList.map(toDouble)
    val yList = (yReal._1 to yReal._2 by (yReal._2 - yReal._1)/nbPix).toList.map(toDouble)
    for {
      x <- xList
      y <- yList
      nbI = getNbIteration(x, y)
      color = toColor(nbI)
      (xS, yS) = toPixel(x, y, xReal, yReal)
      _ = addSquare(xS, yS, color, g)
    } yield ()
  }

  val frame = new JFrame("Mandelbrot")
  var zoom = 1.0
  var actualCoord = ((-2.0 + 0.47)/2, (-1.12 +1.12)/2)
  frame.add(new JPanel()  {
    import java.awt.Graphics
    override protected def paintComponent(g: Graphics): Unit = {
      displayConcurrentBatch(g,(-2.0 , 0.47), (-1.12 ,1.12))
    }
  })
  frame.setVisible(true)
  frame.addMouseListener(new MouseAdapter() {
    override def mousePressed(e: event.MouseEvent): Unit = {
      frame.add(new JPanel()  {
        import java.awt.Graphics
        override protected def paintComponent(g: Graphics): Unit = {
          val coord = toReal(e.getX, e.getY-40, (actualCoord._1 - 1.0/zoom,actualCoord._1 + 1.0/zoom), (actualCoord._2 - 1.0/zoom,actualCoord._2 + 1.0/zoom))
          actualCoord = ((coord._1 - 1.0/zoom+ coord._1 + 1.0/zoom) /2, (coord._2 - 1.0/zoom+coord._2 + 1.0/zoom)/2)
          displayConcurrentBatch(g,(coord._1 - 1.0/zoom,coord._1 + 1.0/zoom), (coord._2 - 1.0/zoom,coord._2 + 1.0/zoom))
          zoom = zoom * 1.8
        }

      })
      frame.setVisible(true)
    }
  })
  frame.setSize(width, height)
  frame.setVisible(true)
}
