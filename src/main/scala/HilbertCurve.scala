import processing.core._

class HilbertCurve extends PApplet {

    val DEFAULT_WIDTH = 1200
    val DEFAULT_HEIGHT = 800

    val INTERFACE_WIDTH = 400

    var shouldDrawGridLines = false
    var shouldOnlyDrawToTraversal = false

    val ORDER_MIN = 1
    val ORDER_MAX = 10

    var order: Int = ORDER_MIN
    var traversalIndex = 0

    var finalLines: List[Point] = List()

    override def settings(): Unit = {
        size(DEFAULT_WIDTH, DEFAULT_HEIGHT)
    }

    override def setup(): Unit = {
        getSurface.setResizable(true)
        getSurface.setTitle("Hilbert Curve")
        colorMode(PConstants.HSB, 360, 100, 100, 1)
        background(color(0, 0, 100))

    }

    override def draw(): Unit = {

        background(color(0, 0, 100))

        stroke(0, 0, 0)
        line(INTERFACE_WIDTH, 0, INTERFACE_WIDTH, height)

        textSize(32)
        fill(0, 0, 0)
        text(s"FPS: $frameRate", 50, 50)

        val baseLines = hilbertLineList(order, 2)

        val boxWidth: Float = ((width - INTERFACE_WIDTH) / math.pow(2, order)).asInstanceOf[Float]
        val boxHeight: Float = (height / math.pow(2, order)).asInstanceOf[Float]

        finalLines = baseLines.map(p => {
            val newX = INTERFACE_WIDTH + (p.x * boxWidth + boxWidth / 2)
            val newY = p.y * boxHeight + boxHeight / 2

            Point(newX, newY)
        })

        stroke(180, 50, 100)
        (INTERFACE_WIDTH.asInstanceOf[Float] until width.asInstanceOf[Float] by boxWidth).foreach(x => {
            line(x, 0, x, height)
        })
        (0f until height.asInstanceOf[Float] by boxHeight).foreach(y => {
            line(INTERFACE_WIDTH, y, width, y)
        })

        stroke(0, 0, 0)
        if (shouldOnlyDrawToTraversal) {
            finalLines.zipWithIndex.sliding(2).foreach(list => {
                val a = list.head
                val b = list.tail.head

                if (traversalIndex > a._2) {
                    line(a._1.x, a._1.y, b._1.x, b._1.y)
                }
            })
        } else {
            finalLines.sliding(2).foreach(list => {
                val a = list.head
                val b = list.tail.head

                line(a.x, a.y, b.x, b.y)
            })
        }

        ellipse(finalLines(traversalIndex).x - 2.5f, finalLines(traversalIndex).y - 2.5f, 5f, 5f)
    }

    def hilbertLineList(n: Int, quadrant: Int): List[Point] = {
        if (n == 0) {
            List(Point(0, 0))
        } else {
            val quadrant1 = hilbertLineList(n - 1, 1).map(p => {
                val newX = (getNewX(p.x, p.y, 0, n - 1) + math.pow(2, n - 1)).asInstanceOf[Float]
                val newY =  getNewY(p.x, p.y, 0, n - 1)
                Point(newX, newY)
            })
            val quadrant2 = hilbertLineList(n - 1, 2).map(p => {
                val newX = getNewX(p.x, p.y, 0, n - 1)
                val newY = getNewY(p.x, p.y, 0, n - 1)
                Point(newX, newY)
            })
            val quadrant3 = hilbertLineList(n - 1, 3).map(p => {
                val newX =  getNewX(p.x, p.y, 90, n - 1)
                val newY = (getNewY(p.x, p.y, 90, n - 1) + math.pow(2, n - 1)).asInstanceOf[Float]
                Point(newX, newY)
            })
            val quadrant4 = hilbertLineList(n - 1, 4).map(p => {
                val newX = (getNewX(p.x, p.y, -90, n - 1) + math.pow(2, n - 1)).asInstanceOf[Float]
                val newY = (getNewY(p.x, p.y, -90, n - 1) + math.pow(2, n - 1)).asInstanceOf[Float]
                Point(newX, newY)
            })

            quadrant match {
                case 1 => quadrant3 ::: quadrant2 ::: quadrant1 ::: quadrant4
                case 2 => quadrant3 ::: quadrant2 ::: quadrant1 ::: quadrant4
                case 3 => quadrant4.reverse ::: quadrant1.reverse ::: quadrant2.reverse ::: quadrant3.reverse
                case 4 => quadrant4.reverse ::: quadrant1.reverse ::: quadrant2.reverse ::: quadrant3.reverse
            }
        }
    }

    def getNewX(x: Float, y: Float, d: Float, order: Int): Float = {
        if (order > 0) {
            val translatedX: Float = (x - (math.pow(2, order - 1) - 0.5)).asInstanceOf[Float]
            val translatedY: Float = (y - (math.pow(2, order - 1) - 0.5)).asInstanceOf[Float]

            val rotatedX: Float = (translatedX * math.cos(math.toRadians(d)) - translatedY * math.sin(math.toRadians(d))).asInstanceOf[Float]

            val finalX = (rotatedX + (math.pow(2, order - 1) - 0.5)).asInstanceOf[Float]
            finalX
        } else {
            0
        }

    }

    def getNewY(x: Float, y: Float, d: Float, order: Int): Float = {
        if (order > 0) {
            val translatedX: Float = (x - (math.pow(2, order - 1) - 0.5)).asInstanceOf[Float]
            val translatedY: Float = (y - (math.pow(2, order - 1) - 0.5)).asInstanceOf[Float]

            val rotatedY: Float = (translatedY * math.cos(math.toRadians(d)) + translatedX * math.sin(math.toRadians(d))).asInstanceOf[Float]

            val finalY = (rotatedY + (math.pow(2, order - 1) - 0.5)).asInstanceOf[Float]
            finalY
        } else {
            0
        }
    }

    override def keyPressed(): Unit = {
        key match {
            case ',' =>
                if (order > ORDER_MIN) {
                    order -= 1
                }
                traversalIndex = 0
            case '.' =>
                if (order < ORDER_MAX) {
                    order += 1
                }
                traversalIndex = 0
            case 'g' =>
                shouldDrawGridLines = !shouldDrawGridLines
            case 't' =>
                shouldOnlyDrawToTraversal = !shouldOnlyDrawToTraversal
            case PConstants.CODED =>
                keyCode match {
                    case PConstants.LEFT =>
                        if (traversalIndex > 0) {
                            traversalIndex -= 1
                        }
                    case PConstants.RIGHT =>
                        if (traversalIndex < finalLines.length - 1) {
                            traversalIndex += 1
                        }
                    case _ =>
                }
            case _ =>
        }
    }

}

object HilbertCurve extends PApplet {
    def main(args: Array[String]): Unit = {
        PApplet.main(classOf[HilbertCurve].getName)
    }
}

case class Point(x: Float, y: Float)