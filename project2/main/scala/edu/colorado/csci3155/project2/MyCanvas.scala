package edu.colorado.csci3155.project2
import scala.collection.mutable

/* A class to maintain a canvas. */
import java.awt.geom.{Ellipse2D, Rectangle2D}
import java.awt.{Graphics2D}

/* A figure is a sealed trait. It can be a Polygon or a "MyCircle"*/
sealed trait Figure {
    def getBoundingBox: (Double, Double, Double, Double)
    def translate(shiftX: Double, shiftY: Double): Figure
    def rotate(angRad: Double): Figure
    def render(g: Graphics2D, scaleX: Double, scaleY: Double, shiftX: Double, shiftY: Double): Unit
}

/*
 Class Polygon
   A polygon is defined by a list of its vertices
 */

case class Polygon(val cList: List[(Double, Double)]) extends Figure {
    //TODO: Define the bounding box of the polygon
    override def getBoundingBox: (Double, Double, Double, Double ) = {
        val (xList, yList) = cList.unzip
        val maxX = xList.max
        val minX = xList.min
        val maxY = yList.max
        val minY = yList.min
        (minX, maxX, minY, maxY)


    }
    //TODO: Create a new polygon by shifting each vertex in cList by (x,y)
    //    Do not change the order in which the vertices appear
    override def translate(shiftX: Double, shiftY: Double): Polygon = {
        val (xList, yList) = cList.unzip
        val x = xList.map(_ + shiftX)
        val y = yList.map(_ + shiftY)
        val newList = x zip y
        Polygon(newList)
    }

    override def rotate(angRad: Double): Figure = {
        val newX = cList.map{ case (x, y) => x * math.cos(angRad) - y * math.sin(angRad)}
        val newY = cList.map{ case (x, y) => x * math.sin(angRad) + y * math.cos(angRad)}
        Polygon( newX zip newY)
    }
    // Function: render -- draw the polygon. Do not edit this function.
    override def render(g: Graphics2D, scaleX: Double, scaleY: Double, shiftX: Double, shiftY: Double) = {
        val xPoints: Array[Int] = new Array[Int](cList.length)
        val yPoints: Array[Int] = new Array[Int](cList.length)
        for (i <- 0 until cList.length){
            xPoints(i) = ((cList(i)._1 + shiftX )* scaleX).toInt
            yPoints(i) = ((cList(i)._2 + shiftY) * scaleY).toInt
        }
        g.drawPolygon(xPoints, yPoints, cList.length)
    }
}

/*
  Class MyCircle
  Define a circle with a given center c and radius r
 */
case class MyCircle(val c: (Double, Double), val r: Double) extends Figure {
    //TODO: Define the bounding box for the circle
    override def getBoundingBox: (Double, Double, Double, Double) = {
        val maxX = c._1 + r
        val maxY = c._2 + r
        val minX = c._1 - r
        val minY = c._2 - r
        return (minX, maxX, minY, maxY)
    }


    //TODO: Create a new circle by shifting the center
    override def translate(shiftX: Double, shiftY: Double): MyCircle = {
        return MyCircle((c._1 + shiftX, c._2 + shiftY), r)
    }


    override def rotate(angRad: Double): Figure = {
        val newX = c._1 * math.cos(angRad) - c._2 * math.sin(angRad)
        val newY = c._1 * math.sin(angRad) + c._2 * math.cos(angRad)
        val newC = (newX, newY)
        MyCircle(newC, r)
    }

    // Function: render -- draw the polygon. Do not edit this function.
    override def render(g: Graphics2D, scaleX: Double, scaleY: Double, shiftX: Double, shiftY: Double) = {
        val centerX = ((c._1 + shiftX) * scaleX) .toInt
        val centerY = ((c._2 + shiftY) * scaleY) .toInt
        val radX = (r * scaleX).toInt
        val radY = (r * math.abs(scaleY)).toInt
        //g.draw(new Ellipse2D.Double(centerX, centerY, radX, radY))
        g.drawOval(centerX-radX, centerY-radY, 2*radX, 2*radY)
    }



}

/*
  Class : MyCanvas
  Define a canvas through a list of figure objects. Figure objects can be circles or polygons.
 */
class MyCanvas (val listOfObjects: List[Figure]) {
    // Hint: use existing boundingbox functions defined in each figure.
    def getBoundingBox: (Double, Double, Double, Double) = {
        val newList = listOfObjects.map(x => x.getBoundingBox)
        val maxX = newList.maxBy(_._2)
        val minX = newList.minBy(_._1)
        val maxY = newList.maxBy(_._4)
        val minY = newList.minBy(_._3)
        (minX._1, maxX._2, minY._3, maxY._4)
    }

    //TODO: Write a function to translate each figure in the canvas by shiftX, shiftY
    def translate(shiftX: Double, shiftY: Double): MyCanvas = {
        val newL = listOfObjects.map(x => x.translate(shiftX, shiftY))
        new MyCanvas(newL)
    }


    //TODO: Write a function that will return a new MyCanvas object that places
    // all the objects in myc2 to the right of the objects in this MyCanvas.
    // refer to the notebook documentation on how to perform this.
    def placeRight(myc2: MyCanvas):MyCanvas = {
        val c1B = getBoundingBox
        val c2B = myc2.getBoundingBox
        val xMax1 = c1B._2
        val xMin1 = c1B._1
        val yMax1 = c1B._4
        val yMin1 = c1B._3
        val yMax2 = c2B._4
        val yMin2 = c2B._3
        val xShift = xMax1 - xMin1
        val yShift = (yMax1 - yMin1)/2 - (yMax2 - yMin2)/2
        val c2 = myc2.translate(xShift, yShift)
        overlap(c2)
    }

    //TODO: Write a function that will return a new MyCanvas object that places
    // all the figures in myc2 on top of the figures in this MyCanvas.
    // refer to the notebook documentation on how to perform this.
    def placeTop(myc2: MyCanvas): MyCanvas = {
        val c1B = getBoundingBox
        val c2B = myc2.getBoundingBox
        val xMax1 = c1B._2
        val xMin1 = c1B._1
        val xMax2 = c2B._2
        val xMin2 = c2B._1
        val yMax1 = c1B._4
        val yMin1 = c1B._3
        val xShift = (xMax1 - xMin1)/2 - (xMax2 - xMin2)/2
        val yShift = yMax1 - yMin1
        val c2 = myc2.translate(xShift, yShift)
        overlap(c2)
    }

    //TODO: Write a function that will rotate each figure in the canvas using
    // the angle `ang` defined in radians.
    // Suggestion: first write rotation functions for polygon and circle.
    // rotating a polygon is simply rotating each vertex.
    // rotating a circle is simply rotating the center with radius unchanged.
    def rotate(angRad: Double): MyCanvas = {
        val newL = listOfObjects.map(x => x.rotate(angRad))
        new MyCanvas(newL)
    }

    // Function to draw the canvas. Do not edit.
    def render(g: Graphics2D, xMax: Double, yMax: Double) = {
        val (lx1, ux1, ly1, uy1) = this.getBoundingBox
        val shiftx = -lx1
        val shifty = -uy1
        val scaleX = xMax/(ux1 - lx1  + 1.0)
        val scaleY = yMax/(uy1 - ly1 + 1.0)
        listOfObjects.foreach(f => f.render(g,scaleX, -scaleY, shiftx, shifty))
    }

    def overlap(c2: MyCanvas): MyCanvas = {
        new MyCanvas(listOfObjects ++ c2.listOfObjects)
    }

    // DO NOT EDIT THE CODE BELOW
    override def toString: String = {
        listOfObjects.foldLeft[String] ("") { case (acc, fig) => acc ++ fig.toString }
    }
    // DO NOT EDIT
    def getListOfObjects: List[Figure] = listOfObjects

    // DO NOT EDIT
    def numPolygons: Int =
        listOfObjects.count {
            case Polygon(_) => true
            case _ => false }

    //DO NOT EDIT
    def numCircles: Int = {
        listOfObjects.count {
            case MyCircle(_,_) => true
            case _ => false }
    }
    //DO NOT EDIT
    def numVerticesTotal: Int = {
        listOfObjects.foldLeft[Int](0) ((acc, f) =>
            f match {
                case Polygon(lst1) => acc + lst1.length
                case _ => acc
            }
        )
    }
}
