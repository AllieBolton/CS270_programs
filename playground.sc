import scala.io.Source
import edu.holycross.shot.cite._


// EDIT THIS to point to your file!!!
val filepath:String = "/vagrant/YOUR_REPO/YOUR_TEXT.txt"
// Get the file as a vector of lines, ignoring empty lines
val myLines:Vector[String] = Source.fromFile(filepath).getLines.toVector.filter( _.size > 0 )


/* How to (a) remove punctuation, and (b) tokenize by word in a short chunk */
val myTokenizedLines:Vector[Vector[String]] = myLines.map( aLine => {
  val noPunc:String = aLine.replaceAll("""[,.?;":!)(]""",""  ).replaceAll(" +"," ")
  val wordVec:Vector[String] = noPunc.split(" ").toVector
  wordVec
})

/* A cool thing you can do */
val myVec:Vector[Int] = Vector(1,2,3,4,5,6,7,8,9)
val myGroups:Vector[Vector[Int]] = myVec.sliding(3,1).toVector
// Type `myGroups` into the Console to see the result!
// What does the '3' do? What does the '1' do?
// See: http://daily-scala.blogspot.com/2009/11/iteratorsliding.html

/* How to "flatten" a list of lists into just a list */
val v:Vector[Int] = Vector(1,2,3,4,5)
val vv:Vector[Vector[Int]] = Vector(Vector(1,2,3), Vector(4,5))
val didItFlatten:Boolean = v == vv.flatten

/* A thing you can do */
val newVec:Vector[Int] = Vector(1,2,3,4,1,2,3,1,2,3,2,3,4,4,1,2)
val mySlided:Vector[Vector[Int]] = newVec.sliding(3,1).toVector
val grouped:Map[Vector[Int],Vector[Vector[Int]]] = mySlided.groupBy(i => i)
val madeIntoAVectorBecauseMapsAreHard:Vector[(Vector[Int], Vector[Vector[Int]])] = grouped.toVector
// rename this to make it shorter ;-)
val madeVec = madeIntoAVectorBecauseMapsAreHard

// Stop to inspect! You can paste these into the console after the script runs:
madeVec(0)
madeVec(0)._1
madeVec(0)._2

// Change one part into a String
madeVec(0)._1.mkString(" ")
// print it adding quotation marks so you know it is a string
println(s""" "${madeVec(0)._1.mkString(" ")}" """)

// Change another part into a count
madeVec(0)._1.size

// Try this with other individual items by changing (0) to other numbers
madeVec(1)._1.mkString(" ")
madeVec(1)._1.size

// Do that to all items in madeVec

val ng:Vector[(String, Int)] = madeVec.map(mv => {
    val s:String = mv._1.mkString(" ")
    val i:Int = mv._2.size
    (s,i)
})

// See the results
for (n <- ng) {
  println(s""" "${n._1}" occurs ${n._2}""")
}
