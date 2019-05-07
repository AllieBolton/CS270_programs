import scala.io.Source
import edu.holycross.shot.cite._

val filepath:String = "/vagrant/CS270_programs/Alice's_adventures_in_wonderland.txt"
lazy val myText:Vector[String] = Source.fromFile(filepath).getLines.toVector
