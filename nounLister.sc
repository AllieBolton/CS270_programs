import scala.io.Source
import edu.holycross.shot.cite._
import java.io._

case class IndexedLine(text:String, index:Int)
case class SectionHeading(title:String, index:Int)
case class Noun(sectionName:String, text:String, index:Int)


def saveString(s:String, filePath:String = "", fileName:String = "temp.txt"):Unit = {
  val pw = new PrintWriter(new File(filePath + fileName))
  for (line <- s.lines){
    pw.append(line)
    pw.append("\n")
  }
  pw.close
}


val filepath:String = "WordNet Words.CAT"
val myLines:Vector[String] = Source.fromFile(filepath).getLines.toVector.filter( _.size > 0 )

// Grab line numbers

val indexedFileLines:Vector[IndexedLine] = myLines.zipWithIndex.map( ln => {
  new IndexedLine(ln._1, ln._2)
})

// Filter out headings
val chapters:Vector[SectionHeading] = {
  indexedFileLines.filter(_.text.startsWith(" NOUN.")).map( c => {
    println(s"\n${c}")
    val index:Int = c.index
    val newTitle:String = c.text.replaceAll(" NOUN. ","").replaceAll("\\.","")
    new SectionHeading(newTitle, index)
  })
}

val realNouns:Vector[IndexedLine] = {
  indexedFileLines.filter( _.text.startsWith(" NOUN.") == false )
}
// find where each section begins and ends!
val SectionRanges:Vector[Vector[SectionHeading]] = sections.sliding(2,1).toVector

val allButTheLastSection:Vector[Noun]= sectionRanges.map( cr => {
    val thisSect:SectionHeading = cr.head
    val thisSectLineNum:Int = thisSect.index
    val nextSect:SectionHeading = cr.last
    val nextSectLineNum:Int = nextSect.index
    val sectionNouns:Vector[IndexedLine] = {
      realNouns.filter( il => {
        (( il.index > thisSectLineNum) & ( il.index < nextSectLineNum))
      })
    }

    // attach the chapter title to the paragraph
   val Sections:Vector[Noun] = nouns.zipWithIndex.map (cp => {
      val thisIndex:Int = cp._2 + 1
      new Noun( thisSect.title, cp._1.text, thisIndex)
    })
    Nouns
}).flatten

val theLastSection:Vector[Noun] = {

  val lastSectHeading:String = sectionRanges.last.last.title
  val lastSectLineNum:Int = sectionRanges.last.last.index

  val nouns:Vector[IndexedLine] = {
    realNouns.filter( il => {
      ( il.index > lastSectLineNum)
    })
  }
  val Sections:Vector[Noun] = nouns.map( cp => {
    new Section( lastSectHeading, cp.text, cp.index)
  })

  Sections
}

val betterTLC:Vector[Section] = theLastSection.zipWithIndex.map( a => {
  // each "a" is a (Section, Int)
  val thisIndex:Int = a._2 + 1
  val oldNoun:Noun = a._1
  val oldSect:String = oldNoun.sectionName
  val oldText:String = oldNoun.text
  Noun(oldSect, oldText, thisIndex)
})

val allSectionLines:Vector[Noun] = {
  allButTheLastSection ++ betterTLC
}

/* map that to what you want for CEX */

val savableLines:Vector[String] = {
  allSectionLines.map( cl => {
    val nounLine:String = {
     val initSpaces = "^ +".r
     val foundSpaces:Vector[String] = initSpaces.findAllIn(cl.text).toVector
     if (foundSpaces.size < 1) {
      cl.text
    } else {
      val nbSpaces:String = {
        println(s"foundSpaces.size = ${foundSpaces.size}")
        val initString:String = foundSpaces.toVector.head
        val charVec:Vector[Char] = initString.toVector
        val strVec:Vector[String] = charVec.map(_.toString)
        val nbSpaces:Vector[String] = strVec.map(s => "\u00A0\u00A0")
        nbSpaces.mkString
      }
      cl.text.replaceAll("^ +",nbSpaces)
    }
  }
  "urn:cts:fuTexts:carroll.alice.fu2019:" + cl.sectionName + "." + cl.index + "#" + aliceLine
  // or
  //s"${cl.sectionName}.${cl.index}#${cl.text}
})
}

val stringToSave:String = cexHeader + savableLines.mkString("\n")

saveString(stringToSave)
