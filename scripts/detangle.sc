import scala.io.Source
import edu.holycross.shot.cite._
import java.io._

case class IndexedLine(text:String, index:Int)
case class ChapterHeading(title:String, index:Int)
case class BookPara(chapterName:String, text:String, index:Int)


val cexHeader:String = """#!cexversion
3.0

#!citelibrary
name#SOME_NAME
urn#urn:cite2:cex:fufolio.2019a:carroll
license#CC Share Alike.  For details, see more info.


#!ctscatalog
urn#citationScheme#groupName#workTitle#versionLabel#exemplarLabel#online#lang
urn:cts:fuTexts:carroll.alice.fu2019:#chapter/paragraph#AUTHOR#WORK_NAME#F.U. ed##true#hun

#!ctsdata
"""

def saveString(s:String, filePath:String = "", fileName:String = "temp.txt"):Unit = {
  val pw = new PrintWriter(new File(filePath + fileName))
  for (line <- s.lines){
    pw.append(line)
    pw.append("\n")
  }
  pw.close
}


val filepath:String = "alice.txt"
val myLines:Vector[String] = Source.fromFile(filepath).getLines.toVector.filter( _.size > 0 )

// Grab line numbers

val indexedFileLines:Vector[IndexedLine] = myLines.zipWithIndex.map( ln => {
  new IndexedLine(ln._1, ln._2)
})

// Filter out chapter headings
val chapters:Vector[ChapterHeading] = {
  indexedFileLines.filter(_.text.startsWith("CHAPTER")).map( c => {
    println(s"\n${c}")
    val index:Int = c.index
    val newTitle:String = c.text.replaceAll("CHAPTER ","").replaceAll("\\.","")
    new ChapterHeading(newTitle, index)
  })
}

val realParagraphs:Vector[IndexedLine] = {
  indexedFileLines.filter( _.text.startsWith("CHAPTER") == false )
}
// find where each chapter begins and ends!
val chapterRanges:Vector[Vector[ChapterHeading]] = chapters.sliding(2,1).toVector

val allButTheLastChapter:Vector[BookPara]= chapterRanges.map( cr => {
    val thisChapt:ChapterHeading = cr.head
    val thisChaptLineNum:Int = thisChapt.index
    val nextChapt:ChapterHeading = cr.last
    val nextChaptLineNum:Int = nextChapt.index
    val chapterParas:Vector[IndexedLine] = {
      realParagraphs.filter( il => {
        (( il.index > thisChaptLineNum) & ( il.index < nextChaptLineNum))
      })
    }

    // attach the chapter title to the paragraph
   val bookParas:Vector[BookPara] = chapterParas.zipWithIndex.map (cp => {
      val thisIndex:Int = cp._2 + 1
      new BookPara( thisChapt.title, cp._1.text, thisIndex)
    })
    bookParas
}).flatten

val theLastChapter:Vector[BookPara] = {

  val lastChaptHeading:String = chapterRanges.last.last.title
  val lastChaptLineNum:Int = chapterRanges.last.last.index

  val chapterParas:Vector[IndexedLine] = {
    realParagraphs.filter( il => {
      ( il.index > lastChaptLineNum)
    })
  }
  val bookParas:Vector[BookPara] = chapterParas.map( cp => {
    new BookPara( lastChaptHeading, cp.text, cp.index)
  })

  bookParas
}

val betterTLC:Vector[BookPara] = theLastChapter.zipWithIndex.map( a => {
  // each "a" is a (BookPara, Int)
  val thisIndex:Int = a._2 + 1
  val oldPara:BookPara = a._1
  val oldChap:String = oldPara.chapterName
  val oldText:String = oldPara.text
  BookPara(oldChap, oldText, thisIndex)
})

val allChapterLines:Vector[BookPara] = {
  allButTheLastChapter ++ betterTLC
}

/* map that to what you want for CEX */

val savableLines:Vector[String] = {
  allChapterLines.map( cl => {
    val aliceLine:String = {
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
  "urn:cts:fuTexts:carroll.alice.fu2019:" + cl.chapterName + "." + cl.index + "#" + aliceLine
  // or
  //s"${cl.chapterName}.${cl.index}#${cl.text}
})
}

val stringToSave:String = cexHeader + savableLines.mkString("\n")

saveString(stringToSave)
