
import scala.io.Source
import java.io._
import scala.collection.mutable.LinkedHashMap
import edu.holycross.shot.scm._
import edu.holycross.shot.cite._
import edu.holycross.shot.citeobj._
import edu.holycross.shot.ohco2._
import edu.holycross.shot.seqcomp._
import edu.furman.classics.citealign._
import java.util.Calendar

/* Utilities */

def loadLibrary(fp:String):CiteLibrary = {
	val library = CiteLibrary(Source.fromFile(fp).getLines.mkString("\n"),"#",",")
	library
}

def loadFile(fp:String):Vector[String] = {
	Source.fromFile(fp).getLines.toVector
}

def saveString(s:String, filePath:String = "html/", fileName:String = "alice.txt"):Unit = {
	val pw = new PrintWriter(new File(filePath + fileName))
	for (line <- s.lines){
		pw.append(line)
		pw.append("\n")
	}
	pw.close
}

/* Project-specific CEX Stuff */

val myCexFile:String = "alice.cex"

lazy val lib = loadLibrary(myCexFile)
lazy val tr = lib.textRepository.get
lazy val popeCorpus = tr.corpus

// Avoid typing lengthy URNs all the time
def u(passage:String):CtsUrn = {
	val baseUrl:String = "urn:cts:fufolio:carroll.alice.fu2019:"
	CtsUrn(s"${baseUrl}${passage}")
}

// Quick access to the ID of a poetic book
def whichChapter(u:CtsUrn):String = {
	if (u.passageComponent.size > 0) {
		u.collapsePassageTo(1).passageComponent
	} else {
		"1–24"
	}
}

// Getting labels for a URN
//     note that these depend on the stuff defined above
val groupName:String = tr.catalog.groupName(u(""))
val workTitle:String = tr.catalog.workTitle(u(""))
val versionLabel:String = tr.catalog.versionLabel(u(""))

// Chunked by 25
def chunk(corp:Corpus):Vector[Corpus] = {
	// make Vectors with whole chapters in each
	val nodeChunks:Vector[Vector[CitableNode]] = corp.nodes.sliding(25,25).toVector
	// turn each of those into a Corpus
	val corpChunks:Vector[Corpus] = {
		nodeChunks.map( nodeVec => Corpus(nodeVec) )
	}
	// return that as a value
	corpChunks
}

// Write out 25 lines to a page

var htmlTop:String = s"""<!DOCTYPE html>
<html>
<head>
	<title>${groupName}: ${workTitle}</title>
	<meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
	<link rel="stylesheet" type="text/css" href="style.css">
</head>

<body>"""

var htmlBottom:String = """</body></html>"""

val chapterChunks:Vector[Corpus] = chunk(popeCorpus)

for ( ch <- chapterChunks.zipWithIndex) {
	val chNum:Int = ch._2 + 1
	val c:Corpus = ch._1

	// create a filename
	val htmlName:String = s"chapter${bkNum}.html"


	// create a container with all the CitableNodes for this chunk
	val containerOpen:String = """<div class="text">"""
	val containerClose:String = """</div>"""

	val passages:Vector[String] = c.nodes.map( n => {
		s"""<p class="poetryLine"><span class="cite">${n.urn}</span>${n.text}</p>"""
	})

	// save this chunk as an html file
	val htmlString:String = {
		htmlTop +
		containerOpen +
		passages.mkString("\n") +
		containerClose +
		htmlBottom
	}
	// Write out to a file
	saveString(htmlString, "html/", htmlName)
}
