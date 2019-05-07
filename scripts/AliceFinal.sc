/* Utilities */

def showMe(v:Any):Unit = {
	v match {
		case _:Iterable[Any] => println(s"""----\n${v.asInstanceOf[Iterable[Any]].mkString("\n")}\n----""")
		case _:Vector[Any] => println(s"""----\n${v.asInstanceOf[Vector[Any]].mkString("\n")}\n----""")
		case _ => println(s"-----\n${v}\n----")
	}
}

def loadLibrary(fp:String):CiteLibrary = {
	val library = CiteLibrary(Source.fromFile(fp).getLines.mkString("\n"),"#",",")
	library
}

def loadFile(fp:String):Vector[String] = {
	Source.fromFile(fp).getLines.toVector
}

def saveString(s:String, filePath:String = "", fileName:String = ""):Unit = {
	val pw = new PrintWriter(new File(filePath + fileName))
	for (line <- s.lines){
		pw.append(line)
		pw.append("\n")
	}
	pw.close
}

val splitters:String = """[\[\])(:·⸁.,·;;   "?·!–—⸂⸃]"""
val sentencePunc:String = """[.;?!]"""

//read in the word list as a Dictionary; each word is the key
val wordFile:String = "WordNet Words.CAT"
val aliceFile:String = "alice.cex"
val act:Vector[String] = fromFile(wordFile).filter(_.text.startsWith(" NOUN.ACT").endsWith(" NOUN.ANIMAL"))
val animal:Vector[String]
val artifact:Vector[String]
val attribute:Vector[String]
val body:Vector[String]
val cognition:Vector[String]
val communication:Vector[String]
val event:Vector[String]
val feeling:Vector[String]
val food:Vector[String]
val group:Vector[String]
val location:Vector[String]
val motive:Vector[String]
val object:Vector[String]
val person:Vector[String]
val phenomenon:Vector[String]
val plant:Vector[String]
val possesion:Vector[String]
val process:Vector[String]
val quantity:Vector[String]
val relation:Vector[String]
val shape:Vector[String]
val state:Vector[String]
val substance:Vector[String]
val time:Vector[String]


//iterator for counting each kind of word

def Counter(word:String, )



//make word histogram

val myCexFile:String = "alice.cex"

lazy val lib = loadLibrary(myCexFile)
lazy val tr = lib.textRepository.get
lazy val aliceCorpus = tr.corpus

case class WordHisto(word:String, count:Int)

val wordHisto:Vector[WordHisto] = {
	aliceCorpus.nodes
		.map(_.text)
		.mkString(" ")
		.split(splitters).toVector
		.filter(_.size > 0)
		.filter( w => stopWords.contains(w.toLowerCase) == false )
		.groupBy(w => w).toVector
		.map( tup => WordHisto(tup._1, tup._2.size))
		.sortBy(_.count)
		.reverse
}

def whString(i:Int):String = {
	wordHisto.take(i).map(wh => {
		s"${wh.count}  ${wh.word}"
	}).mkString("\n")

}
