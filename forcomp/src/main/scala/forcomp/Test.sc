import forcomp._
type Word = String
type Sentence = List[Word]
type Occurrences = List[(Char, Int)]

val dictionaryPath = List("resources","forcomp", "linuxwords.txt")

val dictionary: List[Word] = loadDictionary

def countOccurencesOfChar(word:String)(c:Char):Int = {
  def acc(count:Int,c: Char,index:Int):Int = {
    if (index == word.length-1) count
    else if  (word.charAt(index).toLower == c.toLower) acc(count+1,c,index+1)
    else acc(count,c,index+1)
  }
  acc(0,c,0)
}

type Occurence = (Char,Int)

def sortByCharCount(occurence1: Occurence,occurence2: Occurence):Boolean = {
  if (occurence1._2 < occurence2._2) true
  else if (occurence1._2 < occurence2._2) false
  else if (occurence1._1 <= occurence2._1) true
  else false
}
def wordOccurrences(w: Word): Occurrences = {
  w.toLowerCase.groupBy(x => x).map((args) => (args._1,args._2.length)).toList.sortWith(sortByCharCount)
}

def sentenceOccurrences(s: Sentence): Occurrences = wordOccurrences(s.flatten.mkString)

lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = {
  dictionary.groupBy(wordOccurrences)
}

def wordAnagrams(word: Word): List[Word] = {
  dictionary.filter((args)=> wordOccurrences(args)==wordOccurrences(word))
}

def getCombinations(occurence: Occurence, occurrences: Occurrences):Occurrences = {
  if (occurence._2 == 0) occurrences
  else (occurence._1,occurence._2)::occurrences
}

def combinations(occs: Occurrences): List[Occurrences] =  {
  val res = occs.map(x => (for(i<-1 until x._2+1) yield (x._1,i)).toList)
  res.foldRight(List[Occurrences](Nil))((x,y) =>  y ++ (for(i <- x;j <- y) yield i::j ))
}

def subtract(x: Occurrences, y: Occurrences): Occurrences = {

  val (s1,s2) = x.partition(occ => y.exists(yocc => yocc._1 ==  occ._1) )
  val common = for ((a,b) <- s1.zip(y) if a._2 != b._2) yield (a._1, a._2 - b._2 )
  val res = (s2 ++ common).sortWith(sortByCharCount)
  res
}

def sentenceAnagrams(sentence: Sentence): List[Sentence] = {

  def getListofSentences(occurrences: Occurrences): List[Sentence] = occurrences match {

    case (List()) => List(Nil)
    case (a::b) => {
      val temp = combinations(occurrences)
      for ( i <- temp if dictionaryByOccurrences.keySet(i);
            j <- dictionaryByOccurrences(i);
            next <- getListofSentences(subtract(occurrences,i))) yield j::next
    }
  }

  getListofSentences(sentenceOccurrences(sentence))
}

val sentence = List("Linux", "rulez")
val res = sentenceAnagrams(sentence)