package worksheets

import Anagrams._

object test {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(103); 
  val w: String = "Welcome to the Scala worksheet";System.out.println("""w  : String = """ + $show(w ));$skip(49); 
  val chars: List[Char] = w.toLowerCase().toList;System.out.println("""chars  : List[Char] = """ + $show(chars ));$skip(40); 
  val grouped = chars.groupBy(identity);System.out.println("""grouped  : scala.collection.immutable.Map[Char,List[Char]] = """ + $show(grouped ));$skip(47); val res$0 = 
  grouped.map(x => (x._1, x._2.length)).toList;System.out.println("""res0: List[(Char, Int)] = """ + $show(res$0));$skip(63); val res$1 = 
  chars.groupBy(identity).map(e => (e._1, e._2.length)).toList;System.out.println("""res1: List[(Char, Int)] = """ + $show(res$1));$skip(40); val res$2 = 
  
  List("qaz", "wsx", "edc").toString;System.out.println("""res2: String = """ + $show(res$2))}


}
