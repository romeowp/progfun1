package forcomp

import Anagrams._

object test {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(63); val res$0 = 
Nil::List();System.out.println("""res0: List[scala.collection.immutable.Nil.type] = """ + $show(res$0));$skip(713); 

  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    def occurrenceAnagrams(occurrences: Occurrences, phrases: List[Sentence]): List[Sentence] = {
      def loop(subsets: List[Occurrences]): List[Sentence] = subsets match {
        case Nil => Nil
        case subset :: rest => dictionaryByOccurrences.get(subset) match {
          case None => loop(rest)
          case Some(words) => loop(rest) ::: occurrenceAnagrams(
            subtract(occurrences, subset),
            phrases.map(phrase => words.flatMap(_ :: phrase)))
        }
      }
      if (occurrences.isEmpty) phrases
      else loop(combinations(occurrences))
    }
    occurrenceAnagrams(sentenceOccurrences(sentence), Nil)
  };System.out.println("""sentenceAnagrams: (sentence: forcomp.Anagrams.Sentence)List[forcomp.Anagrams.Sentence]""")}

}
