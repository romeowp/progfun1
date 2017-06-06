package forcomp

import Anagrams._

object test {
Nil::List()                                       //> res0: List[scala.collection.immutable.Nil.type] = List(List())

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
  }                                               //> sentenceAnagrams: (sentence: forcomp.Anagrams.Sentence)List[forcomp.Anagrams
                                                  //| .Sentence]

}