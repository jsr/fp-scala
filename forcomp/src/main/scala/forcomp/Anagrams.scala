package forcomp

import common._

object Anagrams {

  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /** `Occurrences` is a `List` of pairs of characters and positive integers saying
   *  how often the character appears.
   *  This list is sorted alphabetically w.r.t. to the character in each pair.
   *  All characters in the occurrence list are lowercase.
   *  
   *  Any list of pairs of lowercase characters and their frequency which is not sorted
   *  is **not** an occurrence list.
   *  
   *  Note: If the frequency of some character is zero, then that character should not be
   *  in the list.
   */
  type Occurrences = List[(Char, Int)]

  /** The dictionary is simply a sequence of words.
   *  It is predefined and obtained as a sequence using the utility method `loadDictionary`.
   */
  val dictionary: List[Word] = loadDictionary

  /** Converts the word into its character occurence list.
   *  
   *  Note: the uppercase and lowercase version of the character are treated as the
   *  same character, and are represented as a lowercase character in the occurrence list.
   */
  def wordOccurrences(w: Word): Occurrences = w.groupBy(_.toLower).mapValues(_.length).toList.sortBy(_._1)

  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences = s match {
    case head :: tail => wordOccurrences(s.reduceLeft(_ + _)).sortBy(_._1)
    case Nil => Nil
  }

  /** The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
   *  the words that have that occurrence count.
   *  This map serves as an easy way to obtain all the anagrams of a word given its occurrence list.
   *  
   *  For example, the word "eat" has the following character occurrence list:
   *
   *     `List(('a', 1), ('e', 1), ('t', 1))`
   *
   *  Incidentally, so do the words "ate" and "tea".
   *
   *  This means that the `dictionaryByOccurrences` map will contain an entry:
   *
   *    List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
   *
   */
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = {
    dictionary.map(w => (wordOccurrences(w), w)).groupBy(_._1).mapValues(words => words.map(_._2))
  }

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] = {
    dictionaryByOccurrences.find(_._1 == wordOccurrences(word)).map(_._2) getOrElse Nil
  }

  /** Returns the list of all subsets of the occurrence list.
   *  This includes the occurrence itself, i.e. `List(('k', 1), ('o', 1))`
   *  is a subset of `List(('k', 1), ('o', 1))`.
   *  It also include the empty subset `List()`.
   * 
   *  Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))` are:
   *
   *    List(
   *      List(),
   *      List(('a', 1)),
   *      List(('a', 2)),
   *      List(('b', 1)),
   *      List(('a', 1), ('b', 1)),
   *      List(('a', 2), ('b', 1)),
   *      List(('b', 2)),
   *      List(('a', 1), ('b', 2)),
   *      List(('a', 2), ('b', 2))
   *    )
   *
   *  Note that the order of the occurrence list subsets does not matter -- the subsets
   *  in the example above could have been displayed in some other order.
   */
  def combinations(occurrences: Occurrences): List[Occurrences] = {
    def decompose(elt: (Char, Int)): Occurrences = elt match {
      case (char, count) =>
        (for {
          index <- 1 to count
        } yield char -> index).toList
    }

    def comb(elt: List[Occurrences], rest: List[Occurrences], deep: Int): List[Occurrences] = {
      (elt, rest) match {
        case (head :: tail, _) if(head.length == deep) => elt
        case (acc, head :: tail) => {
          val res = for {
            x <- head
            y <- elt
          } yield (y :+ x)
          comb(res, tail, deep)
        }
        case (_, Nil) => elt
      }
    }

    def loop(occ: Occurrences, acc: List[Occurrences]): List[Occurrences] = occ match {
      case Nil => acc
      case head :: Nil => acc ++ decompose(head).map(e => List(e))
      case head :: tail => {
        val res = (for {
            deep <- 1 to occurrences.length
            cb <- decompose(head)
          } yield comb(List(List(cb)), tail.map(decompose), deep)
        ).reduceLeft(_ ++ _).toList
        loop(tail, acc ++ res)
      }
    }
    loop(occurrences, Nil) ++ List(Nil)
  }

  /** Subtracts occurrence list `y` from occurrence list `x`.
   * 
   *  The precondition is that the occurrence list `y` is a subset of
   *  the occurrence list `x` -- any character appearing in `y` must
   *  appear in `x`, and its frequency in `y` must be smaller or equal
   *  than its frequency in `x`.
   *
   *  Note: the resulting value is an occurrence - meaning it is sorted
   *  and has no zero-entries.
   */
  def subtract(x: Occurrences, y: Occurrences): Occurrences = {
    def loop(acc: Map[Char, Int], elts: Occurrences): Occurrences = elts match {
      case (char, c1) :: tail => acc.get(char).map { c2 =>
        if(c2 - c1 <= 0) loop(acc - char, tail) else loop(acc updated (char, c2 - c1), tail)
      } getOrElse loop(acc, tail)
      case Nil => acc.toList.sortBy(_._1)
    }
    loop(x.toMap, y)
  }

  /** Returns a list of all anagram sentences of the given sentence.
   *  
   *  An anagram of a sentence is formed by taking the occurrences of all the characters of
   *  all the words in the sentence, and producing all possible combinations of words with those characters,
   *  such that the words have to be from the dictionary.
   *
   *  The number of words in the sentence and its anagrams does not have to correspond.
   *  For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
   *
   *  Also, two sentences with the same words but in a different order are considered two different anagrams.
   *  For example, sentences `List("You", "olive")` and `List("olive", "you")` are different anagrams of
   *  `List("I", "love", "you")`.
   *  
   *  Here is a full example of a sentence `List("Yes", "man")` and its anagrams for our dictionary:
   *
   *    List(
   *      List(en, as, my),
   *      List(en, my, as),
   *      List(man, yes),
   *      List(men, say),
   *      List(as, en, my),
   *      List(as, my, en),
   *      List(sane, my),
   *      List(Sean, my),
   *      List(my, en, as),
   *      List(my, as, en),
   *      List(my, sane),
   *      List(my, Sean),
   *      List(say, men),
   *      List(yes, man)
   *    )
   *
   *  The different sentences do not have to be output in the order shown above - any order is fine as long as
   *  all the anagrams are there. Every returned word has to exist in the dictionary.
   *  
   *  Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
   *  so it has to be returned in this list.
   *
   *  Note: There is only one anagram of an empty sentence.
   */

  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {

    def isRex(occ: Occurrences) = occ match {
      case List(('e',1), ('r',2), ('x',1)) => true
      case _ => false
    }

    def findSentences(all: Occurrences, occurrences: Occurrences, sentence: Sentence): List[Sentence] = {
      combinations (occurrences) flatMap { occ =>
        if(isRex(occ)) println("[OCCS] - " + occurrences)
        if(isRex(occ)) println("[COMBINAISON] - " + combinations(occurrences))
        if(isRex(occ)) println("----------------------------")
        val sortedOcc = occ
        val debug = false
        if(isRex(occ)) println("[ALL] - " + all)
        if(isRex(occ)) println("[OCC] - " + occ)
        if(isRex(occ)) println("[SEN] - " + sentence)
        val words = dictionaryByOccurrences.get(sortedOcc) getOrElse Nil
        (words, sortedOcc, all) match {
          case (_, Nil, _) => List(sentence)
          case (Nil, occ, all) if(occ.length > 0 || all.length > 0) => Nil
          case (words, occ, _) =>
            (for {
              word <- words
              if(!sentence.contains(word))
              s <- findSentences(subtract(all, occ), subtract(occurrences, occ), sentence :+ word)
            } yield s)
        }
      }
    }

    val all = sentenceOccurrences(sentence)
    def loop(occurrences: List[Occurrences], acc: List[Sentence]): List[Sentence] = {
      occurrences match {
        case Nil => acc
        case head :: tail => {
          println("=========================================")
          println("[HEAD] - " + head)
          val sentence = findSentences(all, head, Nil).filter(s => !s.isEmpty)
          println("[RESULT] - " + sentence)
          println("=========================================")
          loop(tail, acc ++ sentence)
        }
      }
    }
    println("#############################")
    println(all)
    println(combinations(all))
    println("#############################")
    loop(combinations(all), Nil).distinct ++ List(Nil)
  }
}


List((e,1), (i,1), (l,2), (n,1), (r,1), (u,2), (x,1), (z,1))

List(
  List((e,1)),
  List((e,1), (i,1)),
  List((e,1), (i,1), (l,1)),
  List((e,1), (i,1), (l,2)),
  List((e,1), (i,1), (l,1), (n,1)),
  List((e,1), (i,1), (l,2), (n,1)),
  List((e,1), (i,1), (l,1), (n,1),(r,1)),
  List((e,1), (i,1), (l,2), (n,1), (r,1)),
  List((e,1), (i,1), (l,1), (n,1), (r,1), (u,1)),
  List((e,1), (i,1), (l,2), (n,1), (r,1), (u,1)),
  List((e,1), (i,1), (l,1), (n,1), (r,1), (u,2)),
  List((e,1), (i,1), (l,2), (n,1), (r,1), (u,2)),
  List((e,1), (i,1), (l,1), (n,1), (r,1), (u,1),(x,1)),
  List((e,1), (i,1), (l,2), (n,1), (r,1), (u,1), (x,1)),
  List((e,1), (i,1), (l,1), (n,1), (r,1), (u,2), (x,1)),
  List((e,1), (i,1), (l,2), (n,1), (r,1), (u,2), (x,1)),
  List((e,1), (i,1), (l,1), (n,1), (r,1), (u,1), (x,1), (z,1)),
  List((e,1), (i,1), (l,2), (n,1), (r,1), (u,1), (x,1), (z,1)),
  List((e,1), (i,1), (l,1), (n,1), (r,1), (u,2), (x,1), (z,1)),
  List((e,1), (i,1), (l,2), (n,1), (r,1), (u,2), (x,1), (z,1)),
  List((i,1)), List((i,1), (l,1)), List((i,1), (l,2)), List((i,1), (l,1), (n,1)), List((i,1), (l,2), (n,1)), List((i,1), (l,1), (n,1), (r,1)), List((i,1), (l,2), (n,1), (r,1)), List((i,1), (l,1), (n,1), (r,1), (u,1)), List((i,1), (l,2), (n,1), (r,1), (u,1)), List((i,1), (l,1), (n,1), (r,1), (u,2)), List((i,1), (l,2), (n,1), (r,1), (u,2)), List((i,1), (l,1), (n,1), (r,1), (u,1), (x,1)), List((i,1), (l,2), (n,1), (r,1), (u,1), (x,1)), List((i,1), (l,1), (n,1), (r,1), (u,2), (x,1)), List((i,1), (l,2), (n,1), (r,1), (u,2), (x,1)), List((i,1), (l,1), (n,1), (r,1), (u,1), (x,1), (z,1)), List((i,1), (l,2), (n,1), (r,1), (u,1), (x,1), (z,1)), List((i,1), (l,1), (n,1), (r,1), (u,2), (x,1), (z,1)), List((i,1), (l,2), (n,1), (r,1), (u,2), (x,1), (z,1)), List((i,1), (l,1), (n,1), (r,1), (u,1), (x,1), (z,1)), List((i,1), (l,2), (n,1), (r,1), (u,1), (x,1), (z,1)), List((i,1), (l,1), (n,1), (r,1), (u,2), (x,1), (z,1)), List((i,1), (l,2), (n,1), (r,1), (u,2), (x,1), (z,1)), List((l,1)), List((l,2)), List((l,1), (n,1)), List((l,2), (n,1)), List((l,1), (n,1), (r,1)), List((l,2), (n,1), (r,1)), List((l,1), (n,1), (r,1), (u,1)), List((l,1), (n,1), (r,1), (u,2)), List((l,2), (n,1), (r,1), (u,1)), List((l,2), (n,1), (r,1), (u,2)), List((l,1), (n,1), (r,1), (u,1), (x,1)), List((l,1), (n,1), (r,1), (u,2), (x,1)), List((l,2), (n,1), (r,1), (u,1), (x,1)), List((l,2), (n,1), (r,1), (u,2), (x,1)), List((l,1), (n,1), (r,1), (u,1), (x,1), (z,1)), List((l,1), (n,1), (r,1), (u,2), (x,1), (z,1)), List((l,2), (n,1), (r,1), (u,1), (x,1), (z,1)), List((l,2), (n,1), (r,1), (u,2), (x,1), (z,1)), List((l,1), (n,1), (r,1), (u,1), (x,1), (z,1)), List((l,1), (n,1), (r,1), (u,2), (x,1), (z,1)), List((l,2), (n,1), (r,1), (u,1), (x,1), (z,1)), List((l,2), (n,1), (r,1), (u,2), (x,1), (z,1)), List((l,1), (n,1), (r,1), (u,1), (x,1), (z,1)), List((l,1), (n,1), (r,1), (u,2), (x,1), (z,1)), List((l,2), (n,1), (r,1), (u,1), (x,1), (z,1)), List((l,2), (n,1), (r,1), (u,2), (x,1), (z,1)), List((n,1)), List((n,1), (r,1)), List((n,1), (r,1), (u,1)), List((n,1), (r,1), (u,2)), List((n,1), (r,1), (u,1), (x,1)), List((n,1), (r,1), (u,2), (x,1)), List((n,1), (r,1), (u,1), (x,1), (z,1)), List((n,1), (r,1), (u,2), (x,1), (z,1)), List((n,1), (r,1), (u,1), (x,1), (z,1)), List((n,1), (r,1), (u,2), (x,1), (z,1)), List((n,1), (r,1), (u,1), (x,1), (z,1)), List((n,1), (r,1), (u,2), (x,1), (z,1)), List((n,1), (r,1), (u,1), (x,1), (z,1)), List((n,1), (r,1), (u,2), (x,1), (z,1)), List((r,1)), List((r,1), (u,1)), List((r,1), (u,2)), List((r,1), (u,1), (x,1)), List((r,1), (u,2), (x,1)), List((r,1), (u,1), (x,1), (z,1)), List((r,1), (u,2), (x,1), (z,1)), List((r,1), (u,1), (x,1), (z,1)), List((r,1), (u,2), (x,1), (z,1)), List((r,1), (u,1), (x,1), (z,1)), List((r,1), (u,2), (x,1), (z,1)), List((r,1), (u,1), (x,1), (z,1)), List((r,1), (u,2), (x,1), (z,1)), List((r,1), (u,1), (x,1), (z,1)), List((r,1), (u,2), (x,1), (z,1)), List((u,1)), List((u,2)), List((u,1), (x,1)), List((u,2), (x,1)), List((u,1), (x,1), (z,1)), List((u,2), (x,1), (z,1)), List((u,1), (x,1), (z,1)), List((u,2), (x,1), (z,1)), List((u,1), (x,1), (z,1)), List((u,2), (x,1), (z,1)), List((u,1), (x,1), (z,1)), List((u,2), (x,1), (z,1)), List((u,1), (x,1), (z,1)), List((u,2), (x,1), (z,1)), List((u,1), (x,1), (z,1)), List((u,2), (x,1), (z,1)), List((x,1)), List((x,1), (z,1)), List((x,1), (z,1)), List((x,1), (z,1)), List((x,1), (z,1)), List((x,1), (z,1)), List((x,1), (z,1)), List((x,1), (z,1)), List((z,1)), List())


List(
  List((a,1)),
  List((a,2)),
  List((a,1), (b,1)),
  List((a,1), (b,2)),
  List((a,2), (b,1)),
  List((a,2), (b,2)),
  List((a,1), (b,1), (c,1)),
  List((a,1), (b,2), (c,1)),
  List((a,1), (b,1), (c,2)),
  List((a,1), (b,2), (c,2)),
  List((a,2), (b,1), (c,1)),
  List((a,2), (b,2), (c,1)),
  List((a,2), (b,1), (c,2)),
  List((a,2), (b,2), (c,2)),
  List((b,1)),
  List((b,2)),
  List((b,1), (c,1)),
  List((b,1), (c,2)),
  List((b,2), (c,1)),
  List((b,2), (c,2)),
  List((b,1), (c,1)),
  List((b,1), (c,2)), List((b,2), (c,1)), List((b,2), (c,2)), List((c,1)), List((c,2)), List())
