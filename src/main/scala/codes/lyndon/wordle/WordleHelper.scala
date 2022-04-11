package codes.lyndon.wordle

import better.files.File

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Random

object WordleHelper {

  def main(args: Array[String]): Unit = {
    if (args.length < 1) {
      println("arg 1: should be word list file")
      return
    }

    val words =
      mutable.ListBuffer.from(Random.shuffle(File(args(0)).lines.toSeq))
    println(s"Word list of ${words.size} loaded")

    val freq = letterFrequency(words.toSeq)

    val startWordSuggestion = suggestWord(words)
    println(s"Start word suggestion: $startWordSuggestion")

    val state = GuessState(WordValidity(), words)
    guess(state)
  }

  @tailrec
  def read5LetterWord(state: GuessState): String = {
    val validLetters = state.wordValidity.allValidLetters
    val word = get5CharacterInput(validLetters)
    if(!state.wordValidity.guesses.contains(word) && state.words.contains(word)) {
      word
    } else {
      read5LetterWord(state)
    }
  }
  def readGuessValidity(): String = get5CharacterInput(Seq('+', 'x', '~'))

  @tailrec
  def get5CharacterInput(validChars: Seq[Char]): String = {
    val input = scala.io.StdIn.readLine()
    if (input != null && input.length == 5 && input.forall(validChars.contains(_))) {
      input
    } else {
      println(s"Enter 5 valid characters ${validChars.mkString(", ")}:")
      get5CharacterInput(validChars)
    }
  }

  @tailrec
  def guess(state: GuessState): Unit = {
    println("Enter guess:")
    val wordGuess = read5LetterWord(state)
    println(
      "Enter letter validity, x means not present, + means correct, ~ means present but wrong place:"
    )
    val letterValidity = parseLetters(wordGuess, readGuessValidity())
    val newGuessState = updateState(wordGuess, letterValidity, state)

    val words = newGuessState.words
    println(s"${words.size} words remain")

    if (words.size == 1) {
      println(s"word is: ${words.head}")
    } else if (words.isEmpty) {
      println("No matching word")
    } else {
      val suggestion = suggestWord(words)
      println(s"Suggested word: $suggestion")
      println("Other possibles:")
      words.take(10).foreach(println)
      guess(newGuessState)
    }
  }

  def updateState(
      wordGuess: String,
      letterValidity: Seq[(Char, Validity)],
      current: GuessState
  ): GuessState = {

    val wordValidity = current.wordValidity
    val words = current.words

    val newGuesses: Seq[String] = wordValidity.guesses.appended(wordGuess)

    val initialUpdated =
      letterValidity.zip(wordValidity.letters).map {
        case ((c, validity), letter) =>
          validity match {
            case Valid         => LetterValidity(c)
            case Invalid       => letter.remove(c)
            case WrongPosition => letter.remove(c)
          }
      }

    // we need this to fix when guessing words with multiple of the
    // same character
    val validChars = letterValidity
      .filter { case (_, validity) => validity == Valid}
      .map(_._1)
      .toSet

    val invalidChars = letterValidity
      .filter { case (_, validity) => validity == Invalid }
      .map(_._1)
      .toSet -- validChars

    val wrongPositionChars = letterValidity
      .filter { case (_, validity) => validity == WrongPosition }
      .map(_._1)
      .toSet

    val updated = initialUpdated.map { validity =>
      validity.removeAll(invalidChars)
    }

    // remove the guess
    words -= wordGuess
    alignWords(words, updated, wrongPositionChars)
    GuessState(WordValidity(newGuesses, updated), words)
  }

  case class GuessState(
      wordValidity: WordValidity,
      words: mutable.ListBuffer[String]
  )

  def alignWords(
      words: mutable.ListBuffer[String],
      valids: Seq[WordleHelper.LetterValidity],
      wrongPositionChars: Set[Char]
  ): Unit = {

    words
      .filterInPlace { word =>
        word
          .zip(valids)
          .count { case (c, validity) =>
            validity.letters.contains(c)
          } == word.length
      }
      .filterInPlace { word =>
        wrongPositionChars.intersect(word.toSet).size == wrongPositionChars.size
      }
  }

  def parseLetters(
      word: String,
      validityString: String
  ): Seq[(Char, Validity)] = {
    validityString.zip(word).map { case (c, letter) =>
      (
        letter,
        c match {
          case 'x' => Invalid
          case '+' => Valid
          case '~' => WrongPosition
        }
      )
    }
  }

  def letterFrequency(words: Seq[String]) : Seq[(Char, Int)] = {
    words.flatMap { word => word.toCharArray}
      .groupBy(x => x)
      .view
      .mapValues(_.length)
      .toSeq
      .sortBy(_._2)
      .reverse
  }

  def getNgrams(
      words: mutable.ListBuffer[String],
      n: Int = 2,
      unique: Boolean = true
  ): Seq[(String, Int)] = {
    words
      .flatMap { word =>
        val ngrams = word.sliding(n).toSeq
        if (unique) ngrams.distinct
        else ngrams
      }
      .groupBy(x => x)
      .view
      .mapValues(_.length)
      .toSeq
      .sortBy(a => a._2)
      .reverse
  }

  def wordWithMostDifferentLetters(words: Seq[String]): Option[String] = {
    words
      .map { word => (word, word.distinct.sorted) }
      .sortBy { case (_, letters) => letters.length }
      .reverse
      .map(_._1)
      .headOption
  }

  def ngramSuggestions(
      words: mutable.ListBuffer[String],
      n: Int = 2
  ): Seq[String] = {
    val ngrams = getNgrams(words, n).take(3)
    val wordsWithTopNgram = words.filter { word =>
      word.contains(ngrams.head._1)
    }
    val suggestions = if (ngrams.size > 1) {
      val wordsWithTop2 = wordsWithTopNgram.filter { word =>
        word.contains(ngrams(1))
      }
      if (wordsWithTop2.nonEmpty) {
        if (ngrams.size > 2) {
          val wordsWithTop3 = wordsWithTop2.filter { word =>
            word.contains(ngrams(2))
          }
          if (wordsWithTop3.nonEmpty) wordsWithTop3
          else wordsWithTop2
        } else {
          wordsWithTop2
        }
      } else {
        wordsWithTopNgram
      }
    } else {
      wordsWithTopNgram
    }
    suggestions.take(10).toSeq
  }

  def suggestWord(words: mutable.ListBuffer[String], n: Int = 2): String = {
    val ngramSuggests = ngramSuggestions(words, n)
    if(ngramSuggests.nonEmpty) {
      wordWithMostDifferentLetters(ngramSuggests) match {
        case Some(value) => value
        case None => ngramSuggests.head
      }
    } else {
      wordWithMostDifferentLetters(words.toSeq).head
    }
  }

  sealed abstract class Validity
  case object Valid extends Validity
  case object Invalid extends Validity
  case object WrongPosition extends Validity

  case class LetterValidity(
      letters: Set[Char] = 'a'.to('z').toSet
  ) {
    def remove(c: Char): LetterValidity = {
      copy(letters = letters.filterNot(_ == c))
    }

    def removeAll(chars: Set[Char]): LetterValidity = {
      copy(letters = letters.removedAll(chars))
    }
  }

  object LetterValidity {
    def apply(c: Char): LetterValidity = LetterValidity(Set(c))
  }

  case class WordValidity(
      guesses: Seq[String] = Seq.empty,
      letters: Seq[LetterValidity] = Seq.fill(6)(LetterValidity())
  ) {
    def allValidLetters: Seq[Char] = {
      letters.flatMap(_.letters).distinct.sorted
    }
  }

}
