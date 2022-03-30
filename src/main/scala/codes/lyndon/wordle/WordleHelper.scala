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
    val state = WordValidity()
    guess(words, state)
  }

  @tailrec
  def guess(words: mutable.ListBuffer[String], state: WordValidity): Unit = {
    println("Enter guess:")
    val wordGuess = scala.io.StdIn.readLine()
    println(
      "Enter letter validity, x means not present, + means correct, ~ means present but wrong place:"
    )
    val letterValidity = parseLetters(wordGuess, scala.io.StdIn.readLine())

    val newGuesses: Seq[String] = state.guesses.appended(wordGuess)

    val initialUpdated =
      letterValidity.zip(state.letters).map { case ((c, validity), letter) =>
        validity match {
          case Valid         => LetterValidity(c)
          case Invalid       => letter.remove(c)
          case WrongPosition => letter.remove(c)
        }
      }

    val invalidChars = letterValidity
      .filter { case (_, validity) => validity == Invalid }
      .map(_._1)
      .toSet

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

    println(s"${words.size} words remain")
    words.take(10).foreach(println)

    if (words.size == 1) {
      println(s"word is: ${words.head}")
    } else if (words.isEmpty) {
      println("No matching word")
    } else {
      guess(words, WordValidity(newGuesses, updated))
    }
  }

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
  )

}
