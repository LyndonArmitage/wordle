package codes.lyndon.wordle

import better.files.File

import scala.annotation.tailrec
import scala.util.Random

object Wordle {

  case class GameState(
      word: Word,
      guesses: Int,
      maxGuesses: Int,
      state: State
  )

  object GameState {
    def apply(word: String, maxGuesses: Int = 6): GameState =
      GameState(Word(word), 0, maxGuesses, Playing)
  }

  sealed abstract class State
  case object Playing extends State
  case object Win extends State
  case object Lose extends State

  sealed abstract class LetterGuess
  case object Correct extends LetterGuess
  case object Incorrect extends LetterGuess
  case object Present extends LetterGuess


  case class Word(
      letters: Seq[Char]
  ) {
    def word: String = letters.mkString

    def matches(guess: String): Seq[(Char, LetterGuess)] = {
      guess.zipWithIndex.map { case (c, i) =>
        val result: LetterGuess = if (letters(i) == c) {
          Correct
        } else if (letters.contains(c)) {
          Present
        } else {
          Incorrect
        }
        (c, result)
      }
    }

  }

  object Word {
    def apply(word: String): Word = {
      Word(word.toCharArray)
    }
  }

  def randomWord(file: File): String = {
    val lineCount = file.lineCount.toInt
    val randomNum = Random.nextInt(lineCount)
    file.lineIterator.toSeq(randomNum)
  }

  def main(args: Array[String]): Unit = {
    if (args.length < 1) {
      println("arg 1: should be word list file")
      return
    }

    val wordList = File(args(0))
    val gameState = GameState(randomWord(wordList))

    playGame(gameState)
  }

  @tailrec
  def playGame(game: Wordle.GameState): Unit = {
    game.state match {
      case Win | Lose =>
        println("Game Over")
        println(s"Word was: ${game.word.word}")
      case Playing =>
        println(s"You have ${game.maxGuesses - game.guesses} guesses left")
        val guess = Option(scala.io.StdIn.readLine())
        guess match {
          case None =>
            println("Exiting")
          case Some(guess) =>
            val newState = if (!guess.matches("""[a-z]{5}""")) {
              println("Guess must be 5 letters long")
              game
            } else {
              val guessCount = game.guesses + 1
              val check = game.word.matches(guess)

              // print out the status
              val status = check
                .map { case (c, guess) =>
                  guess match {
                    case Correct   => s"[$c]"
                    case Incorrect => s"X${c}X"
                    case Present   => s"~${c}~"
                  }
                }
                .mkString(" ")
              println(status)

              val forHelper = check.map { case (_, guess) => guess match {
                case Correct => '+'
                case Incorrect => 'x'
                case Present => '~'
              }}.mkString
              println(forHelper)

              if (
                check.count { case (_, guess) =>
                  guess == Correct
                } == check.length
              ) {
                println(s"You win in $guessCount guesses!")
                game.copy(guesses = guessCount, state = Win)
              } else if (guessCount >= game.maxGuesses) {
                println("You lose!")
                game.copy(guesses = guessCount, state = Lose)
              } else {
                game.copy(guesses = guessCount)
              }
            }
            playGame(newState)
        }
    }
  }

}
