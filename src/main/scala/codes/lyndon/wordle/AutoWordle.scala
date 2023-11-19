package codes.lyndon.wordle

import better.files.File

import scala.collection.mutable
import scala.util.Random

object AutoWordle {

  def main(args: Array[String]): Unit = {
    if (args.length < 1) {
      println("arg 1: should be word list file")
      return
    }
    val gameCount = if (args.length > 1) args(1).toInt else 1
    val shouldLog = if (args.length > 2) args(2).toBoolean else true

    val wordList = File(args(0))

    println(s"Autoplaying Wordle for $gameCount games")

    val results = 1
      .to(gameCount)
      .map { gameNo =>
        if(shouldLog) println(s"Playing game $gameNo")
        val actualWord = Wordle.randomWord(wordList)
        val result = autoPlay(actualWord, wordList, shouldLog)
        if(shouldLog) println()
        result
      }

    val wins = results.filter { a => a.win }
    val losses = results.filterNot { a => a.win }

    println(
      s"${wins.length} wins vs ${losses.length} losses (${(losses.length.toDouble / gameCount.toDouble) * 100}%)"
    )

    println()
    println("Wins:")
    println("guesses,count")
    wins
      .groupBy { case AutoResult(_, _, guesses, _) => guesses.length }
      .view
      .mapValues(_.length)
      .toSeq
      .sortBy(_._1)
      .foreach { case (guesses, count) => println(s"$guesses,$count") }

    println()
    println("Losses:")
    println("wordsLeft,count")
    losses
      .groupBy { case AutoResult(_, _, _, wordsLeft) => wordsLeft.length }
      .view
      .mapValues(_.length)
      .toSeq
      .sortBy(_._1)
      .foreach { case (wordsLeft, count) => println(s"$wordsLeft,$count") }

    val (worstCount, worstSet) = losses
      .groupBy { case AutoResult(_, _, _, wordsLeft) => wordsLeft.length }
      .toSeq
      .sortBy(_._1)
      .reverse
      .head
    println(s"Worst Count $worstCount")
    worstSet.foreach { case AutoResult(_, actualWord, guesses, wordsLeft) =>
      println(s"Actual Word: $actualWord")
      println(s"Guesses: ${guesses.mkString(", ")}")
      println(s"Words Left: ${wordsLeft.mkString(", ")}")
    }

  }

  def autoPlay(actualWord: String, wordList: File, shouldLog: Boolean = true): AutoResult = {

    def log(message: String): Unit = if (shouldLog) println(message)

    val words = mutable.ListBuffer.from(Random.shuffle(wordList.lines.toSeq))
    log(s"Word list of ${words.size} loaded")
    var helperState =
      WordleHelper.GuessState(WordleHelper.WordValidity(), words)

    log("Beginning game")
    var gameState = Wordle.GameState(actualWord)
    val guesses = mutable.ListBuffer[String]()
    while (gameState.state == Wordle.Playing) {
      val guessWord = WordleHelper.suggestWord(words)
      log(s"Guessing word: $guessWord")
      guesses += guessWord
      val check = gameState.word.matches(guessWord)

      // print out the status
      val status = check
        .map { case (c, guess) =>
          guess match {
            case Wordle.Correct   => s"[$c]"
            case Wordle.Incorrect => s"X${c}X"
            case Wordle.Present   => s"~${c}~"
          }
        }
        .mkString(" ")
      log(status)

      helperState = WordleHelper.updateState(
        guessWord,
        check.map { case (c, guess) =>
          (
            c,
            guess match {
              case Wordle.Correct   => WordleHelper.Valid
              case Wordle.Incorrect => WordleHelper.Invalid
              case Wordle.Present   => WordleHelper.WrongPosition
            }
          )
        },
        helperState
      )

      gameState =
        if (
          check.count { case (_, guess) =>
            guess == Wordle.Correct
          } == check.length
        ) {
          gameState.copy(guesses = guesses.length, state = Wordle.Win)
        } else if (guesses.length >= gameState.maxGuesses) {
          gameState.copy(guesses = guesses.length, state = Wordle.Lose)
        } else {
          gameState.copy(guesses = guesses.length)
        }
    }

    gameState.state match {
      case Wordle.Win =>
        log(s"Won game in ${guesses.length}")
        AutoResult(win = true, actualWord, guesses.toSeq, words.toSeq)
      case Wordle.Lose =>
        log(s"Lost game with ${words.length} words remaining to guess")
        AutoResult(win = false, actualWord, guesses.toSeq, words.toSeq)
    }
  }

  case class AutoResult(
      win: Boolean,
      actualWord: String,
      guesses: Seq[String],
      wordsLeft: Seq[String]
  )

}
