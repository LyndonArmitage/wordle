package codes.lyndon.wordle

object WordListGenerator {

  import better.files._

  def main(args: Array[String]): Unit = {
    // arg 1 should be a folder containing our various text files
    // arg 2 should be the output path for our word list
    if (args.length < 2) {
      throw new IllegalArgumentException("Not enough arguments")
    }

    val textFolder = File(args(0))
    val outputFile = File(args(1))

    val textFiles = textFolder
      .list(
        f => f.isRegularFile && f.extension.contains(".txt"),
        1
      )
      .toSeq

    // This pattern matches only simple 5 letter words
    val regex = """([A-Z]{5})""".r

    val words = textFiles
      .flatMap { file =>
        file
          // This splits each file up into a sequence of strings seperated by
          // any of the characters in the given string
          .scanner(StringSplitter.anyOf("\n\t .,\""))()(
            _.iterator
              .map(_.toUpperCase)
              .filter(regex.matches(_))
              .toSeq
          )
          .distinct
      }
      .distinct
      .sorted

    // Finally write out the word list
    outputFile.bufferedWriter()(writer =>
      words.foreach(word => writer.write(s"$word\n"))
    )
  }

}
