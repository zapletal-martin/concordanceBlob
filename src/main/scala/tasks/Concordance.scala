package tasks

case class ConcordanceResult(word: String, count: Int, occurrences: Set[Int])

object Concordance {

  def computeConcordance(text: String): Seq[ConcordanceResult] = {
    def filterSpecialCharacters(text: String) = text.replaceAll("\\W+", " ")

    def splitSentenceToWords(sentence: String) = split(filterSpecialCharacters(sentence), "\\s+")

    def splitTextToSentences(text: String) = split(text, "[.?!]")

    def split(text: String, separatorRegex: String) = text.split(separatorRegex).map(_.trim).filter(_ != "")

    def mapToResult(group: (String, Array[(String, Int)])) =
      ConcordanceResult(group._1, group._2.size, group._2.map(_._2).toSet)

    val sentences = splitTextToSentences(text.toLowerCase)

    sentences
      .zip(1 to sentences.length)
      .flatMap(s => splitSentenceToWords(s._1).map((_, s._2)))
      .groupBy(_._1)
      .toSeq
      .sortBy(_._1)
      .map(mapToResult)
  }
}