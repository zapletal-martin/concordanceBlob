package tasks

import org.scalatest.{Matchers, WordSpec}

class ConcordanceSpecs
  extends WordSpec
  with Matchers {

  "Concordance computation result" when {
    "the input text is empty" should {
      "be an empty collection" in {
        Concordance.computeConcordance("") should be(Seq())
      }
    }

    "the input is one word sentence" should {
      "be a single result with 1 occurence in 1st sentence" in {
        Concordance.computeConcordance("Hello.") should be(Seq(ConcordanceResult("hello", 1, Set(1))))
      }
    }

    "the input are characters without any text" should {
      "be an empty collection" in {
        Concordance.computeConcordance("$£^&:,?!.  .?") should be(Seq())
      }
    }

    "the input is the same word repeated multiple times in one sentence" should {
      "be a single result with 5 occurrences in 1st sentence" in {
        Concordance.computeConcordance("Hello hello Hello, hello %Hello") should be(Seq(ConcordanceResult("hello", 5, Set(1))))
      }
    }

    "the input is the same word repeated in multiple sentences" should {
      "be a single result with 4 occurences in 1st, 2nd, 3rd and 4th sentence" in {
        Concordance.computeConcordance("Hello. hello! Hello?.! hello!") should be(Seq(ConcordanceResult("hello", 4, Set(1, 2, 3, 4))))
      }
    }

    "the input is the same word repeated in multiple sentences and multiple times in each" should {
      "be a single result with 5 occurrences in 1st, 2nd and 3rd sentence" in {
        Concordance.computeConcordance("Hello %&*@{} hello. hello Hello!.? Hello  ") should be(
          Seq(
            ConcordanceResult("hello", 5, Set(1, 2, 3))))
      }
    }

    "the input are multiple lines including line breaks" should {
      "be one result for each word with correct number occurrences, lines of occurrence and order" in {
        Concordance.computeConcordance("Hello, how. \r\n Are. \r\n You? Hello.") should be(
          Seq(
            ConcordanceResult("are", 1, Set(2)),
            ConcordanceResult("hello", 2, Set(1, 4)),
            ConcordanceResult("how", 1, Set(1)),
            ConcordanceResult("you", 1, Set(3))))
      }
    }
  }
}
