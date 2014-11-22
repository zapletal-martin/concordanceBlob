package tasks

import org.scalatest.{Matchers, WordSpec}

class BlobBoundarySpecs
  extends WordSpec
  with Matchers {

  "Blob boundary computation result" when {
    "the input is empty" should {
      val input = Array[Array[Cell]](
        Array(Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero),
        Array(Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero),
        Array(Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero),
        Array(Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero),
        Array(Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero),
        Array(Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero),
        Array(Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero),
        Array(Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero),
        Array(Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero),
        Array(Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero)
      )

      "be None" in {
        BlobBoundary.computeBlobBoundary(input) should be(None)
        BlobBoundary.computeBlobBoundary2(input) should be(None)
      }
    }

    "the input is not correctly sized array" should {
      val input = Array[Array[Cell]](
        Array(Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero),
        Array(Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero),
        Array(Zero, Zero, Zero, Zero, Zero, Zero, Zero)
      )

      "be None" in {
        BlobBoundary.computeBlobBoundary(input) should be(None)
        BlobBoundary.computeBlobBoundary2(input) should be(None)
      }
    }

    "the input is only one One" should {
      val input = Array[Array[Cell]](
        Array(Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero),
        Array(Zero, One, One, Zero, Zero, Zero, Zero, Zero, Zero, Zero),
        Array(Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero),
        Array(Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero),
        Array(Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero),
        Array(Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero),
        Array(Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero),
        Array(Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero),
        Array(Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero),
        Array(Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero)
      )

      "be top:1, left:1, bottom:1, right:2" in {
        val result = BlobBoundary.computeBlobBoundary(input).get
        val result2 = BlobBoundary.computeBlobBoundary2(input).get

        result.top should be(1)
        result.left should be(1)
        result.bottom should be(1)
        result.right should be(2)

        result2.top should be(1)
        result2.left should be(1)
        result2.bottom should be(1)
        result2.right should be(2)
      }
    }

    "the input is fork shaped blob" should {
      val input = Array[Array[Cell]](
        Array(Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero),
        Array(Zero, Zero, One, Zero, Zero, Zero, Zero, Zero, Zero, Zero),
        Array(Zero, Zero, One, Zero, Zero, One, One, Zero, Zero, Zero),
        Array(Zero, Zero, One, Zero, Zero, One, Zero, Zero, Zero, Zero),
        Array(Zero, Zero, One, One, One, One, Zero, Zero, Zero, Zero),
        Array(Zero, Zero, Zero, One, Zero, Zero, Zero, Zero, Zero, Zero),
        Array(Zero, Zero, Zero, One, Zero, Zero, Zero, Zero, Zero, Zero),
        Array(Zero, One, One, One, Zero, Zero, Zero, Zero, Zero, Zero),
        Array(Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero),
        Array(Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero)
      )

      "be top:1, left:1, bottom:7, right:6" in {
        val result = BlobBoundary.computeBlobBoundary(input).get
        val result2 = BlobBoundary.computeBlobBoundary2(input).get

        result.top should be(1)
        result.left should be(1)
        result.bottom should be(7)
        result.right should be(6)

        result2.top should be(1)
        result2.left should be(1)
        result2.bottom should be(7)
        result2.right should be(6)
      }
    }

    "the input is area boundary shaped blob" should {
      val input = Array[Array[Cell]](
        Array(One, One, One, One, One, One, One, One, One, One),
        Array(One, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, One),
        Array(One, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, One),
        Array(One, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, One),
        Array(One, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, One),
        Array(One, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, One),
        Array(One, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, One),
        Array(One, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, One),
        Array(One, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, One),
        Array(One, One, One, One, One, One, One, One, One, One)
      )

      "be top:0, left:0, bottom:9, right:9" in {
        val result = BlobBoundary.computeBlobBoundary(input).get
        val result2 = BlobBoundary.computeBlobBoundary2(input).get

        result.top should be(0)
        result.left should be(0)
        result.bottom should be(9)
        result.right should be(9)

        result2.top should be(0)
        result2.left should be(0)
        result2.bottom should be(9)
        result2.right should be(9)
      }
    }

    "the example input is used" should {
      val input = Array[Array[Cell]](
        Array(Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero),
        Array(Zero, Zero, One, One, One, Zero, Zero, Zero, Zero, Zero),
        Array(Zero, Zero, One, One, One, One, One, Zero, Zero, Zero),
        Array(Zero, Zero, One, Zero, Zero, Zero, One, Zero, Zero, Zero),
        Array(Zero, Zero, One, One, One, One, One, Zero, Zero, Zero),
        Array(Zero, Zero, Zero, Zero, One, Zero, One, Zero, Zero, Zero),
        Array(Zero, Zero, Zero, Zero, One, Zero, One, Zero, Zero, Zero),
        Array(Zero, Zero, Zero, Zero, One, One, One, Zero, Zero, Zero),
        Array(Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero),
        Array(Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero)
      )

      "be top:1, left:2, bottom:7, right:6" in {
        val result = BlobBoundary.computeBlobBoundary(input).get
        val result2 = BlobBoundary.computeBlobBoundary2(input).get

        result.top should be(1)
        result.left should be(2)
        result.bottom should be(7)
        result.right should be(6)

        result2.top should be(1)
        result2.left should be(2)
        result2.bottom should be(7)
        result2.right should be(6)
      }
    }
  }
}
