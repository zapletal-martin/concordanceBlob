package tasks

trait Cell {
  def isOne: Boolean
  def processed: Boolean
}
case object One extends Cell {
  override def isOne: Boolean = true
  override def processed: Boolean = false
}
case object Zero extends Cell {
  override def isOne: Boolean = false
  override def processed: Boolean = false
}
case object Processed extends Cell {
  override def isOne: Boolean = false
  override def processed: Boolean = true
}

case class Index(row: Int, column: Int)

case class IntermediaryResult(input: Array[Array[Cell]], index: Index, top: Int, left: Int, bottom: Int, right: Int)

case class BlobBoundaryResult(top: Int, left: Int, bottom: Int, right: Int)

object BlobBoundary {

  private def expectedArraySize(input: Array[Array[Cell]]) = input.size == 10 && input.forall(_.size == 10)

  def computeBlobBoundary(input: Array[Array[Cell]]): Option[BlobBoundaryResult] = {

    def indexInBounds(index: Index) = index.row >= 0 && index.row < 10 && index.column >= 0 && index.column < 10

    def newArray(input: Array[Array[Cell]], index: Index) = {
      val processedArray = input.map(_.clone())
      processedArray(index.row)(index.column) = Processed
      processedArray
    }

    def findBlobStart(input: Array[Array[Cell]], index: Index): Option[IntermediaryResult] = {
      if(!expectedArraySize(input))
        None
      else
        if(input(index.row)(index.column).isOne)
            Some(IntermediaryResult(input, index, index.row, 9, 0, 0))
        else {
          if (index.column == 9 && index.row == 9)
            None
          else if (index.column == 9)
            findBlobStart(newArray(input, index), Index(index.row + 1, 0))
          else
            findBlobStart(newArray(input, index), index.copy(column = index.column + 1))
      }
    }

    def findBlobBoundary(result: IntermediaryResult): IntermediaryResult  = {
      val index = result.index
      val input = result.input

      if(indexInBounds(index) && !input(index.row)(index.column).processed) {
        if(input(index.row)(index.column).isOne) {
          val rightResult = findBlobBoundary(result.copy(input = newArray(input, index), index = index.copy(column = index.column + 1)))
          val leftResult = findBlobBoundary(result.copy(input = rightResult.input, index = index.copy(column = index.column - 1)))
          val bottomResult = findBlobBoundary(result.copy(input = leftResult.input, index = index.copy(row = index.row + 1)))
          val topResult = findBlobBoundary(result.copy(input = bottomResult.input, index = index.copy(row = index.row - 1)))

          IntermediaryResult(
            topResult.input,
            index,
            Set(index.row, leftResult.top, rightResult.top, bottomResult.top, topResult.top).min,
            Set(index.column, leftResult.left, rightResult.left, bottomResult.left, topResult.left).min,
            Set(index.row, leftResult.bottom, rightResult.bottom, bottomResult.bottom, topResult.bottom).max,
            Set(index.column, leftResult.right, rightResult.right, bottomResult.right, topResult.right).max
          )
        } else {
          result.copy(input = newArray(input, index))
        }
      } else {
        result
      }
    }

    findBlobStart(input, Index(0, 0)).map(f => {
      val recursionResult = findBlobBoundary(f)
      BlobBoundaryResult(recursionResult.top, recursionResult.left, recursionResult.bottom, recursionResult.right)
    })
  }

  def computeBlobBoundary2(input: Array[Array[Cell]]): Option[BlobBoundaryResult] = {
    def blobExists(input: Array[Array[Cell]]) =
      input.flatMap(x => x).exists(_.isOne)

    if(expectedArraySize(input) && blobExists(input))
      Some(BlobBoundaryResult(
        input.map(_.exists(_.isOne)).indexOf(true),
        input.filter(_.exists(_.isOne)).map(_.indexOf(One)).min,
        input.map(_.exists(_.isOne)).lastIndexOf(true),
        input.map(_.lastIndexOf(One)).max))
    else
      None
  }
}