import scala.annotation.tailrec
import scala.util.boundary
import scala.util.boundary.break
import scala.math.sqrt

class SudokuVergleichSolver(val size: Int) {
  private type PossibilitiesGrid = Array[Array[Set[Int]]]
  private type RelationsMap = Map[(Int, Int), Char]

  
  val boxSize: Int = sqrt(size).toInt
  require(boxSize * boxSize == size, "O tamanho do grid deve ser um quadrado perfeito (4, 9, 16...).")

  
  private val allPossible: Set[Int] = (1 to size).toSet



  

  def solve(grid: PossibilitiesGrid, hRels: RelationsMap, vRels: RelationsMap): Option[PossibilitiesGrid] = {
    boundary {
      val propagatedGrid = initialPropagation(grid, hRels, vRels)

      var allSingleton = true
      var minimumRemainingValue = size + 1
      var rowMRV = -1
      var colMRV = -1

      for (r <- 0 until size; c <- 0 until size) {
        val possibilities = propagatedGrid(r)(c)
        if (possibilities.isEmpty) break(None) 

        if (possibilities.size > 1) {
          allSingleton = false
          if (possibilities.size < minimumRemainingValue) {
            minimumRemainingValue = possibilities.size
            rowMRV = r
            colMRV = c
          }
        }
      }

      if (allSingleton) {
        break(Some(propagatedGrid)) 
      } else {
        val possibilitiesToTry = propagatedGrid(rowMRV)(colMRV)
        for (p <- possibilitiesToTry) {
          val gridCopy = propagatedGrid.map(_.clone)
          gridCopy(rowMRV)(colMRV) = Set(p)
          val result = solve(gridCopy, hRels, vRels)
          if (result.isDefined) {
            break(result)
          }
        }
        break(None) 
      }
    }
  }

  private def initialPropagation(grid: PossibilitiesGrid, hRels: RelationsMap, vRels: RelationsMap): PossibilitiesGrid = {
    @tailrec
    def loop(currentGrid: PossibilitiesGrid): PossibilitiesGrid = {
      val (gridAfterRows, changedByRows) = applyRowRules(currentGrid)
      val (gridAfterCols, changedByCols) = applyColRules(gridAfterRows)
      val (gridAfterQuads, changedByQuads) = applyQuadrantRules(gridAfterCols)
      val (finalGrid, changedByComparison) = applyComparisonRules(gridAfterQuads, hRels, vRels)

      if (changedByRows || changedByCols || changedByQuads || changedByComparison) {
        loop(finalGrid)
      } else {
        finalGrid
      }
    }
    loop(grid)
  }

  

  private def applyRowRules(grid: PossibilitiesGrid): (PossibilitiesGrid, Boolean) = {
    var changed = false
    val newGrid = grid.map(_.clone)
    for (r <- 0 until size) {
      val solvedValues = newGrid(r).filter(_.size == 1).flatten.toSet
      if (solvedValues.nonEmpty) {
        for (c <- 0 until size if newGrid(r)(c).size > 1) {
          val initialSize = newGrid(r)(c).size
          newGrid(r)(c) = newGrid(r)(c) -- solvedValues 
          if (newGrid(r)(c).size < initialSize) changed = true
        }
      }
    }
    (newGrid, changed)
  }

  private def applyColRules(grid: PossibilitiesGrid): (PossibilitiesGrid, Boolean) = {
    var changed = false
    val newGrid = grid.map(_.clone)
    for (c <- 0 until size) {
      val columnCells = for (r <- 0 until size) yield newGrid(r)(c)
      val solvedValues = columnCells.filter(_.size == 1).flatten.toSet
      if (solvedValues.nonEmpty) {
        for (r <- 0 until size if newGrid(r)(c).size > 1) {
          val initialSize = newGrid(r)(c).size
          newGrid(r)(c) = newGrid(r)(c) -- solvedValues 
          if (newGrid(r)(c).size < initialSize) changed = true
        }
      }
    }
    (newGrid, changed)
  }

  private def applyQuadrantRules(grid: PossibilitiesGrid): (PossibilitiesGrid, Boolean) = {
    var changed = false
    val newGrid = grid.map(_.clone)
    for (boxRow <- 0 until boxSize; boxCol <- 0 until boxSize) {
      val startRow = boxRow * boxSize
      val startCol = boxCol * boxSize
      val quadrantCells = for { r <- startRow until startRow + boxSize; c <- startCol until startCol + boxSize } yield newGrid(r)(c)
      val solvedValues = quadrantCells.filter(_.size == 1).flatten.toSet
      if (solvedValues.nonEmpty) {
        for (r <- startRow until startRow + boxSize; c <- startCol until startCol + boxSize if newGrid(r)(c).size > 1) {
          val initialSize = newGrid(r)(c).size
          newGrid(r)(c) = newGrid(r)(c) -- solvedValues
          if (newGrid(r)(c).size < initialSize) changed = true
        }
      }
    }
    (newGrid, changed)
  }

  private def applyComparisonRules(grid: PossibilitiesGrid, hRels: RelationsMap, vRels: RelationsMap): (PossibilitiesGrid, Boolean) = {
    var changed = false
    val newGrid = grid.map(_.clone)

    def updateChanged(initialSize: Int, finalSize: Int): Unit = if (finalSize < initialSize) changed = true

    for (r <- 0 until size; c <- 0 until size) {
      
      hRels.get((r, c)).foreach { relation =>
        val left = newGrid(r)(c)
        val right = newGrid(r)(c + 1)
        val (s1, s2) = (left.size, right.size)

        relation match {
          case '>' => 
            if (right.nonEmpty) newGrid(r)(c) = left -- (1 to right.min)
            if (left.nonEmpty) newGrid(r)(c + 1) = right -- (left.max to size)
          case '<' => 
            if (right.nonEmpty) newGrid(r)(c) = left -- (right.max to size)
            if (left.nonEmpty) newGrid(r)(c + 1) = right -- (1 to left.min)
        }
        updateChanged(s1, newGrid(r)(c).size)
        updateChanged(s2, newGrid(r)(c + 1).size)
      }

      
      vRels.get((r, c)).foreach { relation =>
        val top = newGrid(r)(c)
        val bottom = newGrid(r + 1)(c)
        val (s1, s2) = (top.size, bottom.size)

        relation match {
          case '>' => 
            if (bottom.nonEmpty) newGrid(r)(c) = top -- (1 to bottom.min)
            if (top.nonEmpty) newGrid(r + 1)(c) = bottom -- (top.max to size)
          case '<' => 
            if (bottom.nonEmpty) newGrid(r)(c) = top -- (bottom.max to size)
            if (top.nonEmpty) newGrid(r + 1)(c) = bottom -- (1 to top.min)
        }
        updateChanged(s1, newGrid(r)(c).size)
        updateChanged(s2, newGrid(r + 1)(c).size)
      }
    }
    (newGrid, changed)
  }
  

  
  def generatePossibilitiesMatrix(grid: Array[Array[Int]]): PossibilitiesGrid = {
    val possibilitiesMatrix = Array.ofDim[Set[Int]](size, size)
    for (r <- 0 until size; c <- 0 until size) {
      val number = grid(r)(c)
      if (number != 0) possibilitiesMatrix(r)(c) = Set(number)
      else possibilitiesMatrix(r)(c) = allPossible
    }
    possibilitiesMatrix
  }

}