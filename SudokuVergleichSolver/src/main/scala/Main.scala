import scala.math.sqrt

object Main {
  def printGrid(solvedGrid: Array[Array[Int]], size: Int): Unit = {
    val boxSize: Int = sqrt(size).toInt
    require(boxSize * boxSize == size, "O tamanho do grid deve ser um quadrado perfeito (4, 9, 16...).")
    println("+-------+-------+-------+")
    for (r <- 0 until size) {
      print("| ")
      for (c <- 0 until size) {
        print(solvedGrid(r)(c) + " ")
        if ((c + 1) % boxSize == 0) print("| ")
      }
      println()
      if ((r + 1) % boxSize == 0) println("+-------+-------+-------+")
    }
  }

  def main(args: Array[String]): Unit = {
    println("--- Resolvendo um Sudoku 4x4 ---")

    val solver4x4 = new SudokuVergleichSolver(4)

    val initialGrid4x4: Array[Array[Int]] = Array.fill(4,4)(0)

    val hRels4x4: Map[(Int, Int), Char] = Map(
      (0, 0) -> '<', (0, 2) -> '>',
      (1, 0) -> '<', (1, 2) -> '>',
      (2, 0) -> '<', (2, 2) -> '<',
      (3, 0) -> '>', (3, 2) -> '>',
    )
    val vRels4x4: Map[(Int, Int), Char] = Map(
      (0, 0) -> '>', (2, 0) -> '<',
      (0, 1) -> '>', (2, 1) -> '<',
      (0, 2) -> '<', (2, 2) -> '<',
      (0, 3) -> '<', (2, 3) -> '>',
    )

    val possibilities4x4 = solver4x4.generatePossibilitiesMatrix(initialGrid4x4)
    val startTime4x4 = System.nanoTime()
    val solutionOption4x4 = solver4x4.solve(possibilities4x4, hRels4x4, vRels4x4)
    val durationMs4x4 = (System.nanoTime() - startTime4x4) / 1_000_000.0


    solutionOption4x4 match {
      case Some(solutionGrid4x4) =>
        println(">>> Solução 4x4 Encontrada! <<<")
        val finalGrid4x4 = solutionGrid4x4.map(_.map(_.head))
        printGrid(finalGrid4x4, 4)
        println(f"Busca concluída em $durationMs4x4%.2f ms.\n")
      case None =>
        println(">>> Nenhuma solução encontrada. <<<")
    }

    println("--- Resolvendo um Sudoku 9x9 ---")

    val solver9x9 = new SudokuVergleichSolver(9)

    val initialGrid9x9: Array[Array[Int]] = Array.fill(9, 9)(0)

    val hRels9x9: Map[(Int, Int), Char] = Map(
      (0,0) -> '<', (0,1) -> '>', (0,3) -> '<', (0,4) -> '<', (0,6) -> '>', (0,7) -> '>',
      (1,0) -> '<', (1,1) -> '<', (1,3) -> '<', (1,4) -> '>', (1,6) -> '>', (1,7) -> '<',
      (2,0) -> '<', (2,1) -> '<', (2,3) -> '<', (2,4) -> '>', (2,6) -> '>', (2,7) -> '<',
      (3,0) -> '>', (3,1) -> '>', (3,3) -> '>', (3,4) -> '<', (3,6) -> '<', (3,7) -> '>',
      (4,0) -> '>', (4,1) -> '<', (4,3) -> '>', (4,4) -> '<', (4,6) -> '>', (4,7) -> '>',
      (5,0) -> '<', (5,1) -> '>', (5,3) -> '<', (5,4) -> '>', (5,6) -> '>', (5,7) -> '<',
      (6,0) -> '<', (6,1) -> '>', (6,3) -> '>', (6,4) -> '>', (6,6) -> '<', (6,7) -> '>',
      (7,0) -> '>', (7,1) -> '<', (7,3) -> '>', (7,4) -> '<', (7,6) -> '<', (7,7) -> '<',
      (8,0) -> '<', (8,1) -> '>', (8,3) -> '<', (8,4) -> '<', (8,6) -> '<', (8,7) -> '>'
    )

    val vRels9x9: Map[(Int, Int), Char] = Map(
      (0, 0) -> '>', (1, 0) -> '<', (3, 0) -> '>', (4, 0) -> '>', (6, 0) -> '>', (7, 0) -> '<',
      (0, 1) -> '<', (1, 1) -> '<', (3, 1) -> '<', (4, 1) -> '<', (6, 1) -> '>', (7, 1) -> '<',
      (0, 2) -> '<', (1, 2) -> '<', (3, 2) -> '<', (4, 2) -> '<', (6, 2) -> '>', (7, 2) -> '>',
      (0, 3) -> '>', (1, 3) -> '<', (3, 3) -> '>', (4, 3) -> '<', (6, 3) -> '<', (7, 3) -> '>',
      (0, 4) -> '>', (1, 4) -> '>', (3, 4) -> '>', (4, 4) -> '<', (6, 4) -> '>', (7, 4) -> '>',
      (0, 5) -> '>', (1, 5) -> '>', (3, 5) -> '>', (4, 5) -> '>', (6, 5) -> '>', (7, 5) -> '<',
      (0, 6) -> '<', (1, 6) -> '>', (3, 6) -> '<', (4, 6) -> '>', (6, 6) -> '<', (7, 6) -> '>',
      (0, 7) -> '>', (1, 7) -> '>', (3, 7) -> '>', (4, 7) -> '>', (6, 7) -> '<', (7, 7) -> '<',
      (0, 8) -> '<', (1, 8) -> '>', (3, 8) -> '<', (4, 8) -> '>', (6, 8) -> '<', (7, 8) -> '>'
    )


    val possibilities9x9 = solver9x9.generatePossibilitiesMatrix(initialGrid9x9)
    val startTime9x9 = System.nanoTime()
    val solutionOption9x9 = solver9x9.solve(possibilities9x9, hRels9x9, vRels9x9)
    val durationMs9x9 = (System.nanoTime() - startTime9x9) / 1_000_000.0

    solutionOption9x9 match {
      case Some(solutionGrid9x9) =>
        println(">>> Solução 9x9 Encontrada! <<<")
        val finalGrid9x9 = solutionGrid9x9.map(_.map(_.head))
        printGrid(finalGrid9x9, 9)
        println(f"Busca concluída em $durationMs9x9%.2f ms.\n")
      case None =>
        println(">>> Nenhuma solução encontrada. <<<")
    }
  }
}