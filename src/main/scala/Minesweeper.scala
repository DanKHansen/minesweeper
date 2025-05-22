object Minesweeper:
   def annotate(grid: List[String]): List[String] =
      if grid.isEmpty || grid.head.isEmpty then return grid
      val adj = Seq((-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1))
      grid.zipWithIndex.map { case (r, idxR) =>
         r.zipWithIndex.map { case (c, idxC) =>
            if c == '*' then c
            else
               adj.map { (dx, dy) => (idxR + dx, idxC + dy) }
                  .filter { case (x, y) => grid.indices.contains(x) && grid.head.indices.contains(y) }
                  .count { case (i, j) => grid(i)(j) == '*' }
                  .toString
                  .replace('0', ' ')
         }.mkString
      }