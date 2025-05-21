object Minesweeper:
   def annotate(l: List[String]): List[String] =
      if l.isEmpty || l.size == 1 then l
      else
         val grid: List[Array[Char]] = l.map(_.toCharArray)
         for
            row <- grid.indices
            col <- grid.head.indices
         yield println((row,col))
         l
