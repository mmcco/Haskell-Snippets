-- this is the logic behind Conway's Game of Life

-- takes a grid and the block in question and returns its number of neighbors
neighCount :: [[Bool]] -> (Int, Int) -> Int
neighCount grid block = sum (map boolInt [grid !! x !! y | (x, y) <- neighbors])
    where cols = length grid
          rows = length (head grid)
          neighbors = neighList block (cols, rows)
          
          boolInt :: Bool -> Int
          boolInt False = 0
          boolInt True = 1

-- takes block in question and grid dimensions and returns list of neighbors
neighList :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
neighList (x, y) (cols, rows) = [(xn, yn) | xn <- [x-1..x+1], yn <- [y-1..y+1], xn >= 0, yn >= 0, xn < rows, yn < cols, (xn,yn) /= (x,y)]
