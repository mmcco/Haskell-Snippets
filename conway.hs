{- 
    An implementation of Conway's Game of Life
    Run using format: ./conway numRows numCols numIters prob
        numRows
        numCols
        numIters: number of boards to print
        prob: probability of each cell in the init board being alive

    Uses the IArray library for time efficiency.
-}
import Control.Concurrent
import System.Environment
import System.IO
import System.Random
import Control.Monad
import Data.List
import Control.Applicative
import Data.Array.IArray
import Data.List.Split
import System.Process

data Board = Board { cells :: Array Int Bool,
                     numRows :: Int,
                     numCols :: Int,
                     numCells :: Int
                   }

applyRules :: Board -> Board
applyRules board = Board    -- we must construct a new cells Array...
                    (listArray (0, numCells board - 1)
                     . map (isLive . neighSum)
                     $ [(r,c) | r<-[0..(numRows board - 1)], c<-[0..(numCols board - 1)]])
                    -- ...leaving the last three values in Board unmodified
                    (numRows board) (numCols board) (numCells board)

          -- neighSum returns the specified cell's number of live neighbors
    where neighSum (r,c) = (cells board ! loc (r,c), length
                                                     . filter id
                                                     . map (\x -> cells board ! x)
                                                     . filter (/= loc (r,c))
                                                     $ [loc (a,b) | a<-[r-1..r+1], b<-[c-1..c+1]])
          -- loc returns the 1D index of a cell when given it's 2D coordinates
          loc (r,c) = (numCols board) * (r `mod` numRows board) + (c `mod` numCols board)
          isLive (wasAlive, neighs) = (neighs == 3) || (wasAlive && neighs == 2)
 

instance Show Board where
    show board = intercalate "\n"
                   . map (concat . (map getPStr))
                   $ chunksOf (numRows board) (elems . cells $ board)

        where getPStr cell = if cell then "@ " else "- "

-- recursively clears terminal, prints board, delays, and applies rules
iterBoard :: Int -> Board -> IO Board
iterBoard 0 board = return board
iterBoard iters board = do
                           system "clear"
                           print board
                           threadDelay 200000
                           iterBoard (iters-1) (applyRules board)

main = do
    numRowsStr:(numColsStr:(numItersStr:(prob:[]))) <- getArgs
    gen <- getStdGen
    let numIters = read numItersStr :: Int
        numRows = read numRowsStr :: Int
        numCols = read numColsStr :: Int
        numCells = numRows * numCols
        -- generate random cells using the user-supplied liveness probability
        initCols = map (< (read prob :: Double)) . take numCells . randoms $ gen
        arr = listArray (0, numCells-1) initCols :: Array Int Bool
        board = Board arr numRows numCols numCells
    iterBoard numIters board
