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
applyRules board = Board
                    (listArray (0, numCells board - 1)
                     . map (isLive . neighSum)
                     $ [(r,c) | r<-[0..(numRows board - 1)], c<-[0..(numCols board - 1)]])
                    (numRows board) (numCols board) (numCells board)

    where neighSum (r,c) = (cells board ! loc (r,c), length
                                                     . filter id
                                                     . map (\x -> cells board ! x)
                                                     . filter (/= loc (r,c))
                                                     $ [loc (a,b) | a<-[r-1..r+1], b<-[c-1..c+1]])
          loc (r,c) = (numCols board) * (r `mod` numRows board) + (c `mod` numCols board)
          isLive (wasAlive, neighs) = neighs == 3 || (wasAlive && neighs == 2)
          

instance Show Board where
    show board = intercalate "\n"
                   . map (concat . (map getPStr))
                   $ chunksOf (numRows board) (elems . cells $ board)

        where getPStr cell = if cell then "@ " else "- "

iterBoard :: Int -> Board -> IO Board
iterBoard 0 board = return board
iterBoard iters board = do
                           system "clear"
                           print board
                           threadDelay 200000
                           iterBoard (iters-1) (applyRules board)

main = do
    numColsStr:(numRowsStr:(numItersStr:[])) <- getArgs
    gen <- getStdGen
    let numIters = read numItersStr :: Int
        numRows = read numRowsStr :: Int
        numCols = read numColsStr :: Int
        numCells = numRows * numCols
        initCols = take numCells (randoms gen)
        arr = listArray (0, numCells-1) initCols :: Array Int Bool
        board = Board arr numRows numCols numCells
    iterBoard numIters board
