-- Programming Language Concepts
-- Project 03 - Good Morning
-- Matteo Caruso, Emma Bahr, Joshua Cajuste

import Data.List (minimumBy)
import Data.Ord (comparing)
import Control.Monad (replicateM)

-- Keypad positions: Map the digits to their (row, column):
keypad :: [(Char, (Int, Int))]
keypad = zip "1234567890" $
    [(0,0),(0,1),(0,2),
     (1,0),(1,1),(1,2),
     (2,0),(2,1),(2,2),
     (3,1)]  -- 0 is at (3,1)

positionMap :: Char -> (Int, Int)
positionMap c = case lookup c keypad of
    Just pos -> pos
    Nothing -> error ("Invalid digit: " ++ [c])

-- Verify if a number is enterable: with only down, right and same moves allowed:
isEnterable :: String -> Bool
isEnterable [] = True
isEnterable [_] = True
isEnterable (x:y:xs) =
    let (r1, c1) = positionMap x
        (r2, c2) = positionMap y
    in (r2 >= r1 && c2 >= c1) && isEnterable (y:xs)

-- Numbers from 1 to 999 (enough for k ≤ 200):
enterableNumbers :: [Int]
enterableNumbers = filter (isEnterable . show) [1..999]

-- Find the closest valid number to k:
closestTo :: Int -> Int
closestTo k = minimumBy (comparing (\x -> (abs (x - k), x))) enterableNumbers

main :: IO ()
main = do
    t <- readLn
    ks <- replicateM t readLn
    mapM_ (print . closestTo) ks
