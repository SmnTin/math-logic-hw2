module Main where

import System.Environment (getArgs)

import MiniSAT
import Queens

instance Show Pos where
    show (Pos (x, y)) = show (x + 1) ++ " " ++ show (y + 1)

main = do
    [nStr] <- getArgs
    let n = read nStr :: Int
    placement <- placeQueens n miniSAT
    mapM_ (mapM_ print) placement