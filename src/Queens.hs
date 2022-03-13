module Queens ( SatSolver
              , SatResult (Sat, Unsat)
              , Pos (Pos, getPos)
              , Placement
              , placeQueens
              ) where

import Forms
import Control.Monad (guard)


data SatResult = Sat Interpretation | Unsat
type SatSolver = CNF -> SatResult

newtype Pos = Pos { getPos :: (Int, Int) }
type Placement = [Pos]


generateNeqRules :: [Symb] -> [Formula]
generateNeqRules vars = do
    a <- vars
    b <- vars
    guard $ a /= b
    return $ Neg (Var a) :\/: Neg (Var b)


posToSymb :: Int -> Pos -> Symb
posToSymb n (Pos (x, y)) = x * n + y

symbToPos :: Int -> Symb -> Pos
symbToPos n s = Pos (s `div` n, s `mod` n)


column :: Int -> Int -> [Pos]
column n c = do
    i <- [0..(n-1)]
    return $ Pos (i, c)

row :: Int -> Int -> [Pos]
row n r = do
    i <- [0..(n-1)]
    return $ Pos (r, i)

diagA :: Int -> Int -> [Pos]
diagA n d = do
    r <- [0..d]
    let c = d - r
    guard $ r < n && c < n
    return $ Pos (r, c)

diagB :: Int -> Int -> [Pos]
diagB n d = do
    r <- [0..d]
    let c = n - 1 - d + r
    guard $ r < n && c < n
    return $ Pos (r, c)


columnVars n c = posToSymb n <$> column n c
rowVars    n r = posToSymb n <$> row    n r
diagAVars  n d = posToSymb n <$> diagA  n d
diagBVars  n d = posToSymb n <$> diagB  n d


columnRules  n = foldMap (generateNeqRules . columnVars n) [0..(n-1)]
rowRules     n = foldMap (generateNeqRules . rowVars    n) [0..(n-1)]
diagARules   n = foldMap (generateNeqRules . diagAVars  n) [0..(2*n - 2)]
diagBRules   n = foldMap (generateNeqRules . diagBVars  n) [0..(2*n - 2)]


existenceRules n = foldMap (\c -> [foldr1 (:\/:) $ Var <$> columnVars n c]) [0..(n-1)]


generateFormula :: Int -> Formula
generateFormula n = foldr1 (:/\:)
    $  columnRules    n
    ++ rowRules       n
    ++ diagARules     n
    ++ diagBRules     n
    ++ existenceRules n


getPositions :: Int -> Interpretation -> Placement
getPositions n int = symbToPos n <$> map fst (filter snd int)


placeQueens :: Int -> SatSolver -> Maybe Placement
placeQueens n solver = case solver $ toForm $ generateFormula n of
    Sat int -> Just $ getPositions n int
    Unsat   -> Nothing