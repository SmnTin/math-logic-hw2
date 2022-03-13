module MiniSAT (miniSAT) where

import Queens (SatSolver, SatResult (Sat, Unsat))
import Forms

import Data.Maybe
import Data.Tuple
import System.Process
import System.IO.Temp


relabelVars :: [Symb] -> [(Symb,Int)]
relabelVars vars = zip vars [1..]


miniSAT :: SatSolver IO
miniSAT formula@(CNF clauses) = do
    let sup = support $ fromForm formula

        symbToNumberMapping = relabelVars sup
        symbToNumber s = fromJust $ lookup s symbToNumberMapping

        numberToSymbMapping = swap <$> symbToNumberMapping
        numberToSymb i = fromJust $ lookup i numberToSymbMapping


        litToLabel lit = show $ case lit of
            Lit s -> symbToNumber s
            NLit s -> -(symbToNumber s)

        labelToMapping label = helper (read label :: Int) where
            helper i | i > 0 = (numberToSymb i, True)
                     | i < 0 = (numberToSymb (-i), False)
                     | otherwise = undefined 


        clauseToRow (Clause lits) = (++"0\n") $
            foldMap (\lit -> litToLabel lit ++ " ") lits

        dimacs :: String
        dimacs = "p cnf " ++ show (length sup) ++ " " ++ show (length clauses) ++ "\n"
            ++ foldMap clauseToRow clauses

        
        outputToResult :: String -> SatResult
        outputToResult output = let
            lines' = lines output
            in case head lines' of 
                "SAT" -> Sat $ labelToMapping <$> init (words $ head $ tail lines') 
                _     -> Unsat

    inFile  <- writeSystemTempFile "sat.in"  dimacs
    outFile <- writeSystemTempFile "sat.out" ""

    readProcessWithExitCode "minisat" [inFile, outFile] ""
    outputToResult <$> readFile outFile
