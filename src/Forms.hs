{-# LANGUAGE InstanceSigs #-}
module Forms where

import Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty)
import Data.Foldable (Foldable(foldl'))
import Data.Maybe (fromJust)
import Data.List (union)
import Data.Monoid (All(getAll, All))


type Symb = String


infixr 7 :/\:, :/\:., :/\:*
infixr 6 :\/:, :\/:., :\/:*
infixr 5 :<->:, :-->

data Formula = Var Symb
             | Neg Formula            -- negation
             | Formula :/\: Formula   -- and
             | Formula :\/: Formula   -- or
             | Formula :--> Formula   -- implication
             | Formula :<->: Formula  -- bi implication
    deriving (Eq, Show)


class FormulaForm form where
    fromForm :: form -> Formula
    toForm :: Formula -> form


data BasisFormula = BVar Symb
                  | BNeg BasisFormula
                  | BasisFormula :/\:. BasisFormula
                  | BasisFormula :\/:. BasisFormula


data Literal = Lit Symb
             | NLit Symb


newtype Clause = Clause (NonEmpty Literal)
clause = Clause . fromJust . nonEmpty

newtype CNF = CNF (NonEmpty Clause)
newtype DNF = DNF (NonEmpty Clause)
cnf = CNF . fromJust . nonEmpty
dnf = DNF . fromJust . nonEmpty



data NNF = NNFVar Literal
         | NNF :/\:* NNF
         | NNF :\/:* NNF



fromLiteral :: Literal -> Formula
fromLiteral (Lit x) = Var x
fromLiteral (NLit x) = Neg (Var x)

fromClause :: (Formula -> Formula -> Formula) -> Clause -> Formula
fromClause binder (Clause literals) = foldr1 binder $ fmap fromLiteral literals


instance FormulaForm BasisFormula where
    fromForm :: BasisFormula -> Formula
    fromForm (BVar x) = Var x
    fromForm (BNeg f) = Neg $ fromForm f
    fromForm (a :/\:. b) = fromForm a :/\: fromForm b
    fromForm (a :\/:. b) = fromForm a :\/: fromForm b

    toForm :: Formula -> BasisFormula
    toForm (Var x)     = BVar x
    toForm (Neg f)     = BNeg $ toForm f
    toForm (a :/\: b)  = toForm a :/\:. toForm b
    toForm (a :\/: b)  = toForm a :\/:. toForm b
    toForm (a :--> b)  = toForm $ Neg a :\/: b
    toForm (a :<->: b) = toForm $ (a :--> b) :/\: (b :--> a)


instance FormulaForm NNF where
    fromForm (NNFVar lit) = fromLiteral lit
    fromForm (a :/\:* b)  = fromForm a :/\: fromForm b
    fromForm (a :\/:* b)  = fromForm a :\/: fromForm b

    toForm = toNNFb . toForm where 
        toNNFb :: BasisFormula -> NNF

        toNNFb (BNeg (BNeg f))    = toNNFb f
        toNNFb (BNeg (a :\/:. b)) = toNNFb (BNeg a) :/\:* toNNFb (BNeg b)
        toNNFb (BNeg (a :/\:. b)) = toNNFb (BNeg a) :\/:* toNNFb (BNeg b)

        toNNFb (a :/\:. b) = toNNFb a :/\:* toNNFb b
        toNNFb (a :\/:. b) = toNNFb a :\/:* toNNFb b

        toNNFb       (BVar x)  = NNFVar (Lit x)
        toNNFb (BNeg (BVar x)) = NNFVar (NLit x)


instance FormulaForm CNF where
    fromForm (CNF clauses) = foldr1 (:/\:) $ fromClause (:\/:) <$> clauses

    toForm = toCNFb . toForm where 
        toCNFb :: BasisFormula -> CNF

        toCNFb       (BVar x)  = CNF $ pure $ Clause $ pure $  Lit x
        toCNFb (BNeg (BVar x)) = CNF $ pure $ Clause $ pure $ NLit x

        toCNFb (a :/\:. b) = let CNF as = toCNFb a
                                 CNF bs = toCNFb b
                             in CNF (as <> bs)

        toCNFb (a :\/:. b) = let CNF as = toCNFb a
                                 CNF bs = toCNFb b
                             in CNF $ do
                                Clause clA <- as
                                Clause clB <- bs
                                return $ Clause $ clA <> clB

        toCNFb (BNeg (BNeg f))    = toCNFb f
        toCNFb (BNeg (a :\/:. b)) = toCNFb (BNeg a :/\:. BNeg b)
        toCNFb (BNeg (a :/\:. b)) = toCNFb (BNeg a :\/:. BNeg b)


instance FormulaForm DNF where
    fromForm (DNF clauses) = foldr1 (:\/:) $ fromClause (:/\:) <$> clauses

    toForm = toDNFb . toForm where 
        toDNFb :: BasisFormula -> DNF

        toDNFb       (BVar x)  = DNF $ pure $ Clause $ pure $  Lit x
        toDNFb (BNeg (BVar x)) = DNF $ pure $ Clause $ pure $ NLit x

        toDNFb (a :\/:. b) = let DNF as = toDNFb a
                                 DNF bs = toDNFb b
                             in DNF (as <> bs)

        toDNFb (a :/\:. b) = let DNF as = toDNFb a
                                 DNF bs = toDNFb b
                             in DNF $ do
                                Clause clA <- as
                                Clause clB <- bs
                                return $ Clause $ clA <> clB

        toDNFb (BNeg (BNeg f))    = toDNFb f
        toDNFb (BNeg (a :\/:. b)) = toDNFb (BNeg a :/\:. BNeg b)
        toDNFb (BNeg (a :/\:. b)) = toDNFb (BNeg a :\/:. BNeg b)


type Interpretation = [(Symb, Bool)]

evaluateB :: Interpretation -> BasisFormula -> Bool
evaluateB int (BVar x)    = fromJust $ lookup x int
evaluateB int (a :/\:. b) = evaluateB int a && evaluateB int b
evaluateB int (a :\/:. b) = evaluateB int a || evaluateB int b
evaluateB int (BNeg a)    = not $ evaluateB int a

evaluate :: Interpretation -> Formula -> Bool
evaluate int = evaluateB int . toForm


supportB :: BasisFormula -> [Symb]
supportB (BVar x)    = [x]
supportB (BNeg f)    = supportB f
supportB (a :/\:. b) = supportB a `union` supportB b
supportB (a :\/:. b) = supportB a `union` supportB b

support :: Formula -> [Symb]
support = supportB . toForm


inputs :: [Symb] -> [Interpretation]
inputs = mapM (\s -> [(s,True), (s,False)])

equivalent :: Formula -> Formula -> Bool
equivalent a b = getAll $ foldMap testOnInterpret $ inputs $ support a `union` support b
    where testOnInterpret int = All $ evaluate int a == evaluate int b