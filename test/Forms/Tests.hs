{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Forms.Tests (tests) where


import Control.Monad

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.SmallCheck as SC
import Test.Tasty.HUnit

import Forms


tests :: TestTree
tests = testGroup "Tests" [ testEvaluation
                          , testEquivalence
                          , testEncoding 
                          ]


instance Arbitrary Formula where
    arbitrary = sized genFormula where
        genFormula 0         = fmap Var arbitrary
        genFormula n | n > 0 = oneof
            [ fmap Var arbitrary
            , liftM2 (:/\:) subtree subtree
            , liftM2 (:\/:) subtree subtree
            , liftM2 (:-->) subtree subtree
            , liftM2 (:<->:) subtree subtree
            ]
            where subtree = genFormula $ min 3 $ n `div` 2
        genFormula _ = undefined


testEquivalence :: TestTree
testEquivalence = testGroup "Formulas equivalence" [
        QC.testProperty "Basis formula equivalent to original" $
            \f -> equivalent f $ fromForm $ (toForm :: Formula -> BasisFormula) f
      , QC.testProperty "NNF equivalent to original" $
            \f -> equivalent f $ fromForm $ (toForm :: Formula -> NNF) f
      , QC.testProperty "CNF equivalent to original" $
            \f -> equivalent f $ fromForm $ (toForm :: Formula -> CNF) f
      , QC.testProperty "DNF equivalent to original" $
            \f -> equivalent f $ fromForm $ (toForm :: Formula -> DNF) f
    ]

testEncoding :: TestTree
testEncoding = testGroup "Forms encoding" [
        testCase "NNF encoding" $ let
            nnf     = NNFVar (NLit "a") :/\:* NNFVar (Lit "b") :\/:* NNFVar (NLit "c")
            formula = Neg (Var "a") :/\: Var "b" :\/: Neg (Var "c")
            in assertEqual "" formula $ fromForm nnf
      , testCase "CNF encoding" $ let
            form    = cnf [clause [Lit "a", NLit "b"], clause [NLit "c"]]
            formula = (Var "a" :\/: Neg (Var "b")) :/\: Neg (Var "c")
            in assertEqual "" formula $ fromForm form
      , testCase "DNF encoding" $ let
            form    = dnf [clause [Lit "a", NLit "b"], clause [NLit "c"]]
            formula = (Var "a" :/\: Neg (Var "b")) :\/: Neg (Var "c")
            in assertEqual "" formula $ fromForm form
    ]

testEvaluation :: TestTree
testEvaluation = testGroup "Evaluation" [
        SC.testProperty "Variable evaluation" $
            \b -> evaluate [("x", b)] (Var "x") == b
      , SC.testProperty "Conjunction evaluation" $
            \a b -> evaluate [("x", a), ("y", b)] (Var "x" :/\: Var "y") == (a && b)
      , SC.testProperty "Disjunction evaluation" $
            \a b -> evaluate [("x", a), ("y", b)] (Var "x" :\/: Var "y") == (a || b)
      , SC.testProperty "Implication evaluation" $
            \a b -> evaluate [("x", a), ("y", b)] (Var "x" :--> Var "y") == (not a || b)
      , SC.testProperty "Bi implication evaluation" $
            \a b -> evaluate [("x", a), ("y", b)] (Var "x" :<->: Var "y") == (a == b)
    ]