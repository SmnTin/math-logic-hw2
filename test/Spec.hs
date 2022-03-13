module Main where
    
import Test.Tasty (defaultMain, testGroup, TestTree)

import qualified Forms.Tests (tests)

main :: IO ()
main = defaultMain tests

tests :: TestTree 
tests = testGroup "Tests" [ Forms.Tests.tests ]