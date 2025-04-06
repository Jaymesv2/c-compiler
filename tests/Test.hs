{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main ( main ) where
-- How to do the tests
-- https://hackage.haskell.org/package/HTF-0.15.0.2/docs/Test-Framework-Tutorial.html
import Test.Framework

-- Import modules defining HTF tests like this:
--import {-@ HTF_TESTS @-} Compiler.Parser.SrcLoc

main :: IO ()
main = htfMain htf_importedTests -- all tests in modules imported via {-@ HTF_TESTS @-} are available in htf_importedTests

