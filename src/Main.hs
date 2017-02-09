module Main where

import Canonicalize
import System.Environment
import Data.List
import qualified Data.Rewriting.Problem as Problem
import qualified Text.PrettyPrint.ANSI.Leijen as Pretty

main :: IO ()
main = do
    [fn] <- getArgs
    Right problem <- Problem.fromFile fn
    putStrLn $ ($ "") $ Pretty.displayS $ Pretty.renderCompact $ Problem.prettyWST Pretty.text Pretty.text $ canonicalize problem
