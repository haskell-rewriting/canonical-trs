module Main where

import Canonicalize
import TPDB
import qualified Data.Rewriting.Problem as Problem
import qualified Text.PrettyPrint.ANSI.Leijen as Pretty
import System.Environment
import System.FilePath
import System.Exit
import System.IO
import qualified Paths_canonical_trs as P
import Data.Version

main :: IO ()
main = do
    args <- getArgs
    let fn = head args
        ext = takeExtension fn
    case (args, ext) of
        ([_], ".trs") -> do
            res <- Problem.fromFile fn
            case res of
                Right problem -> process problem
                Left error -> do
                    hPutStrLn stderr $ show error
                    exitFailure
        ([_], ".xml") -> do
--            problem <- Data.Rewriting.Problem.Xml.xmlFileToProblem fn
--            process problem
            res <- fromXTCFile fn
            case res of
                Right problem -> process problem
                Left error -> do
                    hPutStrLn stderr error
                    exitFailure
        _ -> do
            prog <- getProgName
            hPutStrLn stderr $ "Usage: " ++ prog ++ " [FILE.trs|FILE.xml]"
            exitFailure

process :: Problem.Problem String String -> IO ()
process =
    putStrLn .
    (("(COMMENT produced by " ++ prog ++ ")\n") ++) .
    ($ "") . Pretty.displayS . Pretty.renderCompact .
    Problem.prettyWST Pretty.text Pretty.text .
    canonicalize
  where
    prog = "canonical-trs " ++ showVersion P.version
