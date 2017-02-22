module TPDB (
   fromXTCFile,
) where

import qualified TPDB.XTC as XTC
import qualified Data.Rewriting.Problem as Problem
import qualified Data.Rewriting.Rules as Rules
import qualified Data.Rewriting.Problem.Type as R
import qualified Data.Rewriting.Rule.Type as R
import qualified Data.Rewriting.Term.Type as R
import qualified Data.Set as S
import System.FilePath
import Data.Maybe
import Control.Monad

fromXTCFile :: FilePath -> IO (Either String (R.Problem String String))
fromXTCFile fp = problems <$> XTC.readProblems fp

problems [p] = problem p
problems []  = Left "XTC: No problems found."
problems _   = Left "XTC: Multiple problems found."

problem p = do
    -- ignored: XTC.type_
    rules <- trs (XTC.trs p)
    -- below: XTC.strategy
    symbols <- fullSignature (XTC.full_signature p)
    -- below: XTC.startterm
    -- ignored: XTC.attributes
    return $ R.Problem{
            R.startTerms = maybe R.AllTerms startTerm (XTC.startterm p),
            R.strategy   = maybe R.Full strategy (XTC.strategy p),
            R.theory     = Nothing, -- see XTC.Funcsym and XTC.equal_rules
            R.rules      = rules,
            R.variables  = S.toList $ S.fromList $ Rules.vars $ Problem.allRules rules,
            R.symbols    = symbols,
            R.comment    = Nothing
        }

trs t
    | not . null $ XTC.equal_rules t
    = Left "XTC: equational rules found"
    | otherwise
    = return $
        R.RulesPair (rules (XTC.strict_rules t)) (rules (XTC.weak_rules t))

rules = map (\(l, r) -> R.Rule (term l) (term r))

term (XTC.Var v) = R.Var (ident v)
term (XTC.Node f ts) = R.Fun (ident f) (map term ts)

ident = XTC.name

strategy XTC.Full = R.Full
strategy XTC.Innermost = R.Innermost
strategy XTC.Outermost = R.Outermost

fullSignature (XTC.Signature s) = mapM funcsym s
fullSignature _ = Left "XTC: unsupported signature type"

funcsym XTC.Funcsym{ XTC.fs_theory = Just _ } =
    Left "XTC: theory symbols not supported"
funcsym XTC.Funcsym{ XTC.fs_replacementmap = Just _ } =
    Left "XTC: replacement maps not supported"
funcsym f = return $ XTC.fs_name f
    -- note: we lose arity information here

startTerm XTC.Startterm_Full = R.AllTerms
startTerm XTC.Startterm_Constructor_based = R.BasicTerms
