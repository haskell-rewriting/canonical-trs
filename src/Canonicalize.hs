{-# LANGUAGE FlexibleContexts #-}

module Canonicalize (
    canonicalize,
) where

import Data.List
import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Rewriting.Problem as Problem
import Data.Rewriting.Problem (Problem(..), RulesPair(..))
import qualified Data.Rewriting.Rules as Rules
import qualified Data.Rewriting.Rule as Rule
import Data.Rewriting.Rule (Rule(..))
import qualified Data.Rewriting.Term as Term
import Data.Rewriting.Term (Term(..))

canonicalize :: Problem.Problem String String -> Problem.Problem String String
canonicalize p = Problem{
        startTerms = startTerms p, -- :: StartTerms
        strategy   = strategy p,   -- :: Strategy
        theory     = theory p,     -- :: Maybe [Theory f v]
        rules      = rules',       -- :: RulesPair f v
        variables  = vars',        -- :: [v]
        symbols    = symbols',     -- :: [f]
        comment    = Nothing       -- :: Maybe String
    } where
    varName = ('v' :) . show
    funName = ('f' :) . show
    (strict, weak) = (strictRules (rules p), weakRules (rules p))
    canonical = do
        strict' <- canonicalFunsRs (map canonicalVars strict)
        weak' <- canonicalFunsRs (map  canonicalVars weak)
        return (strict', weak')
    (strict', weak') = evalState canonical [(M.empty, 0)]
    rules' = RulesPair{ strictRules = map adapt strict', weakRules = map adapt weak' }
    adaptTerm = Term.map funName varName
    adapt rl = Rule{ lhs = adaptTerm (lhs rl), rhs = adaptTerm (rhs rl) }
    vars' = S.toList $ S.fromList $ Rules.vars (Problem.allRules rules')
    symbols' = S.toList $ S.fromList $ Rules.funs (Problem.allRules rules')

canonicalVars :: Ord v => Rule.Rule f v -> Rule.Rule f Int
canonicalVars rl = evalState rl' (M.empty, 0) where
    rl' = Rule <$> go (lhs rl) <*> go (rhs rl)
    go (Var v) = state $ \s@(m, i) -> case M.lookup v m of
        Just j  -> (Var j, s)
        Nothing -> (Var i, (M.insert v i m, i + 1))
    go (Fun f ts) = Fun f <$> mapM go ts

canonicalFuns :: Ord f => Rule.Rule f Int -> State (M.Map f Int, Int) (Rule.Rule Int Int)
canonicalFuns rl = rl' where
    rl' = Rule <$> go (lhs rl) <*> go (rhs rl)
    go (Var v) = return (Var v)
    go (Fun f ts) = state $ \s@(m, i) -> case M.lookup f m of
        Just j  -> runState (Fun j <$> mapM go ts) s
        Nothing -> runState (Fun i <$> mapM go ts) (M.insert f i m, i + 1)

canonicalFunsRs :: Ord f => [Rule f Int] -> State [(M.Map f Int, Int)] [Rule Int Int]
canonicalFunsRs rs = concat <$> mapM go rs1 where
    rs0 = [(fingerprintRule r, r) | r <- map head . group . sort $ rs]
    rs1 = sortOn length (map (map snd) $ groupBy (\a b -> fst a == fst b) rs0)
    go :: Ord f => [Rule f Int] -> State [(M.Map f Int, Int)] [Rule Int Int]
    go rs = state $ \mjs ->
        let res = do
                mj <- mjs
                rs' <- take 10000 $ permutations rs
                return $ runState (mapM canonicalFuns rs') mj
            res' : _ = groupBy (\a b -> fst a == fst b) $ sortOn fst res
        in  (fst (head res'), map snd res')

fingerprintRule :: Rule.Rule f Int -> (Int, Int, Int, Int)
fingerprintRule rl = (size (lhs rl), size (rhs rl), fpTerm (lhs rl), fpTerm (rhs rl)) where
    size :: Term f v -> Int
    size = Term.fold (\_ -> 1) (\_ ts -> 1 + sum ts)
    fpTerm :: Term f Int -> Int
    fpTerm = Term.fold (\n -> 7^n) (\_ ts -> foldl (\s r -> 11*s + r) 13 ts)
