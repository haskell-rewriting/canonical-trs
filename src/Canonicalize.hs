{-# LANGUAGE FlexibleContexts #-}

module Canonicalize (
    canonicalize,
) where

import Data.List
import Data.Maybe
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

-- canonicalize a problem
canonicalize :: Problem.Problem String String -> Problem.Problem String String
canonicalize Problem{ theory = Just _ } = error "theory must be empty"
canonicalize p = Problem{
        startTerms = startTerms p, -- :: StartTerms
        strategy   = strategy p,   -- :: Strategy
        theory     = theory p,     -- :: Maybe [Theory f v]
        rules      = rules',       -- :: RulesPair f v
        variables  = vars',        -- :: [v]
        symbols    = symbols',     -- :: [f]
        comment    = Nothing       -- :: Maybe String
    }
  where
    varName = ('v' :) . show
    funName = ('f' :) . show
    (strict, weak) = (map canonicalVars (strictRules (rules p)),
                      map canonicalVars (weakRules (rules p)))
    fpFun = fingerPrintFuns [strict, weak]
    canonical = do
        strict' <- canonicalFunsRs fpFun strict
        weak' <- canonicalFunsRs fpFun weak
        return (strict', weak')
    (strict', weak') = evalState canonical [(M.empty, 0)]
    rules' = RulesPair{ strictRules = map adapt strict',
                        weakRules = map adapt weak' }
    adaptTerm = Term.map funName varName
    adapt rl = Rule{ lhs = adaptTerm (lhs rl), rhs = adaptTerm (rhs rl) }
    vars' = S.toList $ S.fromList $ Rules.vars (Problem.allRules rules')
    symbols' = S.toList $ S.fromList $ Rules.funs (Problem.allRules rules')

-- canonicalize variable names

-- this easy to do on a per rule basis because we don't consider permutations
-- of function arguments
canonicalVars :: Ord v => Rule f v -> Rule f Int
canonicalVars rl = evalState rl' (M.empty, 0)
  where
    rl' = Rule <$> go (lhs rl) <*> go (rhs rl)
    -- auxiliary: given a partial labeling of variables, assign successive
    -- indices to new variables
    go (Var v) = state $ \s@(m, i) -> case M.lookup v m of
        Just j  -> (Var j, s)
        Nothing -> (Var i, (M.insert v i m, i + 1))
    go (Fun f ts) = Fun f <$> mapM go ts

-- canonicalize function symbols

-- auxiliary: given a partial labeling of function symbols, assign successive
-- indices to new function symbols
canonicalFuns :: Ord f =>
    Rule f Int -> State (M.Map f Int, Int) (Rule Int Int)
canonicalFuns rl = rl'
  where
    rl' = Rule <$> go (lhs rl) <*> go (rhs rl)
    go (Var v) = return (Var v)
    go (Fun f ts) = state $ \s@(m, i) -> case M.lookup f m of
        Just j  -> runState (Fun j <$> mapM go ts) s
        Nothing -> runState (Fun i <$> mapM go ts) (M.insert f i m, i + 1)

-- for the canonical representation, we group the rules by fingerprint, and
-- then process all permutations of each group, keeping only the smallest
-- resulting list of rules and the corresponding partial assignments of
-- function symbols to indices
canonicalFunsRs :: Ord f =>
    (f -> Int) -> [Rule f Int] -> State [(M.Map f Int, Int)] [Rule Int Int]
canonicalFunsRs fpF rs = concat <$> mapM go rs1
  where
    rs0 = [(fingerprintRule fpF r, r) | r <- map head . group . sort $ rs]
    rs1 = sortOn length (map (map snd) $ groupBy (\a b -> fst a == fst b) rs0)
    go :: Ord f => [Rule f Int] -> State [(M.Map f Int, Int)] [Rule Int Int]
    go rs = state $ \mjs -> go' [(rs, mj) | mj <- mjs]
    go' :: Ord f => [([Rule f Int], (M.Map f Int, Int))] ->
           ([Rule Int Int], [(M.Map f Int, Int)])
    go' xs@(([], _) : _) = ([], map snd xs)
    go' xs =
        let res = do
                (rs, mj) <- xs
                (r, rs') <- select rs
                let (r', mj') = runState (canonicalFuns r) mj
                return (r', (rs', mj'))
            r' = minimum (map fst res)
            res' = [b | (a, b) <- res, a == r']
            (rs', mjs') = go' res'
        in  (r' : rs', mjs')
{-
    go rs = state $ \mjs ->
        let res = do
                mj <- mjs
                -- we could be more clever here by exploiting the partial
                -- assignment in `mj`... but this is good enough for now.
                rs' <- permutations rs
                return $ runState (mapM canonicalFuns rs') mj
            res' : _ = groupBy (\a b -> fst a == fst b) $ sortOn fst res
        in  (fst (head res'), map snd res')
-}

select :: [a] -> [(a, [a])]
select [] = []
select (x:xs) = (x, xs) : [(y, x:ys) | (y, ys) <- select xs]

-- fingerprinting
--
-- The key property of fingerprints is that they are invariant under the
-- desired symmetries. In our case, these are permutations of strict or weak
-- rules, and renaming function symbols.

-- Given a fingerprinting function for the function symbols, produce a
-- fingerprint of a rule, assuming that variables have already been labeled
-- canonically.
fingerprintRule :: (f -> a) -> Rule.Rule f Int -> (Term a Int, Term a Int)
fingerprintRule f rl = (Term.map f id (lhs rl), Term.map f id (rhs rl))

-- Iteratively produce a fingerprint function for the function symbols.
fingerPrintFuns :: Ord f => [[Rule f Int]] -> f -> Int
fingerPrintFuns trss = go trss (const 0)
  where
    fs = S.toList $ S.fromList $ concat trss >>= Rule.funs
    go trss fp = if length trss == length trss' then fp' else go trss' fp'
      where
        fp' = partitionToMap $ classify (\f -> (fp f, map (fpTrs f) trss)) fs
        fpTrs f = sort . map (fingerprintRule (\g -> (fp g, f == g)))
        trss' = trss >>= classify (fingerprintRule fp')

-- partition a list of objects according to a fingerprint function
classify :: Ord b => (a -> b) -> [a] -> [[a]]
classify f =
    map (map snd) .
    groupBy (\a b -> fst a == fst b) .
    sortBy (\a b -> fst a `compare` fst b) .
    map (\a -> (f a, a))

-- convert partition of objects to fingerprint
partitionToMap :: Ord f => [[f]] -> f -> Int
partitionToMap fss = \f -> fromJust (M.lookup f m)
  where
    m = M.fromList (zip fss [0..] >>= \(fs, i) -> map (\f -> (f, i)) fs)
