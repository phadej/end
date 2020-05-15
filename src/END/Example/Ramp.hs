{-# LANGUAGE RecordWildCards #-}
module END.Example.Ramp (
    rampProblem,
) where

import qualified Data.Set               as Set
import qualified System.Random.SplitMix as SM

import END

rampProblem :: Int -> Problem Int [Int]
rampProblem maxN = Problem {..} where
    fullnodes = Set.fromList [0..maxN]

    completeSolution :: SM.SMGen -> [Int] -> [Int]
    completeSolution g0 xs0 =
        go g0 (fullnodes `Set.difference` Set.fromList xs0)
      where
        go :: SM.SMGen -> Set.Set Int -> [Int]
        go g rest
            | Set.null rest = xs0
            | otherwise     =
                let (w, g') = SM.bitmaskWithRejection64 (fromIntegral (Set.size rest)) g
                    x       = Set.elemAt (fromIntegral w) rest
                in x : go g' (Set.delete x rest)

    fitness xs = length $ filter (uncurry (>)) $ pairs xs

    commitment n      = reverse . take n . reverse
    initialSolution   = []
    initialCommitment = 1
    maximumFitness    = maxN

pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)
