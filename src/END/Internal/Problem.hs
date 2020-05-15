{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE RecordWildCards #-}
module END.Internal.Problem (
    -- * Problem solution
    Problem (..),
    ) where

import qualified System.Random.SplitMix as SM

-- | Problems.
--
-- Problem is parameterised by
--
-- * @solution* type for both partial and complete solutions
-- * @metric@ defining the fitness.
--
data Problem metric solution = Problem
    { completeSolution :: SM.SMGen -> solution -> solution
      -- ^ evolve a (possibly partial) solution to complete one.
      --
    , fitness :: solution -> metric
      -- ^ we try to maximise fitness

    , commitment :: Int -> solution -> solution
      -- ^ truncate the problem to some commitment degree

    , initialSolution :: solution
      -- ^ initial (usually empty) partial solution

    , initialCommitment :: Int
      -- ^ initial commitment, usual default is 1

    , maximumFitness :: metric
      -- ^ maximum fitness. Stop if this threshold is reached
      -- (useful if want a solution which is just better than this)
    }

{-
generateSolution
    :: ElemAt f
    => SM.SMGen                         -- ^ random generator
    -> Problem f metric step solution   -- ^ problem definition
    -> solution                         -- ^ (partial) solution
    -> solution                         -- ^ complete solution
generateSolution !g p curr
    | null next = curr
    | otherwise =
        let (w, g') = SM.bitmaskWithRejection32 (fromIntegral (length next)) g
            step    = next `elemAt` fromIntegral w
        in generateSolution g' p (addStep p step curr)
  where
    next = isSolution p curr
-}

-------------------------------------------------------------------------------
-- ElemAt
-------------------------------------------------------------------------------

-- Size: 7
-- Fitness: 16
-- [(2,4),(1,5),(1,3),(3,4),(0,6),(5,6),(0,1),(0,2),(1,3),(2,5),(1,2),(4,6),(3,5),(2,3),(4,5),(3,4)]
-- [(5,6),(0,1),(3,4),(2,3),(4,6),(0,5),(1,3),(0,2),(1,5),(4,5),(1,4),(1,2),(3,6),(2,4),(3,5),(3,4)]

-- Step 4 population best, fitness = -16
-- [(0,1),(3,6),(4,5),(2,3),(3,5),(0,4),(0,2),(4,6),(1,3),(1,2),(3,6),(1,4),(5,6),(3,5),(2,4),(3,4)
-- cabal run search -- +RTS -s -N32 -qg -A128M  165,78s user 2,03s system 2808% cpu 5,976 total
-- cabal run search -- +RTS -s -N32 -qg -A128M  131,03s user 0,88s system 2672% cpu 4,936 total

-- Size: 8
-- Fitness: 19
--
-- Step 12 population best, fitness = -19
-- [(0,3),(1,2),(4,5),(6,7),(0,1),(2,5),(3,7),(4,6),(2,3),(1,6),(3,6),(5,7),(5,6),(0,4),(1,4),(2,4),(3,5),(3,4),(1,2)]
-- DONE
-- cabal run search -- +RTS -s -N32 -qg -A128M  1563,20s user 5,94s system 3053% cpu 51,386 total
-- cabal run search -- +RTS -s -N32 -qg -A128M  1309,94s user 1,98s system 3054% cpu 42,946 total

-- Size: 9
-- Fitness: 25
--
-- Step 23 population best, fitness = -25
-- [(0,1),(3,4),(5,6),(7,8),(0,5),(1,4),(3,7),(6,8),(0,3),(1,6),(4,8),(5,7),(1,2),(3,5),(1,3),(2,5),(4,6),(5,7),(2,3),(4,5),(3,4),(7,8),(6,7),(5,6),(0,1)]
-- DONE
-- cabal run search -- +RTS -s -N32 -qg -A128M  10802,02s user 11,31s system 3109% cpu 5:47,76 total
-- cabal run search -- +RTS -s -N32 -qg -A128M  9255,52s user 4,34s system 3126% cpu 4:56,22 total
--
-- Size: 10
-- Fitness: 29
--
-- Step 22 population best, fitness = -29
-- [(0,6),(1,2),(3,4),(5,8),(7,9),(0,1),(3,5),(4,8),(0,7),(1,4),(2,6),(8,9),(0,3),(2,8),(5,7),(6,9),(1,5),(2,3),(4,8),(6,7),(3,5),(4,6),(7,8),(4,5),(6,7),(1,2),(3,4),(2,3),(5,6)]
-- DONE
--
