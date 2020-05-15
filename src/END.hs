{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module END (
    -- * Evolving Non-Determism
    evolvingNonDetermism,
    -- * Configuration
    Config (..),
    Threshold,
    ValidConfig,
    -- ** Neighborhood
    Neighborhood,
    Neighbors1,
    NeighborsMax1,
    NeighborsMax2,
    -- * Problem specification
    Problem (..),
    ) where

import Control.DeepSeq   (NFData (..))
import Control.Exception (handle, throwIO)
import Control.Monad     (when)
import Data.Foldable     (foldl', maximumBy, minimumBy)
import Data.Ord          (comparing)
import Data.Proxy        (Proxy (..))
import Data.Typeable     (Typeable)
import Data.Word         (Word64)
import GHC.TypeLits      (natVal)
import Numeric           (showFFloat)
import System.Clock      (Clock (Monotonic), getTime, toNanoSecs, diffTimeSpec)

import qualified System.Random.SplitMix as SM

import END.Internal.Config
import END.Internal.Population
import END.Internal.Problem
import END.Internal.Success
import END.Internal.Threads

-- | Try to find a solution.
--
evolvingNonDetermism
    :: forall threads gridsidep threshold eralength maxsteps neighborhood metric solution.
        (ValidConfig
              threads gridsidep threshold eralength maxsteps neighborhood
       , NFData solution, Show solution, Eq solution, Typeable solution
       , NFData metric,   Show metric,   Ord metric
       )
    => Config threads gridsidep threshold eralength maxsteps neighborhood
    -> Word64 -- ^ seed
    -> Problem metric solution
    -> IO solution
evolvingNonDetermism _ seed problem = handle (\(Success solution) -> return solution) $ do
    threads <- initThreads seed :: IO (Threads threads)

    let threshold :: Int
        threshold = fromInteger (natVal (Proxy @threshold))

        gridsidep :: Int
        gridsidep = fromInteger (natVal (Proxy @gridsidep))

        maxsteps :: Int
        maxsteps = fromInteger (natVal (Proxy @maxsteps))

        eralength :: Int
        eralength = fromInteger (natVal (Proxy @eralength))

        maxDisorder :: Int
        maxDisorder = 2 * (gridsidep * gridsidep)

    putStrLn "Generating initial population"

    initial <- initPopulation @gridsidep threads $ \g -> do
        let (g1, g2) = SM.splitSMGen g
        let s        = completeSolution problem g1 (initialSolution problem)
        let !f       = fitness problem s
        return (g2, (f, s))

    putStrLn $ "Initial population worst, fitness = " ++ show (fst (minimumBy (comparing fst) initial))
    let best0 = maximumBy (comparing fst) initial
    putStrLn $ "Initial population best, fitness = " ++ show (fst best0)
    print $ snd best0

    (final, _, _) <- forN maxsteps (initial, initialCommitment problem, 1) $ \iter (sol, comm, cstep) -> do
        putStrLn ""
        putStrLn $ "Step " ++ show iter ++ " " ++ show (comm,cstep)

        sol1 <- mapWithNeighbors @neighborhood threads sol $ \g ns -> do
            let organism = snd $ neighborMaximumBy (comparing fst) ns
            let species  = commitment problem comm organism

            when (organism == species) $ do
                putStrLn $ "Maximum commitment: " ++ show species
                throwIO $ Success species

            return (g, species)

        -- Print commitment data: used to inspect disorder metric.
        -- when (iter `elem` [2,5,25,50]) $ do
        --     print sol1

        -- calculate disorder
        dis <- mapWithAdjacentPairs threads sol1 $ \g (AP x y z) -> do
            let xy = if x == y then 0 else 1
                xz = if x == z then 0 else 1
            return (g, xy + xz)

        let totalDisorder = foldl' (+) 0 dis

        -- calculate distribution
        -- let stats' = Map.fromListWith (+)
        --         [ (s, 1 :: Int)
        --         | s <- toList sol1
        --         ]

        -- let stats = maximum stats'

        putStrLn $ unwords
            [ "Disorder"
            , show totalDisorder
            , "(" ++ showFFloat (Just 2) (100 * fromIntegral totalDisorder / fromIntegral maxDisorder :: Float) "%)"
            ]

        (comm', cstep') <-
            if cstep >= eralength then do
                putStrLn "Maximum era length reached: increasing commitment"
                return (comm + 1, 1)
            else if totalDisorder < threshold then do
                putStrLn "Disorder threshold reached: increasing commitment"
                return (comm + 1, 1)

            else return (comm, cstep + 1)

        putStr "Generating solutions... "
        gstart <- getTime Monotonic
        sol2 <- mapPopulation threads sol1 $ \g species -> do
            let (g1, g2) = SM.splitSMGen g
            let s        = completeSolution problem g1 species
            let !f       = fitness problem s
            return (g2, (f, s))
        gend <- getTime Monotonic
        putStrLn $ showFFloat (Just 2)
            (fromInteger (toNanoSecs (diffTimeSpec gend gstart)) / 1e9 :: Double)
            "s"

        let best = maximumBy (comparing fst) sol2
        putStrLn $ "Step " ++ show iter ++ " population best, fitness = " ++ show (fst best)
        print $ snd best

        when (fst best >= maximumFitness problem) $ do
            putStrLn "Reached maximum fitness value"
            print $ snd best
            throwIO $ Success $ snd best

        return (sol2, comm', cstep')

    let best = maximumBy (comparing fst) final
    return $ snd best

forN :: Monad m => Int -> a -> (Int -> a -> m a) -> m a
forN n x f = go 0 x where
    go !i y | i >= n = return y
           | otherwise = do
        z <- f i y
        go (i + 1) z
