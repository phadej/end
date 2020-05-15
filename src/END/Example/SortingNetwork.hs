{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module END.Example.SortingNetwork (
    -- * Sorting Network Problem
    sortingNetworkProblem,
    -- * Data types
    -- ** Steps
    Step,
    makeStep,
    stepToPair,
    -- ** Solution
    Solution,
    solutionSteps,
    solutionLength,
    solutionDepth,
    solutionEmpty,
    solutionComplete,
    makeSolution,
    solutionAddStep,
    -- ** Metric
    Metric (..),
) where

import Control.DeepSeq   (NFData (..))
import Control.Exception (assert)
import Data.Bits
       (complement, countLeadingZeros, popCount, setBit, shiftL, shiftR, testBit,
       (.&.), (.|.), bit, countTrailingZeros)
import Data.Word         (Word32)

import qualified Data.List                        as L
import qualified Data.Map.Strict                  as Map
import qualified Data.Set                         as Set
import qualified Data.Vector                      as V
import qualified Data.Vector.Unboxed              as UV
import qualified System.Random.SplitMix           as SM

import END

-- $setup
--
-- >>> import Data.Bits (finiteBitSize)
--
-- >>> chunks xs = if length xs <= 8 then xs else let (x,y) = splitAt 8 xs in x ++ ' ' : chunks y
--
-- >>> showBin b = chunks $ reverse $ [ if testBit b i then '1' else '0' | i <- [ 0 .. finiteBitSize b - 1] ]

-------------------------------------------------------------------------------
-- Step
-------------------------------------------------------------------------------

-- | Steps are saved as bitmask in lo and hi 'Word16' parts of 'Word32'.
--
--
type Step        = Word32

-- |
--
-- >>> showBin $ makeStep 0 1
-- "00000000 00000001 00000000 00000010"
--
-- >>> showBin $ makeStep 7 8
-- "00000000 10000000 00000001 00000000"
--
makeStep :: Int -> Int -> Step
makeStep i j = shiftL (bit i) 16 .|. bit j

-- |
--
-- >>> stepToPair (makeStep 0 1)
-- (0,1)
stepToPair :: Step -> (Int, Int)
stepToPair x = (countTrailingZeros hi, countTrailingZeros lo)
  where
    (Mask hi, Mask lo) = stepToMasks x

-------------------------------------------------------------------------------
-- Masks
-------------------------------------------------------------------------------

newtype Mask = Mask Word32
  deriving (Eq, Ord)

stepToMasks :: Step -> (Mask, Mask)
stepToMasks x = (Mask $ x `shiftR` 16, Mask $ x .&. 0xffff)

maskIsSet :: Word32 -> Mask -> Bool
maskIsSet w (Mask mask) = (w .&. mask) /= 0

maskIsNotSet :: Word32 -> Mask -> Bool
maskIsNotSet w (Mask mask) = (w .&. mask) == 0

maskSet :: Word32 -> Mask -> Word32
maskSet w (Mask mask) = w .|. mask

-------------------------------------------------------------------------------
-- Solution
-------------------------------------------------------------------------------

data Solution = Solution
    { solutionSteps  :: !(UV.Vector Step)
    , solutionChunks :: [Chunk] -- chunks are stored in reverse order, last chunk is in the head of the list.
    }
  deriving (Eq, Ord)

instance Show Solution where
    showsPrec d
        = showsPrec d
        . map (\(Chunk _ uv) -> map stepToPair (Set.toList uv))
        . reverse
        . solutionChunks

instance NFData Solution where
    rnf (Solution steps chunks) = rnf steps `seq` rnf chunks

solutionEmpty :: Solution
solutionEmpty = Solution UV.empty []

solutionDepth :: Solution -> Int
solutionDepth (Solution _ chunks) = length chunks

solutionLength :: Solution -> Int
solutionLength (Solution steps _) = UV.length steps

solutionAddStep :: Step -> Solution -> Solution
solutionAddStep x (Solution steps []) =
    assert (UV.null steps) $ Solution (UV.singleton x) [singletonChunk x]
solutionAddStep x (Solution steps cs0@(c:cs)) =
    Solution (UV.snoc steps x) $ case addToChunk x c of
        Just c' -> c' : cs
        Nothing -> singletonChunk x : cs0
{-# INLINE solutionAddStep #-}

makeSolution :: [(Int, Int)] -> Solution
makeSolution = L.foldl' (flip solutionAddStep) solutionEmpty . map (uncurry makeStep)

solutionComplete
    :: Int -- ^ Size
    -> Solution
    -> Bool
solutionComplete size (Solution steps _) =
    forallZO $ \w ->
    isSorted (sortWith steps w)
  where
    forallZO :: (Word32 -> Bool) -> Bool
    forallZO predicate = go 0 where
        go :: Word32 -> Bool
        go !w | w < zeroOneN = predicate w && go (succ w)
              | otherwise    = True

    zeroOneN :: Word32
    zeroOneN = 2 ^ size

-------------------------------------------------------------------------------
-- Chunk
-------------------------------------------------------------------------------

data Chunk = Chunk !Word32 !(Set.Set Step)
  deriving (Eq, Ord)

instance NFData Chunk where
    rnf (Chunk _ uv) = rnf uv

addToChunk :: Step -> Chunk -> Maybe Chunk
addToChunk step (Chunk mask steps)
    | maskIsNotSet mask a
    , maskIsNotSet mask b
    = Just $ Chunk (mask `maskSet` a `maskSet` b) (Set.insert step steps)

    | otherwise
    = Nothing
  where
    (a,b) = stepToMasks step

singletonChunk :: Step -> Chunk
singletonChunk step = Chunk (0 `maskSet` a `maskSet` b) (Set.singleton step)
  where
    (a,b) = stepToMasks step

-------------------------------------------------------------------------------
-- Metric
-------------------------------------------------------------------------------

-- | Metric consist of length *and* solutionDepth.
data Metric = Metric !Int !Int
  deriving (Eq, Show)

instance Ord Metric where
    compare (Metric a b) (Metric c d) = compare c a <> compare d b

instance NFData Metric where
    rnf (Metric _ _) = ()

-- | Sorting Network generation
--
-- https://en.wikipedia.org/wiki/Sorting_network
--
-- >>> completeSolution (sortingNetworkProblem 4 undefined) (SM.mkSMGen 42) (makeSolution [(0,1),(2,3)])
-- [[(0,1),(2,3)],[(0,2)],[(1,2)],[(2,3)],[(1,2)]]
--
-- >>> completeSolution (sortingNetworkProblem 6 undefined) (SM.mkSMGen 42) (makeSolution [(0,1),(2,3),(4,5)])
-- [[(0,1),(2,3),(4,5)],[(0,2)],[(2,4)],[(0,2),(1,4)],[(1,3)],[(3,4)],[(4,5)],[(2,4)],[(1,4)],[(1,2),(3,4)]]
--
sortingNetworkProblem
    :: Int     -- ^ size
    -> Metric  -- ^ optimum
    -> Problem Metric Solution
sortingNetworkProblem size optimum = Problem {..}
  where
    maximumFitness :: Metric
    maximumFitness = optimum

    fitness :: Solution -> Metric
    fitness sol = Metric (solutionLength sol) (solutionDepth sol)

    initialSolution :: Solution
    initialSolution = solutionEmpty

    initialCommitment :: Int
    initialCommitment = 1

    commitment :: Int -> Solution -> Solution
    commitment n (Solution steps chunks) = Solution
        (UV.take (L.foldl' (\ !acc (Chunk _ cs) -> acc + Set.size cs) 0 chunks') steps)
        chunks'
      where
        chunks' = reverse $ take n $ reverse chunks

    -- Evolving solution

    completeSolution :: SM.SMGen -> Solution -> Solution
    completeSolution g sol@(Solution _ [])=
        let (w, g') = SM.bitmaskWithRejection64 (fromIntegral (Set.size comparators0)) g
            step    = Set.elemAt (fromIntegral w) comparators0
        in completeSolution' g' (Set.delete step comparators0) (solutionAddStep step sol)

    completeSolution g sol@(Solution _ (Chunk _ cas : _)) =
        completeSolution' g (comparators0 `Set.difference` cas) sol

    completeSolution' :: SM.SMGen -> Set.Set Step -> Solution -> Solution
    completeSolution' g cands sol@(Solution steps _)
        | V.null candsV' = sol
        | otherwise      =
            let (w, g')  = SM.bitmaskWithRejection64 (fromIntegral (V.length candsV')) g
                step     = candsV' V.! fromIntegral w
            in completeSolution'
                g'
                (Set.delete step $ Set.union cands' $ comparatorsTouching step)
                (solutionAddStep step sol)
      where
        candsV :: V.Vector Step
        candsV = V.fromList (Set.toList cands)

        candsV' :: V.Vector Step
        candsV' = V.ifilter (\i _ -> notRedundant i) candsV

        cands' :: Set.Set Step
        cands' = Set.fromList (V.toList candsV')

        -- TAOCP Exercise 5.3.4.51 (Graham).
        notRedundant :: Int -> Bool
        notRedundant i = testBit mask i

        predicate :: Word32 -> Integer -> Integer
        predicate w mask0
            | isSorted curr = mask0
            | otherwise     = L.foldl' check mask0 [0..V.length candsV - 1]
          where
            curr = sortWith steps w

            check mask' i = case compare x y of
                LT -> mask' `setBit` i -- (2 * i)
                GT -> mask'            -- mask' `setBit` (2 * i + 1)
                EQ -> mask'
              where
                (a,b) = stepToMasks (candsV V.! i)
                x     = maskIsSet curr a
                y     = maskIsSet curr b

        mask :: Integer
        mask = foldZO predicate 0

    comparators0 :: Set.Set Step
    comparators0 = Set.fromList
        [ makeStep i j
        | i <- [0   .. size-2]
        , j <- [i+1 .. size-1]
        ]

    comparatorsTouching :: Step -> Set.Set Step
    comparatorsTouching step = Set.union
        (Map.findWithDefault Set.empty hi comparatorsTouchingMap)
        (Map.findWithDefault Set.empty lo comparatorsTouchingMap)
      where
        (hi, lo) = stepToMasks step

    comparatorsTouchingMap :: Map.Map Mask (Set.Set Step)
    comparatorsTouchingMap = Map.fromListWith Set.union
        [ (Mask $ bit k, Set.singleton (makeStep i j))
        | k <- [0   .. size-1]
        , i <- [0   .. size-2]
        , j <- [i+1 .. size-1]
        , k == i || k == j
        ]

    -- zero-one principle. It's enough to test input bitstrings
    -- 0 .. 2^-size - 1
    -- to see if sorting network is valid.
    zeroOneN :: Word32
    zeroOneN = 2 ^ size

    foldZO :: forall a. (Word32 -> a -> a) -> a -> a
    foldZO f = go 0 where
        go :: Word32 -> a -> a
        go !w acc | w < zeroOneN = go (succ w) (f w acc)
                  | otherwise    = acc
{-# INLINE sortingNetworkProblem #-}

-- @0...01...1@
isSorted :: Word32 -> Bool
isSorted w = countLeadingZeros w + popCount w == 32
{-# INLINE isSorted #-}

sortWith :: UV.Vector Step -> Word32 -> Word32
sortWith uv w
    | UV.null uv = w
    | otherwise  = sortWith (UV.unsafeTail uv) (compareAndSwap (UV.unsafeHead uv) w)
{-# INLINE sortWith #-}

compareAndSwap :: Step -> Word32 -> Word32
compareAndSwap step w = case ((w .&. i) /= 0, (w .&. j) /= 0) of
    (False, True)  -> (w .|. i) .&. complement j
    (False, False) -> w
    (True,  True)  -> w
    (True,  False) -> w
  where
    i = step `shiftR` 16
    j = step .&. 0xffff
{-# INLINE compareAndSwap #-}
