{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
module END.Internal.Population (
    -- * Population
    Population (..),
    initPopulation,
    mapPopulation,
    -- * Neighborhood
    Neighborhood (..),
    Neighbors1,
    NeighborsMax1,
    NeighborsMax2,
    -- * AdjacentPairs
    AdjacentPairs (..),
    mapWithAdjacentPairs,
) where

import Control.DeepSeq              (NFData, force)
import Control.Exception            (evaluate)
import Data.Bits                    (shiftL, shiftR, (.&.), (.|.))
import Data.Proxy                   (Proxy (..))
import GHC.TypeLits                 (KnownNat, Nat, natVal)
import Math.NumberTheory.Logarithms (intLog2)

import qualified Data.Primitive.Array   as A
import qualified System.Random.SplitMix as SM

import END.Internal.Threads

-------------------------------------------------------------------------------
-- Population size
-------------------------------------------------------------------------------

-- | Population of size size
newtype Population (n :: Nat) a = P (A.Array a)
  deriving (Show, Foldable)

initPopulation
    :: forall m n b. (KnownNat n, KnownNat m, NFData b)
    => Threads n
    -> (SM.SMGen -> IO (SM.SMGen, b))
    -> IO (Population m b)
initPopulation threads f = do
    let size :: Int
        size = sq $ fromInteger $ natVal (Proxy :: Proxy m)

    ma <- A.newArray size (error "initPopulation: result")
    let shard = size `div` threadLen threads
    withThreads threads $ \tid a0 -> do
        let lo = tid * shard
            hi = lo + shard

        let loop i a | i >= hi = return a
                     | otherwise = do
                (a', b) <- f a >>= evaluate . force

                A.writeArray ma i b

                loop (i + 1) a'

        _ <- loop lo a0
        return ()

    a <- A.unsafeFreezeArray ma
    return (P a)
{-# INLINE initPopulation #-}

mapPopulation
    :: forall n m a b. (KnownNat n, KnownNat m, NFData b)
    => Threads n
    -> Population m a
    -> (SM.SMGen -> a -> IO (SM.SMGen, b))
    -> IO (Population m b)
mapPopulation threads (P arr0) f = do
    let size :: Int
        size = sq $ fromInteger $ natVal (Proxy :: Proxy m)

    ma <- A.newArray size (error "mapPopulation: result")
    let shard = size `div` threadLen threads
    withThreads threads $ \tid t0 -> do
        let lo = tid * shard
            hi = lo + shard

        let localCopy = A.cloneArray arr0 0 size
        localArray <- A.newArray shard (error "mapPopulation: local")

        let loop i t | i >= hi = return t
                     | otherwise = do
                a <- A.indexArrayM localCopy i
                (t', b) <- f t a >>= evaluate . force
                A.writeArray localArray (i - lo) b
                loop (i + 1) t'

        t' <- loop lo t0
        A.copyMutableArray ma lo localArray 0 shard
        _ <- evaluate (force t')
        return ()

    a <- A.unsafeFreezeArray ma
    return (P a)
{-# INLINE mapPopulation #-}

-------------------------------------------------------------------------------
-- AdjacentPairs
-------------------------------------------------------------------------------

data AdjacentPairs a = AP a a a

mapWithAdjacentPairs
    :: forall n m a b. (KnownNat n, KnownNat m, NFData b)
    => Threads n
    -> Population m a
    -> (SM.SMGen -> AdjacentPairs a -> IO (SM.SMGen, b))
    -> IO (Population m b)
mapWithAdjacentPairs threads (P arr0) f = do
    let sidesize :: Int
        sidesize = fromInteger $ natVal (Proxy :: Proxy m)

        -- we know that sidesize is power of 2
        sidebits :: Int
        sidebits = intLog2 sidesize

        mask :: Int
        mask = sidesize - 1

        fullsize :: Int
        fullsize = sq sidesize

        move :: Int  -- ^ x-advancement
             -> Int  -- ^ y-advancement
             -> Int  -- ^ index
             -> Int  -- ^ moved index
        move xd yd i = x' .|. (y' `shiftL` sidebits)
          where
            x = i .&. mask
            y = (i `shiftR` sidebits) .&. mask

            x' = (x + xd) .&. mask
            y' = (y + yd) .&. mask

    ma <- A.newArray fullsize undefined
    let shard = fullsize `div` threadLen threads
    withThreads threads $ \tid t0 -> do
        let lo = tid * shard
            hi = lo + shard

        let localCopy = A.cloneArray arr0 0 fullsize
        localArray <- A.newArray shard undefined

        let loop i t | i >= hi = return t
                     | otherwise = do
                (t', b) <- (evaluate . force =<<) $ f t $ AP
                      (A.indexArray localCopy i)
                      (A.indexArray localCopy $ move 0 1 i)
                      (A.indexArray localCopy $ move 1 0 i)

                A.writeArray localArray (i - lo) b

                loop (i + 1) t'

        t' <- loop lo t0
        A.copyMutableArray ma lo localArray 0 shard

        _ <- evaluate (force t')
        return ()

    a <- A.unsafeFreezeArray ma
    return (P a)
{-# INLINE mapWithAdjacentPairs #-}

-------------------------------------------------------------------------------
-- Neighborhood
-------------------------------------------------------------------------------

class Neighborhood o where
    mapWithNeighbors
        :: forall n m a b. (KnownNat n, KnownNat m, NFData b)
        => Threads n
        -> Population m a
        -> (SM.SMGen -> o a -> IO (SM.SMGen, b))
        -> IO (Population m b)

    neighborMaximumBy :: (a -> a -> Ordering) -> o a -> a

instance Neighborhood Neighbors1    where
    mapWithNeighbors  = mapWithNeighbors1
    neighborMaximumBy = neighborMaximumBy1
instance Neighborhood NeighborsMax1 where
    mapWithNeighbors  = mapWithNeighborsMax1
    neighborMaximumBy = neighborMaximumByMax1
instance Neighborhood NeighborsMax2 where
    mapWithNeighbors  = mapWithNeighborsMax2
    neighborMaximumBy = neighborMaximumByMax2

-------------------------------------------------------------------------------
-- Neighbors: 1
-------------------------------------------------------------------------------

-- | Neighbors with distance \( |x - x'| + |y - y'| \le 1 \)
-- i.e. 4 elements.
data Neighbors1 a = N1 a a a a a

mapWithNeighbors1
    :: forall n m a b. (KnownNat n, KnownNat m, NFData b)
    => Threads n
    -> Population m a
    -> (SM.SMGen -> Neighbors1 a -> IO (SM.SMGen, b))
    -> IO (Population m b)
mapWithNeighbors1 threads (P arr0) f = do
    let sidesize :: Int
        sidesize = fromInteger $ natVal (Proxy :: Proxy m)

        -- we know that sidesize is power of 2
        sidebits :: Int
        sidebits = intLog2 sidesize

        mask :: Int
        mask = sidesize - 1

        fullsize :: Int
        fullsize = sq sidesize

        move :: Int  -- ^ x-advancement
             -> Int  -- ^ y-advancement
             -> Int  -- ^ index
             -> Int  -- ^ moved index
        move xd yd i = x' .|. (y' `shiftL` sidebits)
          where
            x = i .&. mask
            y = (i `shiftR` sidebits) .&. mask

            x' = (x + xd) .&. mask
            y' = (y + yd) .&. mask

    ma <- A.newArray fullsize (error "mapWithNeighbors1: result")
    let shard = fullsize `div` threadLen threads
    withThreads threads $ \tid t0 -> do
        let lo = tid * shard
            hi = lo + shard

        let localCopy = A.cloneArray arr0 0 fullsize
        localArray <- A.newArray shard (error "mapWithNeighbors1: local")

        let loop i t | i >= hi = return t
                     | otherwise = do
                (t', b) <- (evaluate . force =<<) $ f t $ N1
                      (A.indexArray localCopy i)
                      (A.indexArray localCopy $ move 0    1    i)
                      (A.indexArray localCopy $ move 0    (-1) i)
                      (A.indexArray localCopy $ move 1    0    i)
                      (A.indexArray localCopy $ move (-1) 0    i)

                A.writeArray localArray (i - lo) b

                loop (i + 1) t'

        t' <- loop lo t0
        A.copyMutableArray ma lo localArray 0 shard

        _ <- evaluate (force t')
        return ()

    a <- A.unsafeFreezeArray ma
    return (P a)
{-# INLINE mapWithNeighbors1 #-}

neighborMaximumBy1 :: (a -> a -> Ordering) -> Neighbors1 a -> a
neighborMaximumBy1 f (N1 x0 x1 x2 x3 x4) = bestOf5 f x0 x1 x2 x3 x4

bestOf3
    :: (a -> a -> Ordering)
    -> a -> a -> a
    -> a
bestOf3 f x0 x1 x2 = x0 `pick` x1 `pick` x2 where
    pick !a !b = case f a b of
        LT -> b
        GT -> a
        EQ -> a
{-# INLINE bestOf3 #-}

bestOf5
    :: (a -> a -> Ordering)
    -> a -> a -> a -> a -> a
    -> a
bestOf5 f x0 x1 x2 x3 x4 = x0 `pick` x1 `pick` x2 `pick` x3 `pick` x4 where
    pick !a !b = case f a b of
        LT -> b
        GT -> a
        EQ -> a
{-# INLINE bestOf5 #-}

-------------------------------------------------------------------------------
-- Neighbors: 2
-------------------------------------------------------------------------------

-- | Neighbors with distance \( \max(|x - x'|, |y - y'|) \le 1 \)
-- i.e. 9 elements.
data NeighborsMax1 a = NM1 a a a
                       a a a
                       a a a

mapWithNeighborsMax1
    :: forall n m a b. (KnownNat n, KnownNat m, NFData b)
    => Threads n
    -> Population m a
    -> (SM.SMGen -> NeighborsMax1 a -> IO (SM.SMGen, b))
    -> IO (Population m b)
mapWithNeighborsMax1 threads (P arr0) f = do
    let sidesize :: Int
        sidesize = fromInteger $ natVal (Proxy :: Proxy m)

        -- we know that sidesize is power of 2
        sidebits :: Int
        sidebits = intLog2 sidesize

        mask :: Int
        mask = sidesize - 1

        fullsize :: Int
        fullsize = sq sidesize

        move :: Int  -- ^ x-advancement
             -> Int  -- ^ y-advancement
             -> Int  -- ^ index
             -> Int  -- ^ moved index
        move xd yd i = x' .|. (y' `shiftL` sidebits)
          where
            x = i .&. mask
            y = (i `shiftR` sidebits) .&. mask

            x' = (x + xd) .&. mask
            y' = (y + yd) .&. mask

    ma <- A.newArray fullsize (error "mapWithNeighborsMax1: result")
    let shard = fullsize `div` threadLen threads

    withThreads threads $ \tid t0 -> do
        let lo = tid * shard
            hi = lo + shard

        let localCopy = A.cloneArray arr0 0 fullsize
        localArray <- A.newArray shard (error "mapWithNeighborsMax1: local")

        let loop i t | i >= hi = return t
                     | otherwise = do
                (t', b) <- (evaluate . force =<<) $ f t $ NM1
                      (A.indexArray localCopy i)
                      (A.indexArray localCopy $ move (-1) (-1) i)
                      (A.indexArray localCopy $ move (-1)   0  i)
                      (A.indexArray localCopy $ move (-1)   1  i)
                      (A.indexArray localCopy $ move   0  (-1) i)
                      -- (A.indexArray localCopy $ move   0    0  i)
                      (A.indexArray localCopy $ move   0    1  i)
                      (A.indexArray localCopy $ move   1  (-1) i)
                      (A.indexArray localCopy $ move   1    0  i)
                      (A.indexArray localCopy $ move   1    1  i)

                A.writeArray localArray (i - lo) b

                loop (i + 1) t'

        t' <- loop lo t0
        A.copyMutableArray ma lo localArray 0 shard

        _ <- evaluate (force t')
        return ()

    a <- A.unsafeFreezeArray ma
    return (P a)
{-# INLINE mapWithNeighborsMax1 #-}

neighborMaximumByMax1 :: (a -> a -> Ordering) -> NeighborsMax1 a -> a
neighborMaximumByMax1 f (NM1 x0 x1 x2
                             y0 y1 y2
                             z0 z1 z2) =
    bestOf3 f (bestOf3 f x0 x1 x2)
              (bestOf3 f y0 y1 y2)
              (bestOf3 f z0 z1 z2)

-------------------------------------------------------------------------------
-- Neighbors: 4
-------------------------------------------------------------------------------

-- | Neighbors with distance \( \max(|x - x'|, |y - y'|) \le 2 \)
-- i.e 25 elements.
data NeighborsMax2 a = NM2 a a a a a
                       a a a a a
                       a a a a a
                       a a a a a
                       a a a a a

mapWithNeighborsMax2
    :: forall n m a b. (KnownNat n, KnownNat m, NFData b)
    => Threads n
    -> Population m a
    -> (SM.SMGen -> NeighborsMax2 a -> IO (SM.SMGen, b))
    -> IO (Population m b)
mapWithNeighborsMax2 threads (P arr0) f = do
    let sidesize :: Int
        sidesize = fromInteger $ natVal (Proxy :: Proxy m)

        -- we know that sidesize is power of 2
        sidebits :: Int
        sidebits = intLog2 sidesize

        mask :: Int
        mask = sidesize - 1

        fullsize :: Int
        fullsize = sq sidesize

        move :: Int  -- ^ x-advancement
             -> Int  -- ^ y-advancement
             -> Int  -- ^ index
             -> Int  -- ^ moved index
        move xd yd i = x' .|. (y' `shiftL` sidebits)
          where
            x = i .&. mask
            y = (i `shiftR` sidebits) .&. mask

            x' = (x + xd) .&. mask
            y' = (y + yd) .&. mask

    ma <- A.newArray fullsize (error "mapWithNeighborsMax2: result")
    let shard = fullsize `div` threadLen threads
    withThreads threads $ \tid t0 -> do
        let lo = tid * shard
            hi = lo + shard

        let localCopy = A.cloneArray arr0 0 fullsize
        localArray <- A.newArray shard (error "mapWithNeighborsMax2: local")

        let loop i t | i >= hi = return t
                     | otherwise = do
                (t', b) <- (evaluate . force =<<) $ f t $ NM2
                      (A.indexArray localCopy i)
                      (A.indexArray localCopy $ move (-2) (-2) i)
                      (A.indexArray localCopy $ move (-2) (-1) i)
                      (A.indexArray localCopy $ move (-2)   0  i)
                      (A.indexArray localCopy $ move (-2)   1  i)
                      (A.indexArray localCopy $ move (-2)   2  i)
                      (A.indexArray localCopy $ move (-1) (-2) i)
                      (A.indexArray localCopy $ move (-1) (-1) i)
                      (A.indexArray localCopy $ move (-1)   0  i)
                      (A.indexArray localCopy $ move (-1)   1  i)
                      (A.indexArray localCopy $ move (-1)   2  i)
                      (A.indexArray localCopy $ move   0  (-2) i)
                      (A.indexArray localCopy $ move   0  (-1) i)
                      -- (A.indexArray localCopy $ move   0    0  i)
                      (A.indexArray localCopy $ move   0    1  i)
                      (A.indexArray localCopy $ move   0    2  i)
                      (A.indexArray localCopy $ move   1  (-2) i)
                      (A.indexArray localCopy $ move   1  (-1) i)
                      (A.indexArray localCopy $ move   1    0  i)
                      (A.indexArray localCopy $ move   1    1  i)
                      (A.indexArray localCopy $ move   1    2  i)
                      (A.indexArray localCopy $ move   2  (-2) i)
                      (A.indexArray localCopy $ move   2  (-1) i)
                      (A.indexArray localCopy $ move   2    0  i)
                      (A.indexArray localCopy $ move   2    1  i)
                      (A.indexArray localCopy $ move   2    2  i)

                A.writeArray localArray (i - lo) b

                loop (i + 1) t'

        t' <- loop lo t0
        A.copyMutableArray ma lo localArray 0 shard

        _ <- evaluate (force t')
        return ()

    a <- A.unsafeFreezeArray ma
    return (P a)
{-# INLINE mapWithNeighborsMax2 #-}

neighborMaximumByMax2 :: (a -> a -> Ordering) -> NeighborsMax2 a -> a
neighborMaximumByMax2 f (NM2 x0 x1 x2 x3 x4
                             y0 y1 y2 y3 y4
                             z0 z1 z2 z3 z4
                             u0 u1 u2 u3 u4
                             v0 v1 v2 v3 v4) =
    bestOf5 f (bestOf5 f x0 x1 x2 x3 x4)
              (bestOf5 f y0 y1 y2 y3 y4)
              (bestOf5 f z0 z1 z2 z3 z4)
              (bestOf5 f u0 u1 u2 u3 u4)
              (bestOf5 f v0 v1 v2 v3 v4)

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

sq :: Int -> Int
sq x = x * x
{-# INLINE sq #-}
