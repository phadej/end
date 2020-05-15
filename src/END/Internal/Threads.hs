{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
module END.Internal.Threads (
    Threads,
    initThreads,
    withThreads,
    threadLen,
) where

import Control.Concurrent.Async  (asyncOn, wait)
import Control.Monad.Primitive   (PrimState)
import Data.Foldable             (for_)
import Data.Proxy                (Proxy (..))
import Data.Word                 (Word64)
import GHC.TypeLits

import qualified Data.Primitive.Array   as A
import qualified System.Random.SplitMix as SM

-- cabal build search && time cabal run search -- +RTS -s -N4 -I0 -qn2

-------------------------------------------------------------------------------
-- Threads
-------------------------------------------------------------------------------

newtype Threads (n :: Nat) = T (A.MutableArray (PrimState IO) SM.SMGen)

threadLen :: forall n. KnownNat n => Threads n -> Int
threadLen _ = fromInteger $ natVal (Proxy :: Proxy n)
{-# INLINE threadLen #-}

initThreads :: forall n. KnownNat n => Word64 -> IO (Threads n)
initThreads seed = do
    arr  <- A.newArray size (error "initThreads")
    let loop :: SM.SMGen -> Int -> IO ()
        loop g i | i >= size = return ()
                 | otherwise = do

            -- split the generator
            let (g1, g2) = SM.splitSMGen g
            A.writeArray arr i g1

            loop g2 (i + 1)

    -- run loop
    loop (SM.mkSMGen seed) 0

    return $ T arr
  where
    size :: Int
    size = fromInteger $ natVal (Proxy :: Proxy n)

withThreads
    :: forall n. KnownNat n
    => Threads n                   -- ^ per thread data
    -> (Int -> SM.SMGen -> IO ())  -- ^ function to execute
    -> IO ()
withThreads (T arr) f = do
    asyncs <- A.newArray size (error "withThreads async")

    for_ [0 .. size-1] $ \i -> do
        g <- A.readArray arr i
        let (g1, g2) = SM.splitSMGen g
        A.writeArray arr i g1

        a <- asyncOn i (f i g2)
        A.writeArray asyncs i a

    for_ [0 .. size-1] $ \i -> do
        a <- A.readArray asyncs i
        wait a
    
    -- g0 <- A.readArray arr 0
    -- let (g0', g0'') = SM.splitSMGen g0
    -- A.writeArray arr 0 g0'
    -- a0 <- asyncOn 0 (f 0 g0'')

    -- g1 <- A.readArray arr 1
    -- let (g1', g1'') = SM.splitSMGen g1
    -- A.writeArray arr 1 g1'
    -- a1 <- asyncOn 1 (f 1 g1'')

    -- g2 <- A.readArray arr 2
    -- let (g2', g2'') = SM.splitSMGen g2
    -- A.writeArray arr 2 g2'
    -- a2 <- asyncOn 2 (f 2 g2'')

    -- g3 <- A.readArray arr 3
    -- let (g3', g3'') = SM.splitSMGen g3
    -- A.writeArray arr 3 g3'
    -- a3 <- asyncOn 3 (f 3 g3'')

    -- wait a0
    -- wait a1
    -- wait a2
    -- wait a3

  where
    size :: Int
    size = fromInteger $ natVal (Proxy :: Proxy n)
{-# INLINE withThreads #-}
