{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveTraversable    #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module END.Internal.Config where

import Data.Kind    (Constraint, Type)
import GHC.TypeLits

import END.Internal.Population (Neighborhood)

-- | Configuration
--
-- * @threads@ amount of threads
-- * @gridside@ we work on square grid.
--   Note: we assume that @gridsize@ is power of 2.
-- * disorder @threshold@ for commitment degree to auto-increase
-- * maximum @eralength@ before commitment degree is forcefully increased
-- * and total @maxsteps@
-- * @neighborhood@ specifies how the species spread.
--
data Config
    (threads      :: Nat)
    (gridside     :: Nat)
    (threshold    :: Nat)
    (eralength    :: Nat)
    (maxsteps     :: Nat)
    (neighborhood :: Type -> Type)
    = Config

-- | Not all 'Config' are valid. 'ValidConfig' are.
type ValidConfig threads gridside threshold eralength maxsteps neighborhood =
    ( KnownNat threads
    , KnownNat gridside
    , KnownNat threshold
    , KnownNat eralength
    , KnownNat maxsteps
    , GridSideIsPowerOf2 gridside
    , ThreadsDivideGridSize threads gridside
    , Neighborhood neighborhood
    )

-------------------------------------------------------------------------------
-- Threshold
-------------------------------------------------------------------------------

-- | A helper type-alias to calculate threshold based on
-- gridside and percentage.
type Threshold (gridside :: Nat) (percent :: Nat)
    = Div ((gridside GHC.TypeLits.* gridside) GHC.TypeLits.* percent) 50
    -- note we divide by 50, so we don't need to multiply by 2.
    -- The total disorder is @gridsize * gridsize * 2@.

-------------------------------------------------------------------------------
-- Errors
-------------------------------------------------------------------------------

type family GridSideIsPowerOf2 (gridside :: Nat) :: Constraint where
    GridSideIsPowerOf2 gridside =
        GridSideIsPowerOf2' gridside (2 GHC.TypeLits.^ (Log2 gridside))

type family GridSideIsPowerOf2' (gridside :: Nat) (r :: Nat) :: Constraint where
    GridSideIsPowerOf2' x        x = ()
    GridSideIsPowerOf2' gridsize _ = TypeError
        ('Text "Gridside " ':<>: 'ShowType gridsize ':<>: 'Text " is not power of 2")
      

type family ThreadsDivideGridSize (threads :: Nat) (gridside :: Nat) :: Constraint where
    ThreadsDivideGridSize threads gridsize =
        ThreadsDivideGridSize' threads gridsize (Mod (gridsize GHC.TypeLits.* gridsize) threads)

type family ThreadsDivideGridSize' (threads :: Nat) (gridside :: Nat) (r :: Nat) :: Constraint where
    ThreadsDivideGridSize' _ _ 0 = ()
    ThreadsDivideGridSize' threads gridsize r = TypeError
        ('Text "Threads don't divide grid "
         ':$$: 'Text "(" ':<>: 'ShowType gridsize ':<>: 'Text " * " ':<>:'ShowType gridsize
         ':<>: 'Text ") `mod` " ':<>: 'ShowType threads
         ':<>: 'Text " = " ':<>: 'ShowType r
        )
