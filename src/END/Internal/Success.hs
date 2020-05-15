module END.Internal.Success where

import Control.Exception (Exception)
import Data.Typeable     (Typeable)

-- | We throw success exception :)
newtype Success solution = Success solution
  deriving (Show)

instance (Show solution, Typeable solution) => Exception (Success solution)
