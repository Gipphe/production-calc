module Prelude
    ( module Relude
    , Eff
    , Effect
    , InterpreterFor
    , (:>)
    , HasField (..)
    ) where

import Cleff (Eff, Effect, (:>))
import GHC.Records (HasField (..))
import Relude hiding
    ( State
    , evalState
    , execState
    , fromException
    , get
    , gets
    , modify
    , modify'
    , put
    , runState
    , state
    , trace
    )


type InterpreterFor e es = forall a. Eff (e ': es) a -> Eff es a
