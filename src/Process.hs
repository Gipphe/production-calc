module Process
    ( ProcessSet (..)
    , Process (..)
    , ProcessCollection (..)
    , CrafterMultiplier (..)
    , Input (..)
    , Output (..)
    , ProcessType (..)
    , ProcessIxs
    ) where

import Data.IxSet.Typed (Indexable (..))
import Data.IxSet.Typed qualified as Ix
import Data.Map.Strict qualified as M

import Units (Quantity, Seconds)
import Prelude hiding (Alt (..))


data ProcessSet item crafter = ProcessSet
    { main :: Process item crafter
    , alt :: [Process item crafter]
    }
    deriving (Show)


data Process item crafter = Process
    { output :: Map item Quantity
    , mainFor :: [item]
    , crafter :: crafter
    , cycleTime :: Seconds
    , input :: Map item Quantity
    }
    deriving (Eq, Ord, Show)


class ProcessCollection item crafter where
    findProcessSet :: item -> ProcessSet item crafter


newtype Output item = Output item
    deriving (Eq, Ord)


newtype Input item = Input item
    deriving (Eq, Ord)


data ProcessType item = Main item | Alt item
    deriving (Eq, Ord)


type ProcessIxs item crafter = '[Output item, Input item, ProcessType item]


class CrafterMultiplier crafter where
    crafterMultiplier :: crafter -> Double


instance
    ( Ord crafter
    , Ord item
    )
    => Indexable (ProcessIxs item crafter) (Process item crafter)
    where
    indices =
        Ix.ixList
            (Ix.ixFun (fmap Output . M.keys . output))
            (Ix.ixFun (fmap Input . M.keys . input))
            ( Ix.ixFun
                ( \p ->
                    let
                        mains = mainFor p
                     in
                        (Main <$> mains)
                            <> ( fmap Alt
                                    . filter (not . (`elem` mains))
                                    . M.keys
                                    $ p.output
                               )
                )
            )
