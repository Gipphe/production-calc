module Factorio.SpaceExploration.Factories where

import Data.Map.Strict qualified as M
import Relude.Unsafe qualified as Unsafe

import Factorio.SpaceExploration.Processes (Crafter, Item (..))
import Process
    ( Process
    , ProcessSet (..)
    , findProcessSet
    )
import ProductionLine
    ( ProductionLine
    , mkProductionLine
    , reverseProductionLine
    )
import Units (QuantityPerMinute, itemsPerSecond)
import Prelude


type FactorioProductionLine = ProductionLine Item Crafter
type FactorioProcess = Process Item Crafter


steelPlate :: FactorioProductionLine
steelPlate =
    reverseProductionLine
        mempty
        (IronPlate, yellowBelt 0.5)
        SteelPlate


transportBelt :: FactorioProductionLine
transportBelt = mkProductionLine mempty (itemsPerSecond 2) TransportBelt


inserter :: FactorioProductionLine
inserter = mkProductionLine mempty (itemsPerSecond 2) Inserter


getAltRecipe :: Item -> Int -> Map Item FactorioProcess
getAltRecipe i n = M.singleton i $ (findProcessSet i).alt Unsafe.!! n


yellowBelt :: Double -> QuantityPerMinute
yellowBelt = itemsPerSecond . (15 *)
