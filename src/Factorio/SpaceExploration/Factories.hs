module Factorio.SpaceExploration.Factories where

import Data.Map.Strict qualified as M
import Relude.Unsafe qualified as Unsafe

import Factorio.SpaceExploration.Processes
    ( Crafter (..)
    , CrafterType (..)
    , Furnace (..)
    , Item (..)
    )
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
import Units (QuantityPerMinute, itemsPerMinute, itemsPerSecond)
import Prelude


type FactorioProductionLine = ProductionLine Item CrafterType Crafter


type FactorioProcess = Process Item CrafterType


steelPlate :: FactorioProductionLine
steelPlate =
    reverseProductionLine
        mempty
        mempty
        (IronPlate, yellowBelt 0.5)
        SteelPlate


transportBelt :: FactorioProductionLine
transportBelt = mkProductionLine mempty mempty (itemsPerSecond 2) TransportBelt


inserter :: FactorioProductionLine
inserter = mkProductionLine mempty mempty (itemsPerSecond 1) Inserter


smallElectricMotor :: FactorioProductionLine
smallElectricMotor =
    mkProductionLine mempty mempty (itemsPerMinute 75) SmallElectricMotor


glass :: FactorioProductionLine
glass =
    mkProductionLine
        (M.fromList [(Furnace, FurnaceCrafter SteelFurnace)])
        mempty
        (itemsPerMinute 240)
        Glass


-- Utils

getAltRecipe :: Item -> Int -> Map Item FactorioProcess
getAltRecipe i n =
    M.singleton i $ processSet.alt Unsafe.!! n
  where
    processSet = findProcessSet @Item @CrafterType i


yellowBelt :: Double -> QuantityPerMinute
yellowBelt = itemsPerSecond . (15 *)
