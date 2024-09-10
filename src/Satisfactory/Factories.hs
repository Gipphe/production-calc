module Satisfactory.Factories where

import Data.Map.Strict qualified as M
import Relude.Unsafe qualified as Unsafe

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
import Satisfactory.Processes (Crafter, Item (..), RawResource (..))
import Units (itemsPerMinute)
import Prelude


type SatisfactoryProductionLine = ProductionLine Item Crafter Crafter
type SatisfactoryProcess = Process Item Crafter


heatsink :: SatisfactoryProductionLine
heatsink =
    reverseProductionLine
        mempty
        (getAltRecipe CopperIngot 0)
        (RawResource Bauxite, itemsPerMinute 1200)
        HeatSink


copperIngot :: SatisfactoryProductionLine
copperIngot =
    reverseProductionLine
        mempty
        (getAltRecipe CopperIngot 0)
        (RawResource CopperOre, itemsPerMinute 1800)
        CopperIngot


coolingSystem :: SatisfactoryProductionLine
coolingSystem =
    mkProductionLine
        mempty
        mempty
        (itemsPerMinute 20)
        CoolingSystem


turboMotor :: SatisfactoryProductionLine
turboMotor =
    mkProductionLine
        mempty
        (getAltRecipe Motor 0)
        (itemsPerMinute 5)
        TurboMotor


reinforcedIronPlate :: SatisfactoryProductionLine
reinforcedIronPlate =
    reverseProductionLine
        mempty
        mempty
        (RawResource IronOre, itemsPerMinute 45)
        ReinforcedIronPlate


customPreferredProcesses :: Map Item SatisfactoryProcess
customPreferredProcesses =
    mconcat $
        uncurry getAltRecipe
            <$> [ (ReinforcedIronPlate, 0)
                , (Wire, 1)
                , (SteelIngot, 0)
                , (CircuitBoard, 0)
                , (Fabric, 0)
                , (Quickwire, 0)
                , (EncasedIndustrialBeam, 0)
                , (Computer, 0)
                ]


getAltRecipe :: Item -> Int -> Map Item SatisfactoryProcess
getAltRecipe i n = M.singleton i $ (findProcessSet i).alt Unsafe.!! n
