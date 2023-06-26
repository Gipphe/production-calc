module Factories where

import Data.Map.Strict qualified as M
import Relude.Unsafe qualified as Unsafe

import Process
    ( Item (..)
    , Process
    , ProcessSet (..)
    , RawResource (..)
    , findProcessSet
    )
import ProductionLine
    ( ProductionLine
    , machineSummary
    , productionSummary
    , productionTree
    , reverseProductionLine
    )
import Units (itemsPerMinute)
import Prelude


heatsink :: ProductionLine
heatsink =
    reverseProductionLine
        (getAltRecipe CopperIngot 0)
        (RawResource Bauxite, itemsPerMinute 1200)
        HeatSink


heatsinkMachineSummary :: IO ()
heatsinkMachineSummary = putTextLn . machineSummary $ heatsink


heatsinkSummary :: IO ()
heatsinkSummary = putTextLn . productionSummary $ heatsink


heatsinkTree :: IO ()
heatsinkTree = putTextLn . productionTree $ heatsink


copperIngot :: ProductionLine
copperIngot =
    reverseProductionLine
        (getAltRecipe CopperIngot 0)
        (RawResource CopperOre, itemsPerMinute 1800)
        CopperIngot


copperIngotMachineSummary :: IO ()
copperIngotMachineSummary = putTextLn . machineSummary $ copperIngot


copperIngotSummary :: IO ()
copperIngotSummary = putTextLn . productionSummary $ copperIngot


copperIngotTree :: IO ()
copperIngotTree = putTextLn . productionTree $ copperIngot


customPreferredProcesses :: Map Item Process
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


getAltRecipe :: Item -> Int -> Map Item Process
getAltRecipe i n = M.singleton i $ (findProcessSet i).alt Unsafe.!! n
