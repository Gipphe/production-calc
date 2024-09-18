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
        customPreferredProcesses
        (RawResource IronOre, itemsPerMinute 90)
        ReinforcedIronPlate


smartPlating :: SatisfactoryProductionLine
smartPlating =
    reverseProductionLine
        mempty
        mempty
        (RawResource IronOre, itemsPerMinute 46)
        SmartPlating


rotor :: SatisfactoryProductionLine
rotor =
    reverseProductionLine
        mempty
        customPreferredProcesses
        (RawResource IronOre, itemsPerMinute 45)
        Rotor


copperSheet :: SatisfactoryProductionLine
copperSheet =
    reverseProductionLine
        mempty
        mempty
        (RawResource CopperOre, itemsPerMinute 60)
        CopperSheet


modularFrame :: SatisfactoryProductionLine
modularFrame =
    reverseProductionLine
        mempty
        customPreferredProcesses
        (RawResource IronOre, itemsPerMinute (120 / 4 * 3))
        ModularFrame


silica :: SatisfactoryProductionLine
silica =
    reverseProductionLine
        mempty
        customPreferredProcesses
        (RawResource RawQuartz, itemsPerMinute 120)
        Silica


quartzCrystal :: SatisfactoryProductionLine
quartzCrystal =
    reverseProductionLine
        mempty
        customPreferredProcesses
        (RawResource RawQuartz, itemsPerMinute 120)
        QuartzCrystal


steelBeam :: SatisfactoryProductionLine
steelBeam =
    reverseProductionLine
        mempty
        customPreferredProcesses
        (RawResource IronOre, itemsPerMinute 240)
        SteelBeam


steelPipe :: SatisfactoryProductionLine
steelPipe =
    reverseProductionLine
        mempty
        customPreferredProcesses
        (RawResource IronOre, itemsPerMinute 240)
        SteelPipe


reanimatedSam :: SatisfactoryProductionLine
reanimatedSam =
    reverseProductionLine
        mempty
        customPreferredProcesses
        (RawResource SAM, itemsPerMinute 60)
        ReanimatedSAM


encasedIndustrialBeam :: SatisfactoryProductionLine
encasedIndustrialBeam =
    reverseProductionLine
        mempty
        customPreferredProcesses
        (RawResource Limestone, itemsPerMinute 240)
        EncasedIndustrialBeam


versatileFramework :: SatisfactoryProductionLine
versatileFramework =
    reverseProductionLine
        mempty
        customPreferredProcesses
        (RawResource IronOre, itemsPerMinute 240)
        VersatileFramework


customPreferredProcesses :: Map Item SatisfactoryProcess
customPreferredProcesses =
    mconcat $
        uncurry getAltRecipe
            <$> [ (Screw, 0)
                , (SteelIngot, 0)
                ]


getAltRecipe :: Item -> Int -> Map Item SatisfactoryProcess
getAltRecipe i n = M.singleton i $ (findProcessSet i).alt Unsafe.!! n
