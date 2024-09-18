module Satisfactory.Processes
    ( Crafter (..)
    , Item (..)
    , RawResource (..)
    , processes
    ) where

import Data.IxSet.Typed (IxSet)
import Data.IxSet.Typed qualified as Ix
import Data.Map.Strict qualified as M

import Process
    ( CrafterMultiplier (..)
    , Process (..)
    , ProcessCollection (..)
    , ProcessIxs
    , ProcessSet (..)
    , ProcessType (..)
    , ResolveCrafter (..)
    )
import Units (cubicMeters, items)
import Prelude hiding (Alt (..))


instance ProcessCollection Item Crafter where
    findProcessSet i = case maybeMain of
        Nothing -> error $ "Missing main recipe for " <> show i
        Just main ->
            ProcessSet
                { main
                , alt
                }
      where
        maybeMain = Ix.getOne $ Ix.getEQ (Main i) processes
        alt = Ix.toList $ Ix.getEQ (Alt i) processes


processes :: IxSet (ProcessIxs Item Crafter) (Process Item Crafter)
processes =
    Ix.fromList $
        ( ( \(r, crafter) ->
                Process
                    { output = one (RawResource r, items 2)
                    , mainFor = [RawResource r]
                    , crafter
                    , cycleTime = 2
                    , input = mempty
                    }
          )
            <$> [ (Water, WaterExtractor)
                , (CrudeOil, OilExtractor)
                , (IronOre, MinerMk3)
                , (CopperOre, MinerMk3)
                , (Coal, MinerMk3)
                , (CateriumOre, MinerMk3)
                , (FlowerPetals, Manual)
                , (Limestone, MinerMk3)
                , (RawQuartz, MinerMk3)
                , (SAM, MinerMk3)
                , (Bauxite, MinerMk3)
                , (Mycelia, Manual)
                , (Wood, Manual)
                , (Uranium, MinerMk3)
                , (Sulfur, MinerMk3)
                , (NitrogenGas, ResourceWell)
                ]
        )
            <> [ Process
                    { output = one (IronPlate, items 2)
                    , mainFor = [IronPlate]
                    , cycleTime = 6
                    , crafter = Constructor
                    , input = one (IronIngot, items 3)
                    }
               , Process
                    { output = one (IronIngot, items 1)
                    , mainFor = [IronIngot]
                    , cycleTime = 2
                    , crafter = Smelter
                    , input = one (RawResource IronOre, items 1)
                    }
               , Process
                    { output = one (Wire, items 2)
                    , mainFor = [Wire]
                    , cycleTime = 4
                    , crafter = Constructor
                    , input = one (CopperIngot, items 1)
                    }
               , Process
                    { output = one (Wire, items 30)
                    , mainFor = []
                    , crafter = Assembler
                    , cycleTime = 20
                    , input =
                        M.fromList
                            [ (CopperIngot, items 4)
                            , (CateriumIngot, items 1)
                            ]
                    }
               , Process
                    { output = one (Wire, items 9)
                    , mainFor = []
                    , crafter = Constructor
                    , cycleTime = 24
                    , input = one (IronIngot, items 5)
                    }
               , Process
                    { output = one (Screw, items 4)
                    , mainFor = [Screw]
                    , crafter = Constructor
                    , cycleTime = 6
                    , input = one (IronRod, items 1)
                    }
               , Process
                    { output = one (Screw, items 20)
                    , mainFor = []
                    , crafter = Constructor
                    , cycleTime = 24
                    , input = one (IronIngot, items 5)
                    }
               , Process
                    { output = one (IronRod, items 1)
                    , mainFor = [IronRod]
                    , crafter = Constructor
                    , cycleTime = 4
                    , input = one (IronIngot, items 1)
                    }
               , Process
                    { output = one (CopperIngot, items 1)
                    , mainFor = [CopperIngot]
                    , crafter = Smelter
                    , cycleTime = 2
                    , input = one (RawResource CopperOre, items 1)
                    }
               , Process
                    { output = one (CateriumIngot, items 1)
                    , mainFor = [CateriumIngot]
                    , crafter = Smelter
                    , cycleTime = 4
                    , input = one (RawResource CateriumOre, items 3)
                    }
               , Process
                    { output = one (HeavyModularFrame, items 1)
                    , mainFor = [HeavyModularFrame]
                    , crafter = Manufacturer
                    , cycleTime = 30
                    , input =
                        M.fromList
                            [ (ModularFrame, items 5)
                            , (SteelPipe, items 15)
                            , (EncasedIndustrialBeam, items 5)
                            , (Screw, items 100)
                            ]
                    }
               , Process
                    { output = one (ModularFrame, items 2)
                    , mainFor = [ModularFrame]
                    , crafter = Assembler
                    , cycleTime = 60
                    , input =
                        M.fromList
                            [ (ReinforcedIronPlate, items 3)
                            , (IronRod, items 12)
                            ]
                    }
               , Process
                    { output = one (ReinforcedIronPlate, items 1)
                    , mainFor = [ReinforcedIronPlate]
                    , crafter = Assembler
                    , cycleTime = 12
                    , input =
                        M.fromList
                            [ (IronPlate, items 6)
                            , (Screw, items 12)
                            ]
                    }
               , Process
                    { output = one (ReinforcedIronPlate, items 3)
                    , mainFor = []
                    , crafter = Assembler
                    , cycleTime = 32
                    , input =
                        M.fromList
                            [ (IronPlate, items 10)
                            , (Wire, items 20)
                            ]
                    }
               , Process
                    { output = one (SteelPipe, items 2)
                    , mainFor = [SteelPipe]
                    , crafter = Constructor
                    , cycleTime = 6
                    , input = one (SteelIngot, items 3)
                    }
               , Process
                    { output = one (SteelIngot, items 3)
                    , mainFor = [SteelIngot]
                    , crafter = Foundry
                    , cycleTime = 4
                    , input =
                        M.fromList
                            [ (RawResource IronOre, items 3)
                            , (RawResource Coal, items 3)
                            ]
                    }
               , Process
                    { output = one (EncasedIndustrialBeam, items 1)
                    , mainFor = [EncasedIndustrialBeam]
                    , crafter = Assembler
                    , cycleTime = 10
                    , input =
                        M.fromList
                            [ (Concrete, items 5)
                            , (SteelBeam, items 4)
                            ]
                    }
               , Process
                    { output = one (VersatileFramework, items 2)
                    , mainFor = [VersatileFramework]
                    , crafter = Assembler
                    , cycleTime = 24
                    , input =
                        M.fromList
                            [ (ModularFrame, items 1)
                            , (SteelBeam, items 12)
                            ]
                    }
               , Process
                    { output = one (Concrete, items 1)
                    , mainFor = [Concrete]
                    , crafter = Constructor
                    , cycleTime = 4
                    , input = one (RawResource Limestone, items 3)
                    }
               , Process
                    { output = one (ReanimatedSAM, items 1)
                    , mainFor = [ReanimatedSAM]
                    , crafter = Constructor
                    , cycleTime = 2
                    , input = one (RawResource SAM, items 4)
                    }
               , Process
                    { output = one (SteelBeam, items 1)
                    , mainFor = [SteelBeam]
                    , crafter = Constructor
                    , cycleTime = 4
                    , input = one (SteelIngot, items 4)
                    }
               , Process
                    { output = one (CopperSheet, items 1)
                    , mainFor = [CopperSheet]
                    , crafter = Constructor
                    , cycleTime = 6
                    , input = one (CopperIngot, items 2)
                    }
               , Process
                    { output = one (CopperSheet, items 3)
                    , mainFor = []
                    , crafter = Refinery
                    , cycleTime = 8
                    , input =
                        M.fromList
                            [ (CopperIngot, items 3)
                            , (RawResource Water, cubicMeters 3)
                            ]
                    }
               , Process
                    { output =
                        M.fromList
                            [ (HeavyOilResidue, items 1)
                            , (Plastic, items 2)
                            ]
                    , mainFor = [Plastic, HeavyOilResidue]
                    , crafter = Refinery
                    , cycleTime = 6
                    , input =
                        one (RawResource CrudeOil, cubicMeters 3)
                    }
               , Process
                    { output = one (Plastic, items 2)
                    , mainFor = []
                    , crafter = Refinery
                    , cycleTime = 6
                    , input =
                        M.fromList
                            [ (PolymerResin, items 6)
                            , (RawResource Water, cubicMeters 2)
                            ]
                    }
               , Process
                    { output = one (CircuitBoard, items 1)
                    , mainFor = [CircuitBoard]
                    , crafter = Assembler
                    , cycleTime = 8
                    , input =
                        M.fromList
                            [ (CopperSheet, items 2)
                            , (Plastic, items 4)
                            ]
                    }
               , Process
                    { output = one (CircuitBoard, items 7)
                    , mainFor = []
                    , crafter = Assembler
                    , cycleTime = 48
                    , input =
                        M.fromList
                            [ (Plastic, items 10)
                            , (Quickwire, items 30)
                            ]
                    }
               , Process
                    { output =
                        M.fromList
                            [ (PolymerResin, items 3)
                            , (Fuel, items 4)
                            ]
                    , mainFor = [PolymerResin]
                    , crafter = Refinery
                    , cycleTime = 6
                    , input = one (RawResource CrudeOil, cubicMeters 6)
                    }
               , Process
                    { output =
                        M.fromList
                            [ (Rubber, items 2)
                            , (HeavyOilResidue, items 2)
                            ]
                    , mainFor = [Rubber]
                    , crafter = Refinery
                    , cycleTime = 6
                    , input = one (RawResource CrudeOil, cubicMeters 3)
                    }
               , Process
                    { output = one (Rubber, items 2)
                    , mainFor = []
                    , crafter = Refinery
                    , cycleTime = 6
                    , input =
                        M.fromList
                            [ (PolymerResin, items 4)
                            , (RawResource Water, cubicMeters 4)
                            ]
                    }
               , Process
                    { output = one (QuartzCrystal, items 3)
                    , mainFor = [QuartzCrystal]
                    , crafter = Constructor
                    , cycleTime = 8
                    , input = one (RawResource RawQuartz, items 5)
                    }
               , Process
                    { output = one (Silica, items 5)
                    , mainFor = [Silica]
                    , crafter = Constructor
                    , cycleTime = 8
                    , input = one (RawResource RawQuartz, items 3)
                    }
               , Process
                    { output = one (CrystalOscillator, items 2)
                    , mainFor = [CrystalOscillator]
                    , crafter = Manufacturer
                    , cycleTime = 120
                    , input =
                        M.fromList
                            [ (QuartzCrystal, items 36)
                            , (Cable, items 28)
                            , (ReinforcedIronPlate, items 5)
                            ]
                    }
               , Process
                    { output = one (Cable, items 1)
                    , mainFor = [Cable]
                    , crafter = Constructor
                    , cycleTime = 2
                    , input = one (Wire, items 2)
                    }
               , Process
                    { output = one (Computer, items 1)
                    , mainFor = [Computer]
                    , crafter = Manufacturer
                    , cycleTime = 24
                    , input =
                        M.fromList
                            [ (CircuitBoard, items 10)
                            , (Cable, items 9)
                            , (Plastic, items 18)
                            , (Screw, items 52)
                            ]
                    }
               , Process
                    { output = one (ColorCartridge, items 10)
                    , mainFor = [ColorCartridge]
                    , crafter = Constructor
                    , cycleTime = 6
                    , input = one (RawResource FlowerPetals, items 5)
                    }
               , Process
                    { output = one (AdaptiveControlUnit, items 2)
                    , mainFor = [AdaptiveControlUnit]
                    , crafter = Manufacturer
                    , cycleTime = 120
                    , input =
                        M.fromList
                            [ (AutomatedWiring, items 15)
                            , (CircuitBoard, items 10)
                            , (HeavyModularFrame, items 2)
                            , (Computer, items 2)
                            ]
                    }
               , Process
                    { output = one (AutomatedWiring, items 1)
                    , mainFor = [AutomatedWiring]
                    , crafter = Assembler
                    , cycleTime = 24
                    , input =
                        M.fromList
                            [ (Stator, items 1)
                            , (Cable, items 20)
                            ]
                    }
               , Process
                    { output = one (Stator, items 1)
                    , mainFor = [Stator]
                    , crafter = Assembler
                    , cycleTime = 12
                    , input =
                        M.fromList
                            [ (SteelPipe, items 3)
                            , (Wire, items 8)
                            ]
                    }
               , Process
                    { output = one (Stator, items 2)
                    , mainFor = []
                    , crafter = Assembler
                    , cycleTime = 15
                    , input =
                        M.fromList
                            [ (SteelPipe, items 4)
                            , (Quickwire, items 15)
                            ]
                    }
               , Process
                    { output = one (Quickwire, items 5)
                    , mainFor = [Quickwire]
                    , crafter = Constructor
                    , cycleTime = 5
                    , input = one (CateriumIngot, items 1)
                    }
               , Process
                    { output = one (ModularEngine, items 1)
                    , mainFor = [ModularEngine]
                    , crafter = Manufacturer
                    , cycleTime = 60
                    , input =
                        M.fromList
                            [ (Motor, items 2)
                            , (Rubber, items 15)
                            , (SmartPlating, items 2)
                            ]
                    }
               , Process
                    { output = one (Motor, items 1)
                    , mainFor = [Motor]
                    , crafter = Assembler
                    , cycleTime = 12
                    , input =
                        M.fromList
                            [ (Rotor, items 2)
                            , (Stator, items 2)
                            ]
                    }
               , Process
                    { output = one (Motor, items 6)
                    , mainFor = []
                    , crafter = Manufacturer
                    , cycleTime = 48
                    , input =
                        M.fromList
                            [ (Rotor, items 3)
                            , (Stator, items 3)
                            , (CrystalOscillator, items 1)
                            ]
                    }
               , Process
                    { output = one (Rotor, items 1)
                    , mainFor = [Rotor]
                    , crafter = Assembler
                    , cycleTime = 15
                    , input =
                        M.fromList
                            [ (IronRod, items 5)
                            , (Screw, items 25)
                            ]
                    }
               , Process
                    { output = one (SmartPlating, items 1)
                    , mainFor = [SmartPlating]
                    , crafter = Assembler
                    , cycleTime = 30
                    , input =
                        M.fromList
                            [ (ReinforcedIronPlate, items 1)
                            , (Rotor, items 1)
                            ]
                    }
               , Process
                    { output = one (SmartPlating, items 2)
                    , mainFor = []
                    , crafter = Manufacturer
                    , cycleTime = 24
                    , input =
                        M.fromList
                            [ (ReinforcedIronPlate, items 1)
                            , (Rotor, items 1)
                            , (Plastic, items 3)
                            ]
                    }
               , Process
                    { output = one (Plastic, items 2)
                    , mainFor = []
                    , crafter = Refinery
                    , cycleTime = 6
                    , input =
                        M.fromList
                            [ (PolymerResin, items 6)
                            , (RawResource Water, cubicMeters 2)
                            ]
                    }
               , Process
                    { output =
                        M.fromList
                            [ (Fuel, items 4)
                            , (PolymerResin, items 3)
                            ]
                    , mainFor = [Fuel]
                    , crafter = Refinery
                    , cycleTime = 6
                    , input = one (RawResource CrudeOil, cubicMeters 6)
                    }
               , Process
                    { output = one (PackagedFuel, items 2)
                    , mainFor = [PackagedFuel]
                    , crafter = Packager
                    , cycleTime = 3
                    , input =
                        M.fromList
                            [ (Fuel, items 2)
                            , (EmptyCanister, items 2)
                            ]
                    }
               , Process
                    { output = one (SteelIngot, items 3)
                    , mainFor = []
                    , crafter = Assembler
                    , cycleTime = 3
                    , input =
                        M.fromList
                            [ (IronIngot, items 2)
                            , (RawResource Coal, items 2)
                            ]
                    }
               , Process
                    { output = one (HighSpeedConnector, items 1)
                    , mainFor = [HighSpeedConnector]
                    , crafter = Manufacturer
                    , cycleTime = 16
                    , input =
                        M.fromList
                            [ (Quickwire, items 56)
                            , (Cable, items 10)
                            , (CircuitBoard, items 1)
                            ]
                    }
               , Process
                    { output = one (HighSpeedConnector, items 2)
                    , mainFor = []
                    , crafter = Manufacturer
                    , cycleTime = 40
                    , input =
                        M.fromList
                            [ (Quickwire, items 60)
                            , (Silica, items 25)
                            , (CircuitBoard, items 2)
                            ]
                    }
               , Process
                    { output = one (AluminumIngot, items 4)
                    , mainFor = [AluminumIngot]
                    , crafter = Foundry
                    , cycleTime = 4
                    , input =
                        M.fromList
                            [ (AluminumScrap, items 6)
                            , (Silica, items 5)
                            ]
                    }
               , Process
                    { output =
                        M.fromList
                            [ (AluminumScrap, items 6)
                            , (RawResource Water, cubicMeters 2)
                            ]
                    , mainFor = [AluminumScrap]
                    , crafter = Refinery
                    , cycleTime = 1
                    , input =
                        M.fromList
                            [ (AluminaSolution, cubicMeters 4)
                            , (RawResource Coal, items 2)
                            ]
                    }
               , Process
                    { output =
                        M.fromList
                            [ (AluminaSolution, cubicMeters 12)
                            , (Silica, items 5)
                            ]
                    , mainFor = [AluminaSolution]
                    , crafter = Refinery
                    , cycleTime = 6
                    , input =
                        M.fromList
                            [ (RawResource Bauxite, items 12)
                            , (RawResource Water, cubicMeters 18)
                            ]
                    }
               , Process
                    { output = one (AlcladAluminumSheet, items 3)
                    , mainFor = [AlcladAluminumSheet]
                    , crafter = Assembler
                    , cycleTime = 6
                    , input =
                        M.fromList
                            [ (AluminumIngot, items 3)
                            , (CopperIngot, items 1)
                            ]
                    }
               , Process
                    { output = one (AluminumCasing, items 2)
                    , mainFor = [AluminumCasing]
                    , crafter = Constructor
                    , cycleTime = 2
                    , input = one (AluminumIngot, items 3)
                    }
               , Process
                    { output = one (RadioControlUnit, items 2)
                    , mainFor = [RadioControlUnit]
                    , crafter = Manufacturer
                    , cycleTime = 48
                    , input =
                        M.fromList
                            [ (AluminumCasing, items 32)
                            , (CrystalOscillator, items 1)
                            , (Computer, items 1)
                            ]
                    }
               , Process
                    { output = one (SuperComputer, items 1)
                    , mainFor = [SuperComputer]
                    , crafter = Manufacturer
                    , cycleTime = 32
                    , input =
                        M.fromList
                            [ (Computer, items 2)
                            , (AILimiter, items 2)
                            , (HighSpeedConnector, items 3)
                            , (Plastic, items 28)
                            ]
                    }
               , Process
                    { output = one (AILimiter, items 1)
                    , mainFor = [AILimiter]
                    , crafter = Assembler
                    , cycleTime = 12
                    , input =
                        M.fromList
                            [ (CopperSheet, items 5)
                            , (Quickwire, items 20)
                            ]
                    }
               , Process
                    { output = one (GasFilter, items 1)
                    , mainFor = [GasFilter]
                    , crafter = Manufacturer
                    , cycleTime = 8
                    , input =
                        M.fromList
                            [ (RawResource Coal, items 5)
                            , (Rubber, items 2)
                            , (Fabric, items 2)
                            ]
                    }
               , Process
                    { output = one (Fabric, items 1)
                    , mainFor = [Fabric]
                    , crafter = Assembler
                    , cycleTime = 4
                    , input =
                        M.fromList
                            [ (RawResource Mycelia, items 1)
                            , (Biomass, items 5)
                            ]
                    }
               , Process
                    { output = one (Biomass, items 20)
                    , mainFor = [Biomass]
                    , crafter = Constructor
                    , cycleTime = 4
                    , input =
                        M.fromList
                            [ (RawResource Wood, items 4)
                            ]
                    }
               , Process
                    { output = one (Fabric, items 1)
                    , mainFor = []
                    , crafter = Assembler
                    , cycleTime = 2
                    , input =
                        M.fromList
                            [ (PolymerResin, items 1)
                            , (RawResource Water, cubicMeters 1)
                            ]
                    }
               , Process
                    { output = one (EmptyCanister, items 4)
                    , mainFor = [EmptyCanister]
                    , crafter = Constructor
                    , cycleTime = 4
                    , input = one (Plastic, items 2)
                    }
               , Process
                    { output = one (IodineInfusedFilter, items 1)
                    , mainFor = [IodineInfusedFilter]
                    , crafter = Manufacturer
                    , cycleTime = 16
                    , input =
                        M.fromList
                            [ (GasFilter, items 1)
                            , (Quickwire, items 8)
                            , (AluminumCasing, items 1)
                            ]
                    }
               , Process
                    { output =
                        M.fromList
                            [ (EncasedUraniumCell, items 5)
                            , (SulfuricAcid, cubicMeters 2)
                            ]
                    , mainFor = [EncasedUraniumCell]
                    , crafter = Blender
                    , cycleTime = 12
                    , input =
                        M.fromList
                            [ (RawResource Uranium, items 10)
                            , (Concrete, items 3)
                            , (SulfuricAcid, cubicMeters 8)
                            ]
                    }
               , Process
                    { output = one (SulfuricAcid, cubicMeters 5)
                    , mainFor = [SulfuricAcid]
                    , crafter = Refinery
                    , cycleTime = 6
                    , input =
                        M.fromList
                            [ (RawResource Sulfur, items 5)
                            , (RawResource Water, cubicMeters 5)
                            ]
                    }
               , Process
                    { output = one (ElectromagneticControlRod, items 2)
                    , mainFor = [ElectromagneticControlRod]
                    , crafter = Assembler
                    , cycleTime = 30
                    , input =
                        M.fromList
                            [ (Stator, items 3)
                            , (AILimiter, items 2)
                            ]
                    }
               , Process
                    { output = one (UraniumFuelRod, items 1)
                    , mainFor = [UraniumFuelRod]
                    , crafter = Manufacturer
                    , cycleTime = 150
                    , input =
                        M.fromList
                            [ (EncasedUraniumCell, items 50)
                            , (EncasedIndustrialBeam, items 3)
                            , (ElectromagneticControlRod, items 5)
                            ]
                    }
               , Process
                    { output = one (Quickwire, items 12)
                    , mainFor = []
                    , crafter = Assembler
                    , cycleTime = 8
                    , input =
                        M.fromList
                            [ (CateriumIngot, items 1)
                            , (CopperIngot, items 5)
                            ]
                    }
               , Process
                    { output = one (EncasedIndustrialBeam, items 1)
                    , mainFor = []
                    , crafter = Assembler
                    , cycleTime = 15
                    , input =
                        M.fromList
                            [ (SteelPipe, items 7)
                            , (Concrete, items 5)
                            ]
                    }
               , Process
                    { output = one (FusedModularFrame, items 1)
                    , mainFor = [FusedModularFrame]
                    , crafter = Blender
                    , cycleTime = 40
                    , input =
                        M.fromList
                            [ (HeavyModularFrame, items 1)
                            , (AluminumCasing, items 50)
                            , (RawResource NitrogenGas, cubicMeters 25)
                            ]
                    }
               , Process
                    { output = one (TurboMotor, items 1)
                    , mainFor = [TurboMotor]
                    , crafter = Manufacturer
                    , cycleTime = 32
                    , input =
                        M.fromList
                            [ (CoolingSystem, items 4)
                            , (RadioControlUnit, items 2)
                            , (Motor, items 4)
                            , (Rubber, items 24)
                            ]
                    }
               , Process
                    { output = one (CoolingSystem, items 1)
                    , mainFor = [CoolingSystem]
                    , crafter = Blender
                    , cycleTime = 10
                    , input =
                        M.fromList
                            [ (HeatSink, items 2)
                            , (Rubber, items 2)
                            , (RawResource Water, cubicMeters 5)
                            , (RawResource NitrogenGas, cubicMeters 25)
                            ]
                    }
               , Process
                    { output = one (HeatSink, items 1)
                    , mainFor = [HeatSink]
                    , crafter = Assembler
                    , cycleTime = 8
                    , input =
                        M.fromList
                            [ (AlcladAluminumSheet, items 5)
                            , (CopperSheet, items 3)
                            ]
                    }
               , Process
                    { output = one (Computer, items 3)
                    , mainFor = []
                    , crafter = Assembler
                    , cycleTime = 64
                    , input =
                        M.fromList
                            [ (CircuitBoard, items 8)
                            , (CrystalOscillator, items 3)
                            ]
                    }
               , Process
                    { output = one (CircuitBoard, items 5)
                    , mainFor = []
                    , crafter = Assembler
                    , cycleTime = 24
                    , input =
                        M.fromList
                            [ (CopperSheet, items 11)
                            , (Silica, items 11)
                            ]
                    }
               , Process
                    { output = one (AluminaSolution, cubicMeters 12)
                    , mainFor = []
                    , crafter = Refinery
                    , cycleTime = 3
                    , input =
                        M.fromList
                            [ (RawResource Bauxite, items 10)
                            , (RawResource Water, cubicMeters 10)
                            ]
                    }
               , Process
                    { output = one (AluminumIngot, items 1)
                    , mainFor = []
                    , crafter = Smelter
                    , cycleTime = 2
                    , input = one (AluminumScrap, items 2)
                    }
               , Process
                    { output = one (CopperIngot, items 15)
                    , mainFor = []
                    , crafter = Refinery
                    , cycleTime = 24
                    , input =
                        M.fromList
                            [ (RawResource CopperOre, items 6)
                            , (RawResource Water, cubicMeters 4)
                            ]
                    }
               ]


data Item
    = RawResource RawResource
    | IronPlate
    | IronIngot
    | Wire
    | CopperIngot
    | CateriumIngot
    | IronRod
    | Screw
    | HeavyModularFrame
    | ModularFrame
    | ReinforcedIronPlate
    | SteelPipe
    | SteelIngot
    | EncasedIndustrialBeam
    | Concrete
    | SteelBeam
    | CircuitBoard
    | Plastic
    | CopperSheet
    | PolymerResin
    | HeavyOilResidue
    | Rubber
    | QuartzCrystal
    | Silica
    | CrystalOscillator
    | Cable
    | Computer
    | ColorCartridge
    | AdaptiveControlUnit
    | AutomatedWiring
    | Stator
    | Quickwire
    | ModularEngine
    | Motor
    | Rotor
    | SmartPlating
    | Fuel
    | PackagedFuel
    | EmptyCanister
    | HighSpeedConnector
    | AluminumIngot
    | VersatileFramework
    | AluminumScrap
    | AluminaSolution
    | AlcladAluminumSheet
    | AluminumCasing
    | RadioControlUnit
    | SuperComputer
    | AILimiter
    | GasFilter
    | Fabric
    | Biomass
    | IodineInfusedFilter
    | EncasedUraniumCell
    | SulfuricAcid
    | ElectromagneticControlRod
    | UraniumFuelRod
    | FusedModularFrame
    | TurboMotor
    | CoolingSystem
    | HeatSink
    | ReanimatedSAM
    deriving (Eq, Ord, Show)


data RawResource
    = Water
    | CrudeOil
    | IronOre
    | CopperOre
    | Coal
    | CateriumOre
    | FlowerPetals
    | Limestone
    | RawQuartz
    | Bauxite
    | Mycelia
    | Wood
    | Uranium
    | Sulfur
    | NitrogenGas
    | SAM
    deriving (Eq, Ord, Enum, Bounded, Show)


data Crafter
    = Constructor
    | Assembler
    | Manufacturer
    | Refinery
    | MinerMk3
    | Smelter
    | Foundry
    | WaterExtractor
    | OilExtractor
    | Packager
    | Manual
    | Blender
    | ResourceWell
    deriving (Eq, Ord, Show)


instance CrafterMultiplier Crafter where
    crafterMultiplier _ = 1


instance ResolveCrafter Crafter Crafter where
    resolveCrafter _ = id
