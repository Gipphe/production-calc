module Factorio.SpaceExploration.Processes
    ( Item (..)
    , RawResource (..)
    , Crafter (..)
    , processes
    ) where

import Data.IxSet.Typed qualified as Ix
import Data.Map.Strict qualified as M
import Process
    ( CrafterMultiplier (..)
    , Process (..)
    , ProcessCollection (..)
    , ProcessIxs
    , ProcessSet (..)
    , ProcessType (..)
    )
import Units (items)
import Prelude hiding (Alt)


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


processes :: Ix.IxSet (ProcessIxs Item Crafter) (Process Item Crafter)
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
            <$> [ (IronOre, ElectricMiningDrill)
                , (CopperOre, ElectricMiningDrill)
                ]
        )
            <> [ Process
                    { output = one (IronPlate, items 1)
                    , mainFor = [IronPlate]
                    , cycleTime = 3.2
                    , crafter = Furnace StoneFurnace
                    , input = one (RawResource IronOre, items 1)
                    }
               , Process
                    { output = one (SteelPlate, items 1)
                    , mainFor = [SteelPlate]
                    , cycleTime = 16
                    , crafter = Furnace StoneFurnace
                    , input = one (IronPlate, items 5)
                    }
               , Process
                    { output = one (IronGearWheel, items 1)
                    , mainFor = [IronGearWheel]
                    , cycleTime = 0.5
                    , crafter = Assembler AssemblingMachine1
                    , input = one (IronPlate, items 2)
                    }
               , Process
                    { output = one (SingleCylinderEngine, items 1)
                    , mainFor = [SingleCylinderEngine]
                    , cycleTime = 0.6
                    , crafter = Assembler AssemblingMachine1
                    , input =
                        M.fromList
                            [ (IronGearWheel, items 1)
                            , (IronPlate, items 1)
                            ]
                    }
               , Process
                    { output = one (TransportBelt, items 2)
                    , mainFor = [TransportBelt]
                    , cycleTime = 0.5
                    , crafter = Assembler AssemblingMachine1
                    , input =
                        M.fromList
                            [ (SingleCylinderEngine, items 1)
                            , (IronPlate, items 1)
                            ]
                    }
               , Process
                    { output = one (CopperCable, items 2)
                    , mainFor = [CopperCable]
                    , cycleTime = 0.5
                    , crafter = Assembler AssemblingMachine1
                    , input = one (CopperPlate, items 1)
                    }
               , Process
                    { output = one (CopperPlate, items 1)
                    , mainFor = [CopperPlate]
                    , cycleTime = 3.2
                    , crafter = Furnace StoneFurnace
                    , input = one (RawResource CopperOre, items 1)
                    }
               , Process
                    { output = one (SmallElectricMotor, items 1)
                    , mainFor = [SmallElectricMotor]
                    , cycleTime = 0.8
                    , crafter = Assembler AssemblingMachine1
                    , input =
                        M.fromList
                            [ (IronGearWheel, items 1)
                            , (CopperCable, items 6)
                            , (IronPlate, items 1)
                            ]
                    }
               , Process
                    { output = one (IronStick, items 2)
                    , mainFor = [IronStick]
                    , cycleTime = 0.5
                    , crafter = Assembler AssemblingMachine1
                    , input = one (IronPlate, items 1)
                    }
               , Process
                    { output = one (BurnerInserter, items 1)
                    , mainFor = [BurnerInserter]
                    , cycleTime = 0.5
                    , crafter = Assembler AssemblingMachine1
                    , input =
                        M.fromList
                            [ (IronStick, items 2)
                            , (SingleCylinderEngine, items 1)
                            ]
                    }
               , Process
                    { output = one (Inserter, items 1)
                    , mainFor = [Inserter]
                    , cycleTime = 0.5
                    , crafter = Assembler AssemblingMachine1
                    , input =
                        M.fromList
                            [ (SmallElectricMotor, items 1)
                            , (BurnerInserter, items 1)
                            ]
                    }
               ]
data Item
    = IronPlate
    | SteelPlate
    | IronGearWheel
    | SingleCylinderEngine
    | TransportBelt
    | Inserter
    | SmallElectricMotor
    | CopperCable
    | CopperPlate
    | IronStick
    | BurnerInserter
    | RawResource RawResource
    deriving (Eq, Ord, Show)


data RawResource
    = IronOre
    | CopperOre
    deriving (Eq, Ord, Show)


data Crafter
    = Furnace Furnace
    | Assembler AssemblingMachine
    | ElectricMiningDrill
    deriving (Eq, Ord, Show)


instance CrafterMultiplier Crafter where
    crafterMultiplier = \case
        Assembler a -> crafterMultiplier a
        Furnace f -> crafterMultiplier f
        ElectricMiningDrill -> 1


data Furnace
    = StoneFurnace
    deriving (Eq, Ord, Show)


instance CrafterMultiplier Furnace where
    crafterMultiplier = \case
        StoneFurnace -> 1


data AssemblingMachine
    = AssemblingMachine1
    deriving (Eq, Ord, Show)


instance CrafterMultiplier AssemblingMachine where
    crafterMultiplier = \case
        AssemblingMachine1 -> 0.5
