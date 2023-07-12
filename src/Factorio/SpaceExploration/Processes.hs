module Factorio.SpaceExploration.Processes
    ( Item (..)
    , RawResource (..)
    , Crafter (..)
    , CrafterType (..)
    , Furnace (..)
    , AssemblingMachine (..)
    , Miner (..)
    , processes
    )
where

import Data.IxSet.Typed qualified as Ix
import Data.Map.Strict qualified as M
import Text.Show qualified as S

import Process
    ( CrafterMultiplier (..)
    , Process (..)
    , ProcessCollection (..)
    , ProcessIxs
    , ProcessSet (..)
    , ProcessType (..)
    , ResolveCrafter (..)
    )
import Units (items)
import Prelude hiding (Alt)


instance ProcessCollection Item CrafterType where
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


processes :: Ix.IxSet (ProcessIxs Item CrafterType) (Process Item CrafterType)
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
            <$> [ (IronOre, Miner)
                , (CopperOre, Miner)
                , (Stone, Miner)
                ]
        )
            <> [ Process
                    { output = one (IronPlate, items 1)
                    , mainFor = [IronPlate]
                    , cycleTime = 3.2
                    , crafter = Furnace
                    , input = one (RawResource IronOre, items 1)
                    }
               , Process
                    { output = one (SteelPlate, items 1)
                    , mainFor = [SteelPlate]
                    , cycleTime = 16
                    , crafter = Furnace
                    , input = one (IronPlate, items 5)
                    }
               , Process
                    { output = one (IronGearWheel, items 1)
                    , mainFor = [IronGearWheel]
                    , cycleTime = 0.5
                    , crafter = AssemblingMachine
                    , input = one (IronPlate, items 2)
                    }
               , Process
                    { output = one (SingleCylinderEngine, items 1)
                    , mainFor = [SingleCylinderEngine]
                    , cycleTime = 0.6
                    , crafter = AssemblingMachine
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
                    , crafter = AssemblingMachine
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
                    , crafter = AssemblingMachine
                    , input = one (CopperPlate, items 1)
                    }
               , Process
                    { output = one (CopperPlate, items 1)
                    , mainFor = [CopperPlate]
                    , cycleTime = 3.2
                    , crafter = Furnace
                    , input = one (RawResource CopperOre, items 1)
                    }
               , Process
                    { output = one (SmallElectricMotor, items 1)
                    , mainFor = [SmallElectricMotor]
                    , cycleTime = 0.8
                    , crafter = AssemblingMachine
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
                    , crafter = AssemblingMachine
                    , input = one (IronPlate, items 1)
                    }
               , Process
                    { output = one (BurnerInserter, items 1)
                    , mainFor = [BurnerInserter]
                    , cycleTime = 0.5
                    , crafter = AssemblingMachine
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
                    , crafter = AssemblingMachine
                    , input =
                        M.fromList
                            [ (SmallElectricMotor, items 1)
                            , (BurnerInserter, items 1)
                            ]
                    }
               , Process
                    { output = one (Sand, items 2)
                    , mainFor = [Sand]
                    , cycleTime = 0.5
                    , crafter = AssemblingMachine
                    , input =
                        M.fromList
                            [ (RawResource Stone, items 1)
                            ]
                    }
               , Process
                    { output = one (Glass, items 1)
                    , mainFor = [Glass]
                    , cycleTime = 4
                    , crafter = Furnace
                    , input = one (Sand, items 4)
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
    | Sand
    | Glass
    | RawResource RawResource
    deriving (Eq, Ord, Show)


data RawResource
    = IronOre
    | CopperOre
    | Stone
    deriving (Eq, Ord, Show)


data CrafterType = Furnace | AssemblingMachine | Miner
    deriving (Eq, Ord, Show)


data Crafter
    = FurnaceCrafter Furnace
    | AssemblingMachineCrafter AssemblingMachine
    | MinerCrafter Miner
    deriving (Eq, Ord)


instance S.Show Crafter where
    show = \case
        FurnaceCrafter x -> show x
        AssemblingMachineCrafter x -> show x
        MinerCrafter x -> show x


instance CrafterMultiplier Crafter where
    crafterMultiplier = \case
        AssemblingMachineCrafter a -> crafterMultiplier a
        FurnaceCrafter f -> crafterMultiplier f
        MinerCrafter m -> crafterMultiplier m


data Furnace
    = StoneFurnace
    | SteelFurnace
    deriving (Eq, Ord, Show)


instance CrafterMultiplier Furnace where
    crafterMultiplier = \case
        StoneFurnace -> 1
        SteelFurnace -> 2


data AssemblingMachine
    = AssemblingMachine1
    | AssemblingMachine2
    deriving (Eq, Ord, Show)


instance CrafterMultiplier AssemblingMachine where
    crafterMultiplier = \case
        AssemblingMachine1 -> 0.5
        AssemblingMachine2 -> 0.75


data Miner = ElectricMiningDrill
    deriving (Eq, Ord, Show)


instance CrafterMultiplier Miner where
    crafterMultiplier = \case
        ElectricMiningDrill -> 1


instance ResolveCrafter CrafterType Crafter where
    resolveCrafter preferredCrafters crafterType =
        fromMaybe defaultCrafter $ M.lookup crafterType preferredCrafters
      where
        defaultCrafter = case crafterType of
            Furnace -> FurnaceCrafter StoneFurnace
            AssemblingMachine -> AssemblingMachineCrafter AssemblingMachine1
            Miner -> MinerCrafter ElectricMiningDrill
