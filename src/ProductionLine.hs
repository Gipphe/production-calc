module ProductionLine
    ( ProductionLine (..)
    , mkProductionLine
    , reverseProductionLine
    , listProductionLines
    , productionLineToTree
    , productionSummary
    , productionTree
    , productionMachines
    , customPreferredProcesses
    , machineSummary
    ) where

import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Tree (Tree (..), drawTree)
import Relude.Unsafe qualified as Unsafe

import Process
    ( Crafter
    , Item (..)
    , Process (..)
    , ProcessSet (..)
    , findProcessSet
    )
import Units
    ( Quantity
    , QuantityPerMinute
    , items
    , quantityPerMinute
    , quantityPerMinuteRatio
    , scaleQuantityPerMinute
    , sumQuantitiesPerMinute
    )
import Util (indent)
import Prelude


data ProductionLine
    = CraftedProductionLine CraftedItemProduction
    | RawProductionLine QuantityPerMinute Item
    deriving (Show)


data CraftedItemProduction = CraftedItemProduction
    { output :: Map Item QuantityPerMinute
    , process :: Process
    , multiplier :: Double
    , input :: [ProductionLine]
    }
    deriving (Show)


mkProductionLine
    :: Map Item Process
    -> QuantityPerMinute
    -> Item
    -> ProductionLine
mkProductionLine preferredProcess i item = case findProcessSet item of
    ProcessSet mainProcess []
        | M.null mainProcess.input ->
            RawProductionLine i item
    _ ->
        mkCraftedProductionLine preferredProcess i item


mkCraftedProductionLine
    :: Map Item Process
    -> QuantityPerMinute
    -> Item
    -> ProductionLine
mkCraftedProductionLine preferredProcess targetQuantityPerMinute item =
    CraftedProductionLine $
        CraftedItemProduction
            { output
            , process
            , multiplier
            , input
            }
  where
    process = findPreferredProcess item preferredProcess
    output =
        scaleQuantityPerMinute multiplier . quantityPerMinute process.cycleTime
            <$> process.output
    batchSize = getBatchSize item process
    multiplier =
        quantityPerMinuteRatio targetQuantityPerMinute defaultQuantityPerMinute
    defaultQuantityPerMinute = quantityPerMinute process.cycleTime batchSize
    input =
        fmap
            ( \(i, n) ->
                mkProductionLine
                    preferredProcess
                    ( scaleQuantityPerMinute
                        multiplier
                        (quantityPerMinute process.cycleTime n)
                    )
                    i
            )
            . M.toList
            $ process.input


reverseProductionLine
    :: Map Item Process
    -> (Item, QuantityPerMinute)
    -> Item
    -> ProductionLine
reverseProductionLine
    preferredProcess
    (!providedItem, !providedQuantityPerMinute)
    targetItem =
        scaleProduction multiplier baseProductionLine
      where
        baseProductionLine = mkProductionLine preferredProcess (quantityPerMinute 60 $ items 100) targetItem
        currentOutputPerMinute =
            fromMaybe
                ( error
                    ( "Item missing from productionLine: "
                        <> show providedItem
                    )
                )
                $ findProductionLineOutput providedItem baseProductionLine
        multiplier =
            quantityPerMinuteRatio
                providedQuantityPerMinute
                currentOutputPerMinute


scaleProduction :: Double -> ProductionLine -> ProductionLine
scaleProduction scale = \case
    RawProductionLine m i ->
        RawProductionLine (scaleQuantityPerMinute scale m) i
    CraftedProductionLine
        ( CraftedItemProduction
                { output
                , process
                , multiplier
                , input
                }
            ) ->
            CraftedProductionLine $
                CraftedItemProduction
                    { output = scaleQuantityPerMinute scale <$> output
                    , process
                    , multiplier
                    , input = scaleProduction scale <$> input
                    }


findProductionLineOutput :: Item -> ProductionLine -> Maybe QuantityPerMinute
findProductionLineOutput item = \case
    RawProductionLine m pItem
        | item == pItem -> Just m
        | otherwise -> Nothing
    CraftedProductionLine (CraftedItemProduction {output, input}) ->
        case M.lookup item output of
            Nothing -> asum $ findProductionLineOutput item <$> input
            Just o -> Just o


getBatchSize :: Item -> Process -> Quantity
getBatchSize item process =
    fromMaybe
        ( error $
            "Invalid process. Missing " <> show item <> " in output"
        )
        $ M.lookup item process.output


findPreferredProcess :: Item -> Map Item Process -> Process
findPreferredProcess item preferredProcess =
    fromMaybe mainProcess $
        M.lookup item preferredProcess
  where
    ProcessSet mainProcess _ = findProcessSet item


productionTree :: ProductionLine -> Text
productionTree =
    toText
        . drawTree
        . fmap toString
        . productionLineToTree


productionLineToTree :: ProductionLine -> Tree Text
productionLineToTree = \case
    RawProductionLine ipm item ->
        Node (showLine item ipm) []
    CraftedProductionLine (CraftedItemProduction {output, input}) ->
        Node (showOutputs output) $
            productionLineToTree <$> input


listProductionLines :: ProductionLine -> Text
listProductionLines = \case
    RawProductionLine ipm item ->
        showLine item ipm
    CraftedProductionLine (CraftedItemProduction {output, input}) ->
        showOutputs output
            <> "\n"
            <> indent
                ( T.intercalate "\n" $
                    listProductionLines
                        <$> input
                )


productionSummary :: ProductionLine -> Text
productionSummary =
    T.intercalate "\n"
        . sort
        . fmap (\(i, n) -> show i <> ": " <> show n)
        . M.toList
        . go mempty
  where
    go m = \case
        RawProductionLine qpm item ->
            M.insertWith sumQuantitiesPerMinute item qpm m
        CraftedProductionLine (CraftedItemProduction {output, input}) ->
            foldl' go (M.unionWith sumQuantitiesPerMinute output m) input


productionMachines :: ProductionLine -> Text
productionMachines =
    toText
        . drawTree
        . fmap toString
        . go
  where
    go = \case
        RawProductionLine qpm item ->
            Node (showLine item qpm) []
        CraftedProductionLine
            ( CraftedItemProduction
                    { output
                    , multiplier
                    , process
                    , input
                    }
                ) ->
                Node (showOutputMachines multiplier process.crafter output) $
                    go <$> input


machineSummary :: ProductionLine -> Text
machineSummary =
    T.intercalate "\n"
        . sort
        . fmap (\(i, n) -> show i <> ": " <> show n)
        . M.toList
        . go mempty
  where
    go m = \case
        RawProductionLine _ _ -> m
        CraftedProductionLine
            ( CraftedItemProduction
                    { multiplier
                    , process
                    , input
                    }
                ) ->
                foldl' go (M.insertWith (+) process.crafter multiplier m) input


showOutputMachines :: Double -> Crafter -> Map Item QuantityPerMinute -> Text
showOutputMachines multiplier crafter output
    | M.size output > 1 =
        T.intercalate "\n"
            . fmap (\(i, t) -> show i <> ". " <> t)
            . zip [(1 :: Int) ..]
            $ showMachines <$> M.keys output
    | otherwise = mconcat $ showMachines <$> M.keys output
  where
    showMachines item =
        show item
            <> ": "
            <> show multiplier
            <> " "
            <> show crafter


showOutputs :: Map Item QuantityPerMinute -> Text
showOutputs output
    | M.size output > 1 =
        T.intercalate "\n"
            . fmap (\(i, t) -> show i <> ". " <> t)
            . zip [(1 :: Int) ..]
            $ uncurry showLine <$> M.toList output
    | otherwise = mconcat $ uncurry showLine <$> M.toList output


showLine :: Item -> QuantityPerMinute -> Text
showLine item ipm =
    show item <> ": " <> show ipm


customPreferredProcesses :: Map Item Process
customPreferredProcesses =
    M.fromList $
        (\(i, n) -> (i, (findProcessSet i).alt Unsafe.!! n))
            <$> [ (ReinforcedIronPlate, 0)
                , (Wire, 1)
                , (SteelIngot, 0)
                , (CircuitBoard, 0)
                , (Fabric, 0)
                , (Quickwire, 0)
                , (EncasedIndustrialBeam, 0)
                ]
