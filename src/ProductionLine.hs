module ProductionLine
    ( ProductionLine (..)
    , mkProductionLine
    , reverseProductionLine
    , listProductionLines
    , productionLineToTree
    , productionSummary
    , productionTree
    , productionMachines
    , machineSummary
    ) where

import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Tree (Tree (..), drawTree)

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
import Prelude hiding (product)


data ProductionLine
    = CraftedProductionLine CraftedItemProduction
    | RawProductionLine QuantityPerMinute Item
    deriving (Show)


data CraftedItemProduction = CraftedItemProduction
    { product :: (Item, QuantityPerMinute)
    , byproduct :: Map Item QuantityPerMinute
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
            { product = (item, product)
            , byproduct
            , process
            , multiplier
            , input
            }
  where
    process = findPreferredProcess item preferredProcess
    product =
        fromMaybe
            (error $ "Missing product in expected process: " <> show item)
            $ M.lookup item output
    byproduct = M.delete item output
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
        baseProductionLine =
            mkProductionLine
                preferredProcess
                (quantityPerMinute 60 $ items 100)
                targetItem
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
                { product
                , byproduct
                , process
                , multiplier
                , input
                }
            ) ->
            CraftedProductionLine $
                CraftedItemProduction
                    { product = second (scaleQuantityPerMinute scale) product
                    , byproduct = scaleQuantityPerMinute scale <$> byproduct
                    , process
                    , multiplier = multiplier * scale
                    , input = scaleProduction scale <$> input
                    }


findProductionLineOutput :: Item -> ProductionLine -> Maybe QuantityPerMinute
findProductionLineOutput item = \case
    RawProductionLine m pItem
        | item == pItem -> Just m
        | otherwise -> Nothing
    CraftedProductionLine (CraftedItemProduction {product, byproduct, input}) ->
        case M.lookup item (uncurry M.insert product byproduct) of
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
        Node (basicShowLine item ipm) []
    CraftedProductionLine
        ( CraftedItemProduction
                { product
                , byproduct
                , process
                , multiplier
                , input
                }
            ) ->
            Node (showOutputs multiplier process.crafter product byproduct) $
                productionLineToTree <$> input


listProductionLines :: ProductionLine -> Text
listProductionLines = \case
    RawProductionLine ipm item ->
        basicShowLine item ipm
    CraftedProductionLine
        ( CraftedItemProduction
                { product
                , byproduct
                , process
                , multiplier
                , input
                }
            ) ->
            showOutputs multiplier process.crafter product byproduct
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
        CraftedProductionLine
            ( CraftedItemProduction
                    { product
                    , byproduct
                    , input
                    }
                ) ->
                foldl'
                    go
                    ( uncurry
                        (M.insertWith sumQuantitiesPerMinute)
                        product
                        $ M.unionWith sumQuantitiesPerMinute byproduct m
                    )
                    input


productionMachines :: ProductionLine -> Text
productionMachines =
    toText
        . drawTree
        . fmap toString
        . go
  where
    go = \case
        RawProductionLine qpm item ->
            Node (basicShowLine item qpm) []
        CraftedProductionLine
            ( CraftedItemProduction
                    { product
                    , byproduct
                    , multiplier
                    , process
                    , input
                    }
                ) ->
                Node
                    ( showOutputMachines
                        multiplier
                        process.crafter
                        product
                        byproduct
                    )
                    $ go <$> input


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


showOutputMachines
    :: Double
    -> Crafter
    -> (Item, QuantityPerMinute)
    -> Map Item QuantityPerMinute
    -> Text
showOutputMachines multiplier crafter (item, _) byproduct
    | not (M.null byproduct) =
        T.intercalate "\n"
            . fmap (\(i, t) -> show i <> ". " <> t)
            . zip [(1 :: Int) ..]
            $ (showMachines item : (showMachines <$> M.keys byproduct))
    | otherwise = showMachines item
  where
    showMachines i =
        show i
            <> ": "
            <> show multiplier
            <> " "
            <> show crafter


showOutputs
    :: Double
    -> Crafter
    -> (Item, QuantityPerMinute)
    -> Map Item QuantityPerMinute
    -> Text
showOutputs multiplier crafter product byproduct
    | not (M.null byproduct) =
        T.intercalate "\n"
            . fmap (\(i, t) -> show i <> ". " <> t)
            . zip [(1 :: Int) ..]
            $ uncurry showProd product
                : (uncurry showProd <$> M.toList byproduct)
    | otherwise =
        uncurry showProd product
  where
    showProd = showLine multiplier crafter


showLine :: Double -> Crafter -> Item -> QuantityPerMinute -> Text
showLine multiplier crafter item qpm =
    basicShowLine item qpm
        <> " ("
        <> show multiplier
        <> " "
        <> show crafter
        <> ")"


basicShowLine :: Item -> QuantityPerMinute -> Text
basicShowLine item qpm =
    show item
        <> ": "
        <> show qpm
