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
    , tree
    , summary
    , machines
    )
where

import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Tree (Tree (..), drawTree)

import Process
    ( CrafterMultiplier (..)
    , Process (..)
    , ProcessCollection
    , ProcessSet (..)
    , ResolveCrafter (..)
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


data ProductionLine item crafterType crafter
    = CraftedProductionLine (CraftedItemProduction item crafterType crafter)
    | RawProductionLine QuantityPerMinute item
    deriving (Show)


data CraftedItemProduction item crafterType crafter = CraftedItemProduction
    { product :: (item, QuantityPerMinute)
    , byproduct :: Map item QuantityPerMinute
    , process :: Process item crafterType
    , multiplier :: Double
    , crafter :: crafter
    , input :: [ProductionLine item crafterType crafter]
    }
    deriving (Show)


mkProductionLine
    :: forall item crafterType crafter
     . ( ProcessCollection item crafterType
       , Show item
       , Ord item
       , CrafterMultiplier crafter
       , ResolveCrafter crafterType crafter
       )
    => Map crafterType crafter
    -> Map item (Process item crafterType)
    -> QuantityPerMinute
    -> item
    -> ProductionLine item crafterType crafter
mkProductionLine preferredCrafters preferredProcess i item =
    case findProcessSet @item @crafterType item of
        ProcessSet mainProcess []
            | M.null mainProcess.input ->
                RawProductionLine i item
        _ ->
            mkCraftedProductionLine preferredCrafters preferredProcess i item


mkCraftedProductionLine
    :: ( Show item
       , Ord item
       , ProcessCollection item crafterType
       , CrafterMultiplier crafter
       , ResolveCrafter crafterType crafter
       )
    => Map crafterType crafter
    -> Map item (Process item crafterType)
    -> QuantityPerMinute
    -> item
    -> ProductionLine item crafterType crafter
mkCraftedProductionLine
    preferredCrafters
    preferredProcess
    targetQuantityPerMinute
    item =
        CraftedProductionLine $
            CraftedItemProduction
                { product = (item, product)
                , byproduct
                , process
                , multiplier
                , crafter
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
            scaleQuantityPerMinute multiplier
                . scaleQuantityPerMinute resolvedCrafterMultiplier
                . quantityPerMinute process.cycleTime
                <$> process.output
        batchSize = getBatchSize item process
        multiplier =
            quantityPerMinuteRatio
                targetQuantityPerMinute
                defaultQuantityPerMinute
        crafter = resolveCrafter preferredCrafters process.crafter
        resolvedCrafterMultiplier =
            crafterMultiplier crafter
        defaultQuantityPerMinute =
            scaleQuantityPerMinute resolvedCrafterMultiplier $
                quantityPerMinute process.cycleTime batchSize
        input =
            fmap
                ( \(i, n) ->
                    let requiredThroughput =
                            scaleQuantityPerMinute
                                multiplier
                                (quantityPerMinute process.cycleTime n)
                     in mkProductionLine
                            preferredCrafters
                            preferredProcess
                            requiredThroughput
                            i
                )
                . M.toList
                $ process.input


reverseProductionLine'
    :: ( Ord item
       , Show item
       , ProcessCollection item crafterType
       , CrafterMultiplier crafter
       , ResolveCrafter crafterType crafter
       )
    => Map crafterType crafter
    -> Map item (Process item crafterType)
    -> (item, QuantityPerMinute)
    -> item
    -> ProductionLine item crafterType crafter
reverseProductionLine' = undefined


reverseProductionLine
    :: ( Ord item
       , Show item
       , ProcessCollection item crafterType
       , CrafterMultiplier crafter
       , ResolveCrafter crafterType crafter
       )
    => Map crafterType crafter
    -> Map item (Process item crafterType)
    -> (item, QuantityPerMinute)
    -> item
    -> ProductionLine item crafterType crafter
reverseProductionLine
    preferredCrafters
    preferredProcess
    (!providedItem, !providedQuantityPerMinute)
    targetItem =
        scaleProduction multiplier baseProductionLine
      where
        baseProductionLine =
            mkProductionLine
                preferredCrafters
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
                $ findProductionLineOutput
                    preferredCrafters
                    providedItem
                    baseProductionLine
        multiplier =
            quantityPerMinuteRatio
                providedQuantityPerMinute
                currentOutputPerMinute


scaleProduction
    :: Double
    -> ProductionLine item crafterType crafter
    -> ProductionLine item crafterType crafter
scaleProduction scale = \case
    RawProductionLine m i ->
        RawProductionLine (scaleQuantityPerMinute scale m) i
    CraftedProductionLine
        ( CraftedItemProduction
                { product
                , byproduct
                , process
                , multiplier
                , crafter
                , input
                }
            ) ->
            CraftedProductionLine $
                CraftedItemProduction
                    { product = second (scaleQuantityPerMinute scale) product
                    , byproduct = scaleQuantityPerMinute scale <$> byproduct
                    , process
                    , multiplier = multiplier * scale
                    , crafter
                    , input = scaleProduction scale <$> input
                    }


findProductionLineOutput
    :: (Ord item, CrafterMultiplier crafter, ResolveCrafter crafterType crafter)
    => Map crafterType crafter
    -> item
    -> ProductionLine item crafterType crafter
    -> Maybe QuantityPerMinute
findProductionLineOutput preferredCrafters item = \case
    RawProductionLine m pItem
        | item == pItem -> Just m
        | otherwise -> Nothing
    CraftedProductionLine
        ( CraftedItemProduction
                { product
                , byproduct
                , input
                , process = Process {crafter}
                }
            ) ->
            case M.lookup item (uncurry M.insert product byproduct) of
                Nothing ->
                    asum $
                        findProductionLineOutput
                            preferredCrafters
                            item
                            <$> input
                Just o ->
                    Just $
                        scaleQuantityPerMinute
                            ( crafterMultiplier $
                                resolveCrafter preferredCrafters crafter
                            )
                            o


getBatchSize
    :: (Show item, Ord item)
    => item
    -> Process item crafterType
    -> Quantity
getBatchSize item process =
    fromMaybe
        ( error $
            "Invalid process. Missing " <> show item <> " in output"
        )
        $ M.lookup item process.output


findPreferredProcess
    :: (Ord item, ProcessCollection item crafterType)
    => item
    -> Map item (Process item crafterType)
    -> Process item crafterType
findPreferredProcess item preferredProcess =
    fromMaybe mainProcess $
        M.lookup item preferredProcess
  where
    ProcessSet mainProcess _ = findProcessSet item


productionTree
    :: (Show crafter, Show item)
    => ProductionLine item crafterType crafter
    -> Text
productionTree =
    toText
        . drawTree
        . fmap toString
        . productionLineToTree


productionLineToTree
    :: (Show crafter, Show item)
    => ProductionLine item crafterType crafter
    -> Tree Text
productionLineToTree = \case
    RawProductionLine ipm item ->
        Node (basicShowLine item ipm) []
    CraftedProductionLine
        ( CraftedItemProduction
                { product
                , byproduct
                , crafter
                , multiplier
                , input
                }
            ) ->
            Node (showOutputs multiplier crafter product byproduct) $
                productionLineToTree <$> input


listProductionLines
    :: (Show crafter, Show item)
    => ProductionLine item crafterType crafter
    -> Text
listProductionLines = \case
    RawProductionLine ipm item ->
        basicShowLine item ipm
    CraftedProductionLine
        ( CraftedItemProduction
                { product
                , byproduct
                , crafter
                , multiplier
                , input
                }
            ) ->
            showOutputs multiplier crafter product byproduct
                <> "\n"
                <> indent
                    ( T.intercalate "\n" $
                        listProductionLines
                            <$> input
                    )


productionSummary
    :: (Show item, Ord item)
    => ProductionLine item crafterType crafter
    -> Text
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


productionMachines
    :: (Show item, Show crafter)
    => ProductionLine item crafterType crafter
    -> Text
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
                    , crafter
                    , input
                    }
                ) ->
                Node
                    ( showOutputMachines
                        multiplier
                        crafter
                        product
                        byproduct
                    )
                    $ go <$> input


machineSummary
    :: (Show crafter, Ord crafter)
    => ProductionLine item crafterType crafter
    -> Text
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
                    , crafter
                    , input
                    }
                ) ->
                foldl' go (M.insertWith (+) crafter multiplier m) input


showOutputMachines
    :: (Show item, Show crafter)
    => Double
    -> crafter
    -> (item, QuantityPerMinute)
    -> Map item QuantityPerMinute
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
    :: (Show crafter, Show item)
    => Double
    -> crafter
    -> (item, QuantityPerMinute)
    -> Map item QuantityPerMinute
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


showLine
    :: (Show crafter, Show item)
    => Double
    -> crafter
    -> item
    -> QuantityPerMinute
    -> Text
showLine multiplier crafter item qpm =
    basicShowLine item qpm
        <> " ("
        <> show multiplier
        <> " "
        <> show crafter
        <> ")"


basicShowLine :: (Show item) => item -> QuantityPerMinute -> Text
basicShowLine item qpm =
    show item
        <> ": "
        <> show qpm


tree
    :: (Show crafter, Show item)
    => ProductionLine item crafterType crafter
    -> IO ()
tree = putTextLn . productionTree


summary
    :: (Show item, Ord item)
    => ProductionLine item crafterType crafter
    -> IO ()
summary = putTextLn . productionSummary


machines
    :: (Show crafter, Ord crafter)
    => ProductionLine item crafterType crafter
    -> IO ()
machines = putTextLn . machineSummary
