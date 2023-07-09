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
    ) where

import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Tree (Tree (..), drawTree)

import Process
    ( CrafterMultiplier (..)
    , Process (..)
    , ProcessCollection
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


data ProductionLine item crafter
    = CraftedProductionLine (CraftedItemProduction item crafter)
    | RawProductionLine QuantityPerMinute item
    deriving (Show)


data CraftedItemProduction item crafter = CraftedItemProduction
    { product :: (item, QuantityPerMinute)
    , byproduct :: Map item QuantityPerMinute
    , process :: Process item crafter
    , multiplier :: Double
    , input :: [ProductionLine item crafter]
    }
    deriving (Show)


mkProductionLine
    :: forall item crafter
     . ( ProcessCollection item crafter
       , Show item
       , Ord item
       , CrafterMultiplier crafter
       )
    => Map item (Process item crafter)
    -> QuantityPerMinute
    -> item
    -> ProductionLine item crafter
mkProductionLine preferredProcess i item =
    case findProcessSet @item @crafter item of
        ProcessSet mainProcess []
            | M.null mainProcess.input ->
                RawProductionLine i item
        _ ->
            mkCraftedProductionLine preferredProcess i item


mkCraftedProductionLine
    :: ( Show item
       , Ord item
       , ProcessCollection item crafter
       , CrafterMultiplier crafter
       )
    => Map item (Process item crafter)
    -> QuantityPerMinute
    -> item
    -> ProductionLine item crafter
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
    defaultQuantityPerMinute =
        scaleQuantityPerMinute (crafterMultiplier process.crafter) $
            quantityPerMinute process.cycleTime batchSize
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
    :: ( Ord item
       , Show item
       , ProcessCollection item crafter
       , CrafterMultiplier crafter
       )
    => Map item (Process item crafter)
    -> (item, QuantityPerMinute)
    -> item
    -> ProductionLine item crafter
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


scaleProduction
    :: Double
    -> ProductionLine item crafter
    -> ProductionLine item crafter
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


findProductionLineOutput
    :: (Ord item, CrafterMultiplier crafter)
    => item
    -> ProductionLine item crafter
    -> Maybe QuantityPerMinute
findProductionLineOutput item = \case
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
                    asum $ findProductionLineOutput item <$> input
                Just o ->
                    Just (scaleQuantityPerMinute (crafterMultiplier crafter) o)


getBatchSize
    :: (Show item, Ord item)
    => item
    -> Process item crafter
    -> Quantity
getBatchSize item process =
    fromMaybe
        ( error $
            "Invalid process. Missing " <> show item <> " in output"
        )
        $ M.lookup item process.output


findPreferredProcess
    :: (Ord item, ProcessCollection item crafter)
    => item
    -> Map item (Process item crafter)
    -> Process item crafter
findPreferredProcess item preferredProcess =
    fromMaybe mainProcess $
        M.lookup item preferredProcess
  where
    ProcessSet mainProcess _ = findProcessSet item


productionTree
    :: (Show crafter, Show item)
    => ProductionLine item crafter
    -> Text
productionTree =
    toText
        . drawTree
        . fmap toString
        . productionLineToTree


productionLineToTree
    :: (Show crafter, Show item)
    => ProductionLine item crafter
    -> Tree Text
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


listProductionLines
    :: (Show crafter, Show item)
    => ProductionLine item crafter
    -> Text
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


productionSummary
    :: (Show item, Ord item)
    => ProductionLine item crafter
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
    => ProductionLine item crafter
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


machineSummary
    :: (Show crafter, Ord crafter)
    => ProductionLine item crafter
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
                    , process
                    , input
                    }
                ) ->
                foldl' go (M.insertWith (+) process.crafter multiplier m) input


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


basicShowLine :: Show item => item -> QuantityPerMinute -> Text
basicShowLine item qpm =
    show item
        <> ": "
        <> show qpm


tree :: (Show crafter, Show item) => ProductionLine item crafter -> IO ()
tree = putTextLn . productionTree


summary :: (Show item, Ord item) => ProductionLine item crafter -> IO ()
summary = putTextLn . productionSummary


machines :: (Show crafter, Ord crafter) => ProductionLine item crafter -> IO ()
machines = putTextLn . machineSummary
