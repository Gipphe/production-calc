module Units
    ( Quantity (..)
    , items
    , cubicMeters
    , QuantityPerMinute (..)
    , itemsPerMinute
    , itemsPerSecond
    , cubicMetersPerMinute
    , cubicMetersPerSecond
    , sumQuantitiesPerMinute
    , Seconds (..)
    , quantityPerMinute
    , scaleQuantityPerMinute
    , quantityPerMinuteRatio
    ) where

import Text.Show qualified as S

import Prelude


data Quantity = Quantity
    { quantity :: Int
    , unit :: Text
    }
    deriving (Eq, Ord)


items :: Int -> Quantity
items = (`Quantity` "items")


cubicMeters :: Int -> Quantity
cubicMeters = (`Quantity` "m3")


instance Enum Quantity where
    toEnum x = Quantity (toEnum x) ""
    fromEnum (Quantity x _) = fromEnum x


instance Bounded Quantity where
    minBound = Quantity minBound ""
    maxBound = Quantity maxBound ""


instance S.Show Quantity where
    show (Quantity i unit) = show i <> " " <> toString unit


data QuantityPerMinute = QuantityPerMinute
    { throughput :: Double
    , unit :: Text
    }
    deriving (Eq, Ord)


itemsPerMinute :: Double -> QuantityPerMinute
itemsPerMinute = (`QuantityPerMinute` "items")


itemsPerSecond :: Double -> QuantityPerMinute
itemsPerSecond = itemsPerMinute . (* 60)


cubicMetersPerMinute :: Double -> QuantityPerMinute
cubicMetersPerMinute = (`QuantityPerMinute` "m3")


cubicMetersPerSecond :: Double -> QuantityPerMinute
cubicMetersPerSecond = cubicMetersPerMinute . (* 60)


mapQuantityPerMinute
    :: (Double -> Double)
    -> QuantityPerMinute
    -> QuantityPerMinute
mapQuantityPerMinute f (QuantityPerMinute t u) = QuantityPerMinute (f t) u


mergeQuantityPerMinutes
    :: (Double -> Double -> Double)
    -> QuantityPerMinute
    -> QuantityPerMinute
    -> QuantityPerMinute
mergeQuantityPerMinutes f (QuantityPerMinute t _) = mapQuantityPerMinute (f t)


instance S.Show QuantityPerMinute where
    show (QuantityPerMinute i u) = show i <> " " <> toString u


sumQuantitiesPerMinute
    :: QuantityPerMinute
    -> QuantityPerMinute
    -> QuantityPerMinute
sumQuantitiesPerMinute = mergeQuantityPerMinutes (+)


newtype Seconds = Seconds
    { getSeconds :: Double
    }
    deriving (Eq, Ord, Num, Fractional, Real, RealFrac) via Double


instance S.Show Seconds where
    show (Seconds s) = show s <> " s"


quantityPerMinute :: Seconds -> Quantity -> QuantityPerMinute
quantityPerMinute cycleTime (Quantity batchSize u) =
    QuantityPerMinute
        (fromIntegral batchSize * (60 / getSeconds cycleTime))
        (u <> "/m")


scaleQuantityPerMinute :: Double -> QuantityPerMinute -> QuantityPerMinute
scaleQuantityPerMinute multiplier (QuantityPerMinute i u) =
    QuantityPerMinute (multiplier * i) u


quantityPerMinuteRatio :: QuantityPerMinute -> QuantityPerMinute -> Double
quantityPerMinuteRatio (QuantityPerMinute x _) (QuantityPerMinute y _) = x / y
