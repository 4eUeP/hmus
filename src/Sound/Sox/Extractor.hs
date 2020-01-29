{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Sound.Sox.Extractor
  ( PlayProgress(..)
  , percentageCompleteL
  , elapsedTimeL
  , peakLevelMeterL
  , eopL
  -- TODO
  , percentageComplete'L

  , timeit
  , playProgressInfo
  , parsePlayOutput
  ) where

import           Brick.Types            (suffixLenses)
import           Control.Applicative    (liftA3)
import qualified Data.Text.Lazy         as L
import qualified Text.RE.Replace        as RR
import qualified Text.RE.TDFA           as RT
import           Text.RE.TDFA.Text.Lazy ((?=~))
import           Text.Read              (readMaybe)

import           Hmus.Lib.Utils         (split)


data PlayProgress = Progress
  { percentageComplete  :: Maybe Double
  , elapsedTime         :: Maybe (Int, Int, Int)
  , peakLevelMeter      :: Maybe String
  , eop                 :: Bool
  -- ^ end of progress
  , percentageComplete' :: Maybe String
  } deriving (Eq, Show)

suffixLenses ''PlayProgress

-- | Parsing time in hours, minutes and seconds.
-- >>> timeit '00:05:01.11'
-- Just (0, 5, 1)
-- >>> timeit '00'
-- Nothing
timeit :: String -> Maybe (Int, Int, Int)
timeit = f . split (== ':')
  where
    f :: [String] -> Maybe (Int, Int, Int)
    f xs = case map str2int xs of
             [h, m, s] -> liftA3 (,,) h m s
             _         -> Nothing
    str2int :: String -> Maybe Int
    str2int s = floor <$> readMaybe s

-- "In:0.00% 00:00:00.00 [00:00:05.55] Out:0     [      |      ]        Clip:0"
playProgressInfo :: L.Text -> PlayProgress
playProgressInfo s =
  Progress { percentageComplete = percentage
           , elapsedTime = elapsed
           , peakLevelMeter = meter
           , eop = eop
           , percentageComplete' = percentage'
           }
  where
    -- FIXME: more safely && elegantly
    percentage' :: Maybe String
    percentage' = L.unpack <$>
      RR.captureTextMaybe [RT.cp|1|] (s ?=~ re_percentage)

    percentage :: Maybe Double
    percentage = readMaybe =<< percentage'

    elapsed' :: Maybe String
    elapsed' =
      L.unpack <$> RR.captureTextMaybe [RT.cp|0|] (s ?=~ re_elapsed)

    elapsed :: Maybe (Int, Int, Int)
    elapsed = timeit =<< elapsed'

    meter :: Maybe String
    meter =
      L.unpack <$> RR.captureTextMaybe [RT.cp|1|] (s ?=~ re_meter)

    eop :: Bool
    eop = RT.matched $ s ?=~ [RT.re|Done|]

    re_percentage = [RT.re|In:(.*)%|]
    re_elapsed = [RT.re|[0-9]{2}:[0-9]{2}:[0-9]{2}|]
    re_meter = [RT.re|Out.*\[(.*)\]|]

parsePlayOutput :: L.Text -> [PlayProgress]
parsePlayOutput = map playProgressInfo . each
  where
    --each = L.split (`elem` ['\n', '\r']) . L.dropWhile (/= '\r')
    each = filter (/= L.pack "") . L.split (`elem` ['\n', '\r']) . L.dropWhile (/= '\r')
