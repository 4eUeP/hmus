{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Hmus.Types
  ( ResourceName (..)

  , Song (..)
  , showSong
  , showDurationSec
  , defaultSong

  , PlaylistName
  , Playlist (..)

  , PlayerStatus (..)
  , playing
  , progress
  , showElapsedTimeSec
  , showPeakLevelMeter

  , PlayerProcess

  , PlayerEvent (..)
  ) where

import           Data.Maybe                  (fromMaybe)
import qualified Data.Text                   as T
import           Lens.Micro.TH               (makeLenses)
import           System.FilePath             (takeFileName)
import           System.Process              (Pid)
import           Text.Printf                 (printf)
import qualified Data.Vector as V

import qualified Sound.Sox                   as Sox


data ResourceName = PlaylistsView
                  | SongsView
                  | CommandLineView
  deriving (Ord, Eq, Show)


data Song = Song
  { songPath        :: FilePath
  , songTitle       :: T.Text
  , songArtist      :: T.Text
  , songAlbum       :: T.Text
  , songDurationSec :: Int
  }
  deriving (Show, Eq)


defaultSong :: FilePath -> Song
defaultSong fp =
  Song { songPath = fp
       , songTitle = ""
       , songArtist = ""
       , songAlbum = ""
       , songDurationSec = 0
       }


showSong :: Song -> String
showSong = takeFileName . songPath


showDurationSec' :: String -> Int -> String
showDurationSec' d sec | sec <= 0 = d
                       | otherwise = printf "%02d:%02d" m s
  where
    m = sec `div` 60
    s = sec `mod` 60


showDurationSec :: Int -> String
showDurationSec = showDurationSec' "--:--"


type PlaylistName = String


data Playlist = Playlist
  { plName  :: PlaylistName
  , plSongs :: V.Vector Song
  }
  deriving (Show, Eq)

-------------------------------------------------------------------------------

type PlayerProcess = Pid


data PlayerStatus = PlayerStatus
  { _playing  :: Bool
  , _progress :: Maybe Sox.PlayProgress
  }
  deriving (Eq, Show)

makeLenses ''PlayerStatus


-- TODO: rm
--elapsedTimeSec :: Sox.PlayProgress -> Maybe Int
--elapsedTimeSec p = (\(h, m, s) -> h * 3600 + m * 60 + s) <$> Sox.elapsedTime p


showElapsedTimeSec' :: String -> Sox.PlayProgress -> String
showElapsedTimeSec' d p = case Sox.elapsedTime p of
                            Nothing -> d
                            Just (h, m, s) -> printf "%02d:%02d" (h * 60 + m) s


showElapsedTimeSec :: Maybe Sox.PlayProgress -> String
showElapsedTimeSec p = let defaultVal = "--:--"
                       in maybe defaultVal (showElapsedTimeSec' defaultVal) p


showPeakLevelMeter' :: String -> Sox.PlayProgress -> String
showPeakLevelMeter' d p = fromMaybe d (Sox.peakLevelMeter p)


showPeakLevelMeter :: Maybe Sox.PlayProgress -> String
showPeakLevelMeter p = let defaultVal = ""
                        in maybe defaultVal (showPeakLevelMeter' defaultVal) p


data PlayerEvent = PEStatus PlayerStatus
                 | PEProcess (Maybe PlayerProcess)
                 | PEPause
                 | PEPlayNext
                 | PEPlayPrev
  deriving (Eq, Show)
