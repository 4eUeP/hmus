{-# LANGUAGE OverloadedStrings #-}

module Sound.Data.Information
  ( getAudioTrack
  , getAudioTracks

  , AudioTrack (..)
  , defaultAudioTrack
  , audioTrackGetter
  ) where

import           Data.Text     (Text)
import qualified Sound.HTagLib as TL


-- N.B. Duration of tracks that TagLib returns is an integer number of seconds.
-- This means that if you want to calculate total duration, you'll have slightly
-- incorrect result. Proper solution is to extract duration as floating-point
-- number.
-- TODO: try this <https://hackage.haskell.org/package/hsndfile>
-- to get more precise duration.
data AudioTrack = AudioTrack
  { atTitle       :: Text
  , atArtist      :: Text
  , atAlbum       :: Text
  , atGenre       :: Text
  , atYear        :: Maybe Int
  , atTrackNumber :: Maybe Int
  , atDuration    :: Int
  }
  deriving Show


defaultAudioTrack = AudioTrack
  { atTitle = ""
  , atArtist = ""
  , atAlbum = ""
  , atGenre = ""
  , atYear = Nothing
  , atTrackNumber = Nothing
  , atDuration = 0
  }

audioTrackGetter :: TL.TagGetter AudioTrack
audioTrackGetter = AudioTrack
  <$> fmap TL.unTitle TL.titleGetter
  <*> fmap TL.unArtist TL.artistGetter
  <*> fmap TL.unAlbum TL.albumGetter
  <*> fmap TL.unGenre TL.genreGetter
  <*> (fmap . fmap) TL.unYear TL.yearGetter
  <*> (fmap . fmap) TL.unTrackNumber TL.trackNumberGetter
  <*> fmap TL.unDuration TL.durationGetter


getAudioTrack :: FilePath -> IO AudioTrack
getAudioTrack = flip TL.getTags audioTrackGetter


getAudioTracks :: [FilePath] -> IO [AudioTrack]
getAudioTracks = mapM getAudioTrack


-- TODO
--getAudioTracksConcurrently :: [FilePath] -> IO [AudioTrack]
--getAudioTracksConcurrently = undefined
