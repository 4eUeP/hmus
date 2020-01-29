module Hmus.Lib.Song
  ( buildSong
  , findSongs
  ) where

import qualified Data.Vector            as V

import qualified Hmus.Lib.Exception     as E
import qualified Hmus.Lib.Utils         as U
import           Hmus.Types             (Song (..))
import qualified Hmus.Types             as T
import qualified Sound.Data.Information as SI


buildSong :: FilePath -> IO Song
buildSong path = do
  at <- E.tryIO SI.defaultAudioTrack (SI.getAudioTrack path)
  return $ Song { songPath = path
                , songTitle = SI.atTitle at
                , songArtist = SI.atArtist at
                , songAlbum = SI.atAlbum at
                , songDurationSec = SI.atDuration at
                }


findSongs :: FilePath -> IO (Either String (V.Vector T.Song))
findSongs fp =
  E.ioExceptionDefaultWrap $ V.fromList <$> findSongs' fp >>= mapM buildSong


-- TODO
-- 1. find by file type instead of file extension.
-- 2. return all supported audios.
findSongs' :: FilePath -> IO [FilePath]
findSongs' = U.listFilesRecursive "*.(flac|mp3|ogg|wav)"


--findSongByIndex :: BL.List T.ResourceName T.Song -> Int -> T.Song
--findSongByIndex = (V.!) . BL.listElements


--safeFindSongByIndex :: BL.List T.ResourceName T.Song -> Int -> Maybe T.Song
--safeFindSongByIndex = (V.!?) . BL.listElements
