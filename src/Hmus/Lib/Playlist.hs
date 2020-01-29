module Hmus.Lib.Playlist
  ( readPlaylists
  --, writePlaylists

  , createPlaylist
  , renamePlaylist
  , deletePlaylist
  ) where


import           Control.Monad    (filterM)
import           Data.Vector      (Vector)
import qualified Data.Vector      as V
import           System.Directory (doesFileExist)

import qualified Hmus.Config      as C
import           Hmus.Types       (Playlist (..), PlaylistName)
import qualified Hmus.Types       as T


data PlaylistInfo = PlaylistInfo
  { name      :: PlaylistName
  , songsPath :: V.Vector FilePath
  }
  deriving (Show, Eq)


readPlaylistsInfo :: IO (Vector PlaylistInfo)
readPlaylistsInfo = undefined


-- TODO:
-- - handle readFile exception
readPlaylists :: IO (Vector Playlist)
readPlaylists = return V.empty --undefined
--getPlaylists = map T.Playlist <$> (playlists >>= filterM doesFileExist)
--  where
--    playlists = lines <$> (C.playlistsIdxFile >>= readFile)


--writePlaylists = undefined

-- FIXME:
-- 1. an existed dir may cause Exception:
--    openFile: inappropriate type (Is a directory)
--doesPlaylistExist :: T.Playlist -> IO Bool
--doesPlaylistExist T.Playlist{ plName=plName } = undefined
--doesFileExist playlistPath


--fromPlaylistName :: T.PlaylistName -> IO T.Playlist
--fromPlaylistName s = T.Playlist . (</> s) <$> C.playlistsDirectory

--doesPlaylistExist' :: T.PlaylistName -> IO Bool
--doesPlaylistExist' n = fromPlaylistName n >>= doesPlaylistExist


--readPlaylist :: T.Playlist -> IO (Vector T.Song)
--readPlaylist pl@T.Playlist{..} = songs >>= mapM buildSong
--  where
--    songs :: IO [FilePath]
--    songs = filterM doesFileExist =<< lines <$> readFile playlistPath

-------------------------------------------------------------------------------
-- Manipulating playlist

--fromPlaylistName :: T.PlaylistName -> IO T.Playlist
--fromPlaylistName s = T.Playlist . (</> s) <$> C.playlistsDirectory
--
--
---- FIXME:
---- 1. an existed dir may cause Exception:
----    openFile: inappropriate type (Is a directory)
--doesPlaylistExist :: T.Playlist -> IO Bool
--doesPlaylistExist T.Playlist{..} = D.doesFileExist playlistPath
--
--
--doesPlaylistExist' :: T.PlaylistName -> IO Bool
--doesPlaylistExist' n = fromPlaylistName n >>= doesPlaylistExist
--
--
--createPlaylist :: T.Playlist -> IO (Either String ())
--createPlaylist pl@T.Playlist{..} = ioExceptionDefaultWrap action
--  where
--    action :: IO ()
--    action = do
--      exist <- doesPlaylistExist pl
--      if exist
--         then ioError $ alreadyExistsError (T.showPlaylist pl)
--         else writeFile playlistPath ""
--
--
--createPlaylist' :: T.PlaylistName -> IO (Either String ())
--createPlaylist' n = fromPlaylistName n >>= createPlaylist
--
--
--deletePlaylist :: T.Playlist -> IO (Either String ())
--deletePlaylist pl@T.Playlist{..} = tryJust pred action
--  where
--    pred :: IOException -> Maybe String
--    pred e = doesNotExist e <|> defaultExceptionPredicate e
--    doesNotExist e =
--      let msg = "Not found: " <> playlistPath
--       in guard (isDoesNotExistError e) *> Just msg
--
--    action :: IO ()
--    action = D.removeFile playlistPath
--
--
--deletePlaylist' :: T.PlaylistName -> IO (Either String ())
--deletePlaylist' n = fromPlaylistName n >>= deletePlaylist
--
--
---- TODO:
---- 1. tag the not exist playlist (insted of filtering)
---- 2. handle readFile exception
--getPlaylists :: IO [T.Playlist]
--getPlaylists = map T.Playlist <$> (playlists >>= filterM D.doesFileExist)
--  where
--    playlists = lines <$> (C.playlistsFile >>= readFile)
--
--
--putPlaylists :: Foldable t => t T.Playlist -> IO (Either String ())
--putPlaylists ps = ioExceptionDefaultWrap action
--  where
--    action = C.playlistsFile >>= flip writeFile content
--    content = let f s T.Playlist{T.playlistPath=p} = s <> p <> "\n"
--               in foldl f "" ps
--
--
--readPlaylist :: T.Playlist -> IO [T.Song]
--readPlaylist pl@T.Playlist{..} = songs >>= mapM buildSong
--  where
--    songs :: IO [FilePath]
--    songs = filterM D.doesFileExist =<< lines <$> readFile playlistPath
--
--
--writePlaylist :: Foldable t
--              => T.Playlist
--              -> t FilePath
--              -> IO (Either String ())
--writePlaylist T.Playlist{..} ss = ioExceptionDefaultWrap action
--  where
--    action = writeFile playlistPath content
--    content = let f s p = s <> p <> "\n"
--               in foldl f "" ss



-------------------------------------------------------------------------------

createPlaylist :: T.PlaylistName
               -> V.Vector T.Playlist
               -> Either String (V.Vector T.Playlist)
createPlaylist = undefined


renamePlaylist :: T.PlaylistName
               -> T.PlaylistName
               -> V.Vector T.Playlist
               -> Either String (V.Vector T.Playlist)
renamePlaylist = undefined


deletePlaylist :: T.PlaylistName
               -> V.Vector T.Playlist
               -> Either String (V.Vector T.Playlist)
deletePlaylist = undefined
