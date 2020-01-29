module Hmus.Config
  ( initConfig
  , configDirectory

  , playlistsIdxFile
  , playlistsDirectory
  , themesDirectory
  ) where

import           System.Directory (XdgDirectory (XdgConfig), getXdgDirectory)
import qualified System.Directory as D
import           System.FilePath  ((</>))


initConfig :: IO ()
initConfig = D.createDirectoryIfMissing True =<< playlistsDirectory


configDirectory :: IO FilePath
configDirectory = getXdgDirectory XdgConfig "hmus" >>= D.makeAbsolute


playlistsIdxFile :: IO FilePath
playlistsIdxFile = fmap (</> "pl") configDirectory


playlistsDirectory :: IO FilePath
playlistsDirectory = fmap (</> "playlists") configDirectory


themesDirectory :: IO FilePath
themesDirectory = fmap (</> "themes") configDirectory
