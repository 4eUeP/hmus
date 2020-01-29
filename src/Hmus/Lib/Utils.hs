module Hmus.Lib.Utils
  ( strip
  , split

  , listFilesRecursive

  , sig
  , sigStop
  , sigCont
  , sigInt
  ) where

import           Control.Exception           (Exception, throw)
import           Data.List                   (isPrefixOf)
import qualified Data.Text                   as T
import qualified System.Directory            as D
import           System.FilePath             ((</>))
import           System.FilePath.Find        ((&&?), (/=?), (~~?))
import qualified System.FilePath.Find        as SF
import           System.FilePath.GlobPattern (GlobPattern)
import           System.Posix.Signals        (Signal, signalProcess)
import           System.Process              (Pid)


strip :: String -> String
strip = T.unpack . T.strip . T.pack


split :: (Char -> Bool) -> String -> [String]
split cond = map T.unpack . T.split cond . T.pack

-------------------------------------------------------------------------------

-- A '~' symbol at the beginning will be replaced by the home directory.
listFilesRecursive :: GlobPattern -> FilePath -> IO [FilePath]
listFilesRecursive pat path = SF.findWithHandler raise SF.always pred =<< p
  where
    p = parsePath path >>= D.makeAbsolute
    pred = SF.fileName ~~? pat &&? SF.fileType /=? SF.Directory

    raise :: Exception e => FilePath -> e -> IO [FilePath]
    raise = const throw


parsePath :: FilePath -> IO FilePath
parsePath path
  | path == "~" = D.getHomeDirectory
  | "~/" `isPrefixOf` path = (</> (tail . tail) path) <$> D.getHomeDirectory
  | otherwise = return path

-------------------------------------------------------------------------------

sig :: Signal -> Maybe Pid -> IO ()
sig signal = maybe (return ()) (signalProcess signal)


-- | Send @SIGSTOP@ (19) signal to the process (if have).
sigStop :: Maybe Pid -> IO ()
sigStop = sig 19


-- | Send @SIGCONT@ (18) signal to the process (if have).
sigCont :: Maybe Pid -> IO ()
sigCont = sig 18


-- | Send @SIGINT@ (2, Ctrl-C) signal to the process (if have).
sigInt :: Maybe Pid -> IO ()
sigInt = sig 2
