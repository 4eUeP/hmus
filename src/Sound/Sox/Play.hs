{-# LANGUAGE OverloadedStrings #-}

module Sound.Sox.Play
  ( withPlay
  ) where

import           Control.Applicative (liftA2)
import qualified Data.Text.Lazy      as L
import qualified Data.Text.Lazy.IO   as LIO
import           System.Process      (ProcessHandle, withCreateProcess)
import qualified System.Process      as Proc


-- '$ play music-file.wav' is functionally equivalent to
-- '$ sox music-file.wav -d'
playProc_ :: FilePath -> Proc.CreateProcess
playProc_ fp = (Proc.proc "sox" [fp, "-d"])
  { Proc.std_out = Proc.CreatePipe
  , Proc.std_in = Proc.CreatePipe
  , Proc.std_err = Proc.CreatePipe
  }


-- sox play progress redirected to stderr (instead of stdout)
withPlay :: FilePath
          -> (ProcessHandle -> IO a)
          -> (L.Text -> IO b)
          -> IO (a, b)
withPlay fp ph_p out_p = withCreateProcess (playProc_ fp) $
  \_ _ (Just err) ph -> liftA2 (,) (ph_p ph) (out_p =<< LIO.hGetContents err)


-- TODO
--withPlayAt = undefined
