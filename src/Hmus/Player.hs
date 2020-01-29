{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Hmus.Player
  ( St (..)
  -- lens
  , playlists
  , songs
  , currentFocus
  , playerStatus
  , playerProcess
  , currentPlaylist
  , currentSong
  , commandActive
  , commandLine
  , playerEventChan

  --
  , setListElements

  --
  , play
  , pause
  , resume
  , stop
  , stopAndPlay
  , getNextSongCircularly
  , getPrevSongCircularly

  --
  , popPlayerPid
  , popPlayerStatus
  , isEndOfPlaying

  -- * User command
  , runUserCommand

  -- * Manipulating playlist
  --, module Hmus.Playlist

  ) where

import qualified Brick.BChan                 as BC
import qualified Brick.Focus                 as BF
import qualified Brick.Widgets.List          as BL
import           Control.Applicative         (liftA2, (<|>))
import           Control.Monad               (filterM, guard, void)
import qualified Data.Text.Lazy              as L
import qualified Data.Vector                 as V
import           Lens.Micro                  ((&), (.~), (^.))
import           Lens.Micro.TH               (makeLenses)
import qualified System.Directory            as D
import           System.FilePath             ((</>))
import           System.IO.Error             (isDoesNotExistError)
import qualified System.Process              as Proc

import qualified Hmus.Command                as HC
import qualified Hmus.Config                 as C
import qualified Hmus.Lib.Playlist           as HPL
import qualified Hmus.Lib.Utils              as U
import qualified Hmus.Types                  as T
import qualified Hmus.Ui.Widgets.CommandLine as UC
import qualified Sound.Data.Information      as SI
import qualified Sound.Sox                   as Sox


-- App state
data St =
  St { _playlists       :: BL.List T.ResourceName T.Playlist
     , _songs           :: BL.List T.ResourceName T.Song
     , _currentFocus    :: BF.FocusRing T.ResourceName

     , _currentPlaylist :: Maybe (Int, T.Playlist)
     -- ^ playing playlist, pair of index and 'Hmus.Types.Playlist'
     , _currentSong     :: Maybe (Int, T.Song)
     -- ^ playing song, pair of index and 'Hmus.Types.Song'

     , _playerProcess   :: Maybe T.PlayerProcess
     , _playerStatus    :: T.PlayerStatus

     , _commandLine     :: UC.CommandLine String T.ResourceName
     , _commandActive   :: Bool

     , _playerEventChan :: BC.BChan T.PlayerEvent
     }

makeLenses ''St


setListElements :: (Show n) => V.Vector a -> BL.List n a -> BL.List n a
setListElements xs list = list & BL.listElementsL .~ xs


setStPlaylist :: V.Vector T.Playlist -> St -> St
setStPlaylist xs = setStPlaylistBy (const xs)


setStPlaylistBy :: (V.Vector T.Playlist -> V.Vector T.Playlist) -> St -> St
setStPlaylistBy f st = st & playlists .~ setListElements xs ps
  where
    xs = f $ BL.listElements ps
    ps = st ^. playlists


setStSonglist :: V.Vector T.Song -> St -> St
setStSonglist xs = setStSonglistBy (const xs)


setStSonglistBy :: (V.Vector T.Song -> V.Vector T.Song) -> St -> St
setStSonglistBy f st = st & songs .~ setListElements xs ps
  where
    xs = f $ BL.listElements ps
    ps = st ^. songs

-------------------------------------------------------------------------------

runUserCommand :: HC.UserCommand -> St -> IO (Either String St)
runUserCommand (HC.CmdPlCreate n) = cmdCreatePlaylist n
runUserCommand (HC.CmdPlDelete n) = cmdDeletePlaylist n
runUserCommand (HC.CmdAdd fp)     = cmdAdd fp
runUserCommand HC.CmdClear        = cmdClear
runUserCommand HC.CmdPlayerPause  = cmdPlayerPause
runUserCommand HC.CmdPlayerNext   = cmdPlayerNext
runUserCommand HC.CmdPlayerPrev   = cmdPlayerPrev


setPlHelper :: (V.Vector T.Playlist -> Either String (V.Vector T.Playlist))
            -> St
            -> IO (Either String St)
setPlHelper f st =
  pure $ flip setStPlaylist st <$> f (BL.listElements (st ^. playlists))


cmdCreatePlaylist :: T.PlaylistName -> St -> IO (Either String St)
cmdCreatePlaylist = setPlHelper . HPL.createPlaylist


cmdDeletePlaylist :: T.PlaylistName -> St -> IO (Either String St)
cmdDeletePlaylist = setPlHelper . HPL.deletePlaylist


cmdRenamePlaylist :: T.PlaylistName
                  -> T.PlaylistName
                  -> St
                  -> IO (Either String St)
cmdRenamePlaylist = undefined


cmdAdd :: FilePath -> St -> IO (Either String St)
cmdAdd = undefined

cmdClear :: St -> IO (Either String St)
cmdClear = undefined




--emptySongList :: BL.List T.ResourceName T.Song
--emptySongList = BL.list T.SongsView V.empty 1

--cmdPlCreate :: T.PlaylistName -> St -> IO (Either String St)
--cmdPlCreate n st =
--  let ps e = gListAppend e (st ^. playlists)
--      emptySong = V.singleton emptySongList
--      updatePlSongs st = st & plSongs .~ (st ^. plSongs <> emptySong)
--   in do
--     pl <- T.Playlist n V.empty
--     create <- createPlaylist pl
--     save <- _savePlaylists st (ps pl)
--     return $ create >> save >>= Right . updatePlSongs
--
--
--cmdPlDelete :: T.PlaylistName -> St -> IO (Either String St)
--cmdPlDelete n st =
--  -- yield the index of the first element, because there should not have
--  -- two playlists which have the same name.
--  let idx e = listFindIndex (== e) (st ^. playlists)
--      savePl e = case idx e of
--                   Just i  -> let l = BL.listRemove i (st ^. playlists)
--                               in do
--                                 mst <- _savePlaylists st l
--                                 return $ mst >>= Right . updatePlSongs i
--                   Nothing -> return $ Left "No such playlist"
--      updatePlSongs i ast = ast & plSongs .~ vctRemove i (ast ^. plSongs)
--      vctRemove i xs = let (hs, ts) = V.splitAt i xs
--                        in hs <> V.tail ts
--   in do
--     pl <- fromPlaylistName n
--     delete <- deletePlaylist pl
--     save <- savePl pl
--     return $ delete >> save
--
--
--_savePlaylists :: St
--               -> BL.List T.ResourceName T.Playlist
--               -> IO (Either String St)
--_savePlaylists st ps =
--  let updateSt ps = st & commandActive .~ False
--                       & playlists .~ ps
--   in putPlaylists ps >>= \r -> return (r >> Right (updateSt ps))
--
--
---- TODO: performance improvement
--cmdAdd :: FilePath -> St -> IO (Either String St)
--cmdAdd fp st = findSongs fp >>= save
--  where
--    xs = BL.listElements . ((st ^. plSongs) V.!) . fst <$> selectedPl
--    save ys = case liftA2 (,) selectedPl (liftA2 (<>) xs ys) of
--                Left x       -> return $ Left x
--                Right (p, r) -> _saveSongs st r p
--    selectedPl = maybe (Left "Select a playlist first") Right selectedPl'
--    selectedPl' = BL.listSelectedElement $ st ^. playlists
--
--
---- FIXME: try BL.listClear?
--cmdClear :: St -> IO (Either String St)
--cmdClear st = case selectedPl of
--                Left x  -> return $ Left x
--                Right x -> _saveSongs st V.empty x
--  where
--    selectedPl = maybe (Left "Select a playlist first") Right selectedPl'
--    selectedPl' = BL.listSelectedElement $ st ^. playlists
--
--
--_saveSongs :: St
--           -> V.Vector T.Song
--           -> (Int, T.Playlist)
--           -> IO (Either String St)
--_saveSongs st ss (idx, pl) = write >>= \r -> return (r >> Right updateSt)
--  where
--    write = writePlaylist pl (T.songPath <$> ss)
--    updateSt = st & commandActive .~ False
--                  & songs .~ songWidget
--                  & plSongs .~ updatedPlSongs
--    updatedPlSongs = V.update (st ^. plSongs) (V.singleton (idx, songWidget))
--    songWidget = BL.list T.SongsView ss 1


cmdPlayerPause :: St -> IO (Either String St)
cmdPlayerPause st =
  Right st <$ writePlayerEvent (st ^. playerEventChan) T.PEPause


cmdPlayerPrev :: St -> IO (Either String St)
cmdPlayerPrev st =
  Right st <$ writePlayerEvent (st ^. playerEventChan) T.PEPlayPrev


cmdPlayerNext :: St -> IO (Either String St)
cmdPlayerNext st =
  Right st <$ writePlayerEvent (st ^. playerEventChan) T.PEPlayNext

-------------------------------------------------------------------------------

popPlayerPid :: BC.BChan T.PlayerEvent -> Proc.ProcessHandle -> IO ()
popPlayerPid chan hdl = Proc.getPid hdl >>= writePlayerEvent chan . T.PEProcess

popPlayerStatus :: BC.BChan T.PlayerEvent -> L.Text -> IO ()
popPlayerStatus chan s = mapM_ (writePlayerEvent chan . buildPlayerEvent) ps
  where
    ps = Sox.parsePlayOutput s
    buildPlayerEvent p = T.PEStatus $ T.PlayerStatus True (Just p)


isEndOfPlaying :: T.PlayerStatus -> Bool
isEndOfPlaying s = maybe False Sox.eop $ s ^. T.progress

-------------------------------------------------------------------------------

play :: BC.BChan T.PlayerEvent -> T.Song -> IO ()
play chan s = void $
  Sox.withPlay (T.songPath s) (popPlayerPid chan) (popPlayerStatus chan)


pause :: Maybe Proc.Pid -> IO ()
pause = U.sigStop


resume :: Maybe Proc.Pid -> IO ()
resume = U.sigCont


stop :: Maybe Proc.Pid -> IO ()
stop = U.sigInt


stopAndPlay :: BC.BChan T.PlayerEvent -> Maybe Proc.Pid -> T.Song -> IO ()
stopAndPlay chan maybe_pid s = stop maybe_pid >> play chan s


getNextSongCircularly :: V.Vector T.Song -> Int -> (Int, T.Song)
getNextSongCircularly ss idx = (idx', ss V.! idx')
  where idx' = (idx + 1) `mod` len
        len = V.length ss


getPrevSongCircularly :: V.Vector T.Song -> Int -> (Int, T.Song)
getPrevSongCircularly ss idx = (idx', ss V.! idx')
  where idx' = (idx + len - 1) `mod` len
        len = V.length ss

-------------------------------------------------------------------------------
-- helper functions

gListAppend :: (BL.Splittable t, Applicative t, Semigroup (t e), Foldable t)
           => e
           -> BL.GenericList n t e
           -> BL.GenericList n t e
gListAppend e xs = BL.listInsert (length xs) e xs


listFilter :: (e -> Bool)
           -> BL.GenericList n V.Vector e
           -> BL.GenericList n V.Vector e
listFilter pred xs = BL.list n t 1
  where
    n = BL.listName xs
    t = V.filter pred (BL.listElements xs)


listFindIndex :: (e -> Bool)
              -> BL.GenericList n V.Vector e
              -> Maybe Int
listFindIndex f l = V.findIndex f (BL.listElements l)


writePlayerEvent :: BC.BChan T.PlayerEvent -> T.PlayerEvent -> IO ()
writePlayerEvent = BC.writeBChan


writePlayerEvents :: BC.BChan T.PlayerEvent -> [T.PlayerEvent] -> IO ()
writePlayerEvents = mapM_ . writePlayerEvent


--exampleShowOut :: L.Text -> IO ()
--exampleShowOut = mapM_ print . Sox.parsePlayOutput
--
--exampleShowPid :: Proc.ProcessHandle -> IO ()
--exampleShowPid = (print =<<) . Proc.getPid
