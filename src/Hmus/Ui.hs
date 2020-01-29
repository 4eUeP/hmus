{-# LANGUAGE OverloadedStrings #-}

module Hmus.Ui
  ( drawUI
  , appEvent

  , getSelectedSongs
  ) where

import qualified Brick.BChan                 as BC
import qualified Brick.Focus                 as BF
import           Brick.Main
import           Brick.Types
import           Brick.Widgets.Border
import           Brick.Widgets.Core          ((<+>), (<=>))
import qualified Brick.Widgets.Core          as BW
import qualified Brick.Widgets.List          as BL
import           Control.Concurrent
import           Control.Monad.IO.Class      (liftIO)
-- TODO: rename
import qualified Data.Text                   as T
import           Data.Vector                 ((!))
import qualified Data.Vector                 as V
import qualified Graphics.Vty                as GV
import           Graphics.Vty.Input.Events
import           Lens.Micro

import qualified Hmus.Command                as HC
import           Hmus.Player                 (St (..), commandActive,
                                              commandLine, currentFocus,
                                              currentPlaylist, currentSong,
                                              playerEventChan, playerProcess,
                                              playerStatus, playlists, songs)
import qualified Hmus.Player                 as HP
-- TODO: rename
import qualified Hmus.Types                  as T
import qualified Hmus.Ui.Theme               as UT
import qualified Hmus.Ui.Widgets.CommandLine as UC

-------------------------------------------------------------------------------

selectedSong :: St -> Maybe (Int, T.Song)
selectedSong st = if isCurrentFocusSong st
                     then listSelectedSong st
                     else Nothing


listSelectedPlaylist :: St -> Maybe (Int, T.Playlist)
listSelectedPlaylist st = BL.listSelectedElement $ st ^. playlists


listSelectedSong :: St -> Maybe (Int, T.Song)
listSelectedSong st = BL.listSelectedElement $ st ^. songs


isCurrentFocusPlaylist :: St -> Bool
isCurrentFocusPlaylist st =
  case BF.focusGetCurrent $ st ^. currentFocus of
    Just T.PlaylistsView -> True
    _                    -> False


isCurrentFocusSong :: St -> Bool
isCurrentFocusSong st =
  case BF.focusGetCurrent $ st ^. currentFocus of
    Just T.SongsView -> True
    _                -> False

-------------------------------------------------------------------------------

getSelectedSongs :: BL.List T.ResourceName T.Playlist -> V.Vector T.Song
getSelectedSongs ps =
  let m_ele = BL.listSelectedElement ps
      get (_, pl) = T.plSongs pl
   in maybe V.empty get m_ele

--getCurrentSongs :: V.Vector (BL.List T.ResourceName T.Song)
--                -> BL.List T.ResourceName T.Playlist
--                -> BL.List T.ResourceName T.Song
--getCurrentSongs plSongs pl =
--  let m_ele = BL.listSelectedElement pl
--      get (i, _) = plSongs ! i  -- assumed indexing will not out of bounds
--   in maybe HP.emptySongList get m_ele


--readCurrentSongs :: BL.List T.ResourceName T.Playlist -> IO [T.Song]
--readCurrentSongs pl =
--  case BL.listSelectedElement pl of
--    Just (_, e) -> HP.readPlaylist e  -- FIXME: slow
--    Nothing     -> return []


stopAndPlay :: St -> T.Song -> IO ()
stopAndPlay st = HP.stopAndPlay (st ^. playerEventChan) (st ^. playerProcess)


songPlaying :: St -> Maybe (Int, T.Song) -> EventM T.ResourceName St
songPlaying st (Just (idx, song)) = do
  liftIO $ forkIO $ stopAndPlay st song
  return $ st & currentSong ?~ (idx, song)
songPlaying st Nothing = return st

-------------------------------------------------------------------------------

drawUI :: St -> [Widget T.ResourceName]
drawUI st = [ui]
  where
    ui =
      BW.hBox [ BW.hLimitPercent 20 $ drawPlaylists st
              , vBorder
              , drawSongs st
              ]
      <=>
        drawCurrentPlayingBar st
      <=>
        drawPlayingStatusBar st
      <=>
        drawCommandLine st


drawPlaylists :: St -> Widget T.ResourceName
drawPlaylists st = title <=> pls
  where
    title =
      hBorderWithLabel (BW.forceAttr UT.titleBaseAttr $ BW.str "Playlists")
    pls = BL.renderListWithIndex _renderer _focusing (st ^. playlists)
    _renderer = renderPlaylist (fst <$> st ^. currentPlaylist) _focusing
    _focusing = isCurrentFocusPlaylist st


renderPlaylist :: Maybe Int   -- ^ current playing playlist's index
               -> Bool        -- ^ whether the playlists widget has focus
               -> Int -> Bool -> T.Playlist -> Widget T.ResourceName
renderPlaylist crtidx focus idx selected pl = render $ BW.padRight Max name
  where
    render = maybe id BW.forceAttr attr
    name = let n = (if current then "> " else "  ") ++ T.plName pl
            in BW.hBox [BW.str n]
    current = maybe False (idx ==) crtidx
    attr = if selected
              then if focus
                      then Just UT.playlistSelectedFocusedAttr
                      else Just UT.playlistSelectedAttr
              else Nothing
        <> if current then Just UT.currentPlaylistAttr else Nothing


drawSongs :: St -> Widget T.ResourceName
drawSongs st = BL.renderListWithIndex _renderer _focusing (st ^. songs)
  where
    _renderer = renderSong _playingSongIdx _focusing
    _focusing = isCurrentFocusSong st
    _playingSongIdx = if isThisPlSelected
                         then fst <$> st ^. currentSong
                         else Nothing
    isThisPlSelected =
      (fst <$> listSelectedPlaylist st) == (fst <$> st ^. currentPlaylist)


renderSong :: Maybe Int   -- ^ current playing song's index
           -> Bool        -- ^ whether the songs widget has focus
           -> Int -> Bool -> T.Song -> Widget T.ResourceName
renderSong crtidx focus idx selected s = render $ lft <+> rgt
  where
    lft = BW.padRight Max $ BW.hBox [BW.str $ "♪ " ++ T.showSong s]
    rgt = BW.str $ " " ++ T.showDurationSec (T.songDurationSec s)
    render = maybe id BW.forceAttr attr
    current = maybe False (idx ==) crtidx
    attr = if selected
              then if focus
                      then Just UT.songSelectedFocusedAttr
                      else Just UT.songSelectedAttr
              else Nothing
        <> if current then Just UT.currentSongAttr else Nothing


drawCurrentPlayingBar :: St -> Widget T.ResourceName
drawCurrentPlayingBar St{_currentSong=playing} =
  -- make sure there is at least one char in 'title' even if the song's
  -- Title, Artist and Album are all empty. Or this widget will disappear.
  render $ BW.hBox [ BW.padRight Max $ BW.txt title
                   , BW.txt album
                   ]
  where
    render = BW.forceAttr UT.currentPlayingBarAttr
    title = let t = prop T.songTitle
                a = prop T.songArtist
             in " " <> a <> if T.null a then "" else " - " <> t <> " "
    album = prop T.songAlbum
    prop :: (T.Song -> T.Text) -> T.Text
    prop key = maybe "" (key . snd) playing


drawPlayingStatusBar :: St -> Widget T.ResourceName
drawPlayingStatusBar st = BW.hBox [ playingIcon st
                                  , BW.padRight Max $ playingElapsedTime st
                                  , playingPeakLevelMeter st
                                  ]


playingIcon :: St -> Widget T.ResourceName
playingIcon st =
  if st ^. playerStatus . T.playing
     then BW.str " | "
     else BW.str " > "


playingElapsedTime :: St -> Widget T.ResourceName
playingElapsedTime st = BW.str $ elapsed <> " / " <> duration
  where
    elapsed = T.showElapsedTimeSec $ st ^. playerStatus . T.progress
    duration = let p = st ^. currentSong
                   s = T.showDurationSec . T.songDurationSec . snd
                in maybe "--:--" s p


playingPeakLevelMeter :: St -> Widget T.ResourceName
playingPeakLevelMeter st = let mp = st ^. playerStatus . T.progress
                            in BW.str $ T.showPeakLevelMeter mp


drawCommandLine :: St -> Widget T.ResourceName
drawCommandLine St{_commandActive=active, _commandLine=cmdl} =
  UC.renderCommandLine (BW.str . unlines) active cmdl

-------------------------------------------------------------------------------

appEvent :: St
         -> BrickEvent T.ResourceName T.PlayerEvent
         -> EventM T.ResourceName (Next St)
appEvent st (VtyEvent e) =
  if st ^. commandActive
     then case e of
            EvKey KEnter []           -> continue =<< handleCommandSubmit st
            EvKey (KChar '[') [MCtrl] -> continue =<< handleCommandExit st
            EvKey KEsc []             -> continue =<< handleCommandExit st
            _                         -> continue =<< handleCommandInput st e
     else case e of
            EvKey (KChar ':') []  -> continue =<< handleCommandStart st
            EvKey (KChar '\t') [] ->
              continue $ st & currentFocus %~ BF.focusNext
            EvKey (KChar 'q') []  -> halt st
            EvKey KEnter []       ->
              -- N.B. In this situation, the selected playlist is current
              -- playing playlist, whether playlist pane is focused or not.
              let newSt = st & currentPlaylist .~ listSelectedPlaylist st
               in continue =<< handleSongPlaying newSt
            -- XXX: also update the seleted element while playing next/prev ?
            EvKey (KChar 'e') []  -> continue =<< handlePlayingNext st
            EvKey (KChar 'b') []  -> continue =<< handlePlayingPrev st
            EvKey (KChar ' ') []  -> continue =<< handleTogglePlaying st
            _                     -> continue =<< handleListMovement st e
appEvent st (AppEvent (T.PEProcess p)) = continue $ st & playerProcess .~ p
appEvent st (AppEvent (T.PEStatus s)) =
  if HP.isEndOfPlaying s
     then continue =<< onPlayingNext st s
     else continue $ st & playerStatus .~ s
appEvent st (AppEvent T.PEPause) = continue =<< handleTogglePlaying st
appEvent st (AppEvent T.PEPlayNext) = continue =<< handlePlayingNext st
appEvent st (AppEvent T.PEPlayPrev) = continue =<< handlePlayingPrev st
appEvent st _ = continue st


-- key: Enter
handleSongPlaying :: St -> EventM T.ResourceName St
handleSongPlaying st = songPlaying st (selectedSong st)


-- key: Space
handleTogglePlaying :: St -> EventM T.ResourceName St
handleTogglePlaying st =
  let cond = st ^. playerStatus . T.playing
      action = if cond then HP.pause else HP.resume
   in case st ^. currentSong of
        Just _ -> do
          liftIO $ action $ st ^. playerProcess
          return $ st & playerStatus . T.playing .~ not cond
        Nothing -> return st


-- key: e
handlePlayingNext :: St -> EventM T.ResourceName St
handlePlayingNext st = songPlaying st (m_songs >>= m_nextSong)
  where
    m_songs = (T.plSongs . snd) <$> st ^. HP.currentPlaylist
    m_nextSong ss = HP.getNextSongCircularly ss . fst <$> st ^. currentSong


onPlayingNext :: St -> T.PlayerStatus -> EventM T.ResourceName St
onPlayingNext st s =
  let st' = st & playerProcess .~ Nothing
               & playerStatus .~ s
   in handlePlayingNext st'


-- key: b
handlePlayingPrev :: St -> EventM T.ResourceName St
handlePlayingPrev st = songPlaying st (m_songs >>= m_prevSong)
  where
    m_songs = T.plSongs . snd <$> st ^. HP.currentPlaylist
    m_prevSong ss = HP.getPrevSongCircularly ss . fst <$> st ^. currentSong


handleListMovement :: St -> Event -> EventM T.ResourceName St
handleListMovement st e =
  case BF.focusGetCurrent $ st ^. currentFocus of
    Just T.PlaylistsView -> do
      ps <- l playlists
      let ss = HP.setListElements (getSelectedSongs ps) (st ^. songs)
      return $ st & playlists .~ ps
                  & songs .~ ss
    Just T.SongsView -> do
      ss <- l songs
      return $ st & songs .~ ss
    where
      l n = BL.handleListEventVi BL.handleListEvent e (st ^. n)


handleCommandInput :: St -> Event -> EventM T.ResourceName St
handleCommandInput s =
  handleEventLensed s commandLine UC.handleCommandLineEvent


handleCommandStart :: St -> EventM T.ResourceName St
handleCommandStart st@St{_commandLine=cmdl} =
  return $ st & commandActive .~ True
              & commandLine .~ UC.commandLineClear cmdl


handleCommandExit :: St -> EventM T.ResourceName St
handleCommandExit st@St{_commandLine=cmdl} =
  return $ st & commandActive .~ False
              & commandLine .~ UC.commandLineClear cmdl


handleCommandSubmit :: St -> EventM T.ResourceName St
handleCommandSubmit st@St{_commandLine=cmdl} = do
  let content = UC.getCommandLineContent cmdl
  case HC.parseCommands $ T.pack content of
    Left errmsg -> return $ putmsg st errmsg
    Right cmd   -> do
      r <- liftIO $ HP.runUserCommand cmd st
      case r of
        Left errmsg -> return $ putmsg st errmsg
        Right st    -> return $ putmsg st ""
  where
    putmsg :: St -> String -> St
    putmsg st msg = st & commandActive .~ False
                       & commandLine .~ UC.commandLinePut msg cmdl
