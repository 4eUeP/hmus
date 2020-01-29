module Hmus ( runHmus )
  where

import qualified Brick.BChan                 as BC
import qualified Brick.Focus                 as BF
import           Brick.Main                  (App (..), customMain,
                                              showFirstCursor)
import qualified Brick.Widgets.List          as BL
import qualified Graphics.Vty                as GV

import           Hmus.Config                 (initConfig)
import           Hmus.Lib.Playlist           (readPlaylists)
import           Hmus.Player                 (St (..))
import           Hmus.Types                  (PlayerEvent,
                                              PlayerStatus (PlayerStatus),
                                              ResourceName (..))
import           Hmus.Ui                     (appEvent, drawUI,
                                              getSelectedSongs)
import           Hmus.Ui.Theme               (attrMapping)
import           Hmus.Ui.Widgets.CommandLine (commandline)


runHmus :: IO ()
runHmus = do
  initConfig
  initialState <- buildInitialState
  let buildVty = GV.mkVty GV.defaultConfig
  initialVty <- buildVty
  endState <- customMain
                initialVty
                buildVty
                (Just $ _playerEventChan initialState)
                hmusApp
                initialState
  return ()


buildInitialState :: IO St
buildInitialState = do
  ec <- BC.newBChan 10
  ps <- flip (BL.list PlaylistsView) 1 <$> readPlaylists
  let ss = BL.list SongsView (getSelectedSongs ps) 1

  return St
    { _playerEventChan = ec
    , _playlists = ps
    , _songs = ss
    , _currentFocus = BF.focusRing [PlaylistsView, SongsView]
    , _currentPlaylist = Nothing
    , _currentSong = Nothing
    , _playerProcess = Nothing
    , _playerStatus = PlayerStatus False Nothing
    , _commandLine = commandline CommandLineView ":" ""
    , _commandActive = False
    }


hmusApp :: App St PlayerEvent ResourceName
hmusApp =
  App { appDraw = drawUI
      , appChooseCursor = showFirstCursor
      , appHandleEvent = appEvent
      , appStartEvent = return
      , appAttrMap = const attrMapping
      }
