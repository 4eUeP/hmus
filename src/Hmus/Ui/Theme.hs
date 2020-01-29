module Hmus.Ui.Theme
  ( attrMapping

  , textBaseAttr  -- ^ base text
  , highlightBaseAttr  -- ^ highlight text
  , titleBaseAttr
  , selectedBaseAttr

  , playlistSelectedAttr
  , playlistSelectedFocusedAttr
  , songSelectedAttr
  , songSelectedFocusedAttr
  , currentPlaylistAttr
  , currentSongAttr

  , currentPlayingBarAttr

  , progressTodoAttr
  , progressDoneAttr
  , durationAttr

  ) where

import           Brick.AttrMap (AttrMap, AttrName, attrName)
import           Brick.Themes  (Theme, newTheme, themeToAttrMap)
import           Brick.Util    (fg, on)
import qualified Graphics.Vty  as V


textBaseAttr :: AttrName
textBaseAttr = attrName "textBaseAttr"


highlightBaseAttr :: AttrName
highlightBaseAttr = attrName "highlightBaseAttr"


titleBaseAttr :: AttrName
titleBaseAttr = attrName "titleBase"


selectedBaseAttr :: AttrName
selectedBaseAttr = attrName "selectedBase"


playlistSelectedAttr, songSelectedAttr :: AttrName
playlistSelectedAttr = textBaseAttr <> attrName "playlistSelectedAttr"
songSelectedAttr = textBaseAttr <> attrName "songSelectedAttr"


playlistSelectedFocusedAttr, songSelectedFocusedAttr :: AttrName
playlistSelectedFocusedAttr = selectedBaseAttr
                           <> attrName "playlistSelectedFocusedAttr"
songSelectedFocusedAttr = selectedBaseAttr
                       <> attrName "songSelectedFocusedAttr"


currentPlaylistAttr, currentSongAttr :: AttrName
currentPlaylistAttr = highlightBaseAttr <> attrName "currentPlaylistAttr"
currentSongAttr = highlightBaseAttr <> attrName "currentSongAttr"


currentPlayingBarAttr :: AttrName
currentPlayingBarAttr = attrName "currentPlayingBarAttr"


progressDoneAttr, progressTodoAttr :: AttrName
progressDoneAttr = attrName "progressDoneAttr"
progressTodoAttr = attrName "progressTodoAttr"


durationAttr :: AttrName
durationAttr = attrName "durationAttr"


defaultTheme :: Theme
defaultTheme = newTheme (fg V.brightWhite)
    [ (textBaseAttr, fg V.brightWhite)
    , (titleBaseAttr, fg V.cyan)
    , (highlightBaseAttr, fg V.brightMagenta)
    , (selectedBaseAttr, V.black `on` V.yellow)

    -- TODO
    , (playlistSelectedAttr, V.brightWhite `on` V.cyan)
    , (playlistSelectedFocusedAttr, V.brightYellow `on` V.cyan)
    , (songSelectedAttr, V.brightWhite `on` V.cyan)
    , (songSelectedFocusedAttr, V.brightYellow `on` V.cyan)

    , (currentPlayingBarAttr, V.green `on` V.brightBlack)

    , (durationAttr, fg V.blue)
    , (progressDoneAttr, V.black `on` V.white)
    , (progressTodoAttr, V.white `on` V.black)
    ]


-- TODO: customizedTheme
-- customizedTheme <- loadCustomizations "custom.ini" defaultTheme
attrMapping :: AttrMap
attrMapping = themeToAttrMap defaultTheme
