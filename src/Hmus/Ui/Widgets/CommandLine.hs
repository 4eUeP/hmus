{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module Hmus.Ui.Widgets.CommandLine
  ( CommandLine
  , commandline
  , renderCommandLine
  , commandLinePut
  , commandLineClear
  , getCommandLineContent
  , handleCommandLineEvent

  -- better treat as private functions
  , applyEdit
  , getCommandLineContents
  ) where

import           Brick                     ((<+>))
import qualified Brick                     as B
import           Brick.Types               (EventM, Widget, suffixLenses)
import qualified Brick.Widgets.Edit        as BE
import           Data.Function             ((&))
import qualified Data.Text.Zipper          as Z hiding (textZipper)
import qualified Data.Text.Zipper.Generic  as Z
import           Graphics.Vty.Input.Events (Key (..), Modifier (..))
import qualified Graphics.Vty.Input.Events as V
import           Lens.Micro                ((%~), (^.))


data CommandLine t n =
  CommandLine { placeholder :: String
              , editor      :: BE.Editor t n
              }
  deriving (Show)

suffixLenses ''CommandLine


instance B.Named (CommandLine t n) n where
  getName = B.getName . editor


commandline :: Z.GenericTextZipper t
            => n
            -> String
            -> t
            -> CommandLine t n
commandline name p s =
  CommandLine { placeholder = p
              , editor = BE.editor name (Just 1) s
              }


renderCommandLine
  :: (Ord n, Show n, Monoid t, B.TextWidth t, Z.GenericTextZipper t)
  => ([t] -> Widget n)
  -> Bool
  -> CommandLine t n
  -> Widget n
renderCommandLine draw False c = BE.renderEditor draw False (c ^. editorL)
renderCommandLine draw True c =
  B.str (c ^. placeholderL) <+> BE.renderEditor draw True (c ^. editorL)


applyEdit :: (Z.TextZipper t -> Z.TextZipper t)
          -> CommandLine t n
          -> CommandLine t n
applyEdit f c = c & editorL . BE.editContentsL %~ f


commandLinePut :: (Eq t, Monoid t, Z.GenericTextZipper t)
               => t
               -> CommandLine t n
               -> CommandLine t n
commandLinePut s = applyEdit $ const . Z.gotoEOL $ Z.textZipper [s] (Just 1)


commandLineClear :: (Eq t, Monoid t)
                 => CommandLine t n
                 -> CommandLine t n
commandLineClear = applyEdit Z.killToBOL


getCommandLineContents :: Monoid t => CommandLine t n -> [t]
getCommandLineContents c = BE.getEditContents $ c ^. editorL


getCommandLineContent :: Monoid t => CommandLine t n -> t
getCommandLineContent = mconcat . getCommandLineContents


-- TODO: auto complete, history ...

handleCommandLineEvent :: (Eq t, Monoid t)
                       => V.Event
                       -> CommandLine t n
                       -> EventM n (CommandLine t n)
handleCommandLineEvent e c =
  let f = case e of
            V.EvKey (KChar 'a') [MCtrl] -> Z.gotoBOL
            V.EvKey (KChar 'e') [MCtrl] -> Z.gotoEOL
            V.EvKey (KChar 'd') [MCtrl] -> Z.deleteChar
            V.EvKey (KChar 'k') [MCtrl] -> Z.killToEOL
            V.EvKey (KChar 'u') [MCtrl] -> Z.killToBOL
            V.EvKey KDel []             -> Z.deleteChar
            V.EvKey KLeft []            -> Z.moveLeft
            V.EvKey KRight []           -> Z.moveRight
            V.EvKey KBS []              -> Z.deletePrevChar
            V.EvKey (KChar c) []        | c /= '\t' -> Z.insertChar c
            _                           -> id
  in return $ applyEdit f c
