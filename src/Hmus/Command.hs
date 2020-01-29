{-# LANGUAGE OverloadedStrings #-}

module Hmus.Command
  ( UserCommand (..)
  , parseCommands
  ) where

import           Control.Monad.Fail   (fail)
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Data.Void            (Void)
import           Prelude              hiding (fail)
import           Text.Megaparsec      (eof, (<?>))
import qualified Text.Megaparsec      as P
import qualified Text.Megaparsec.Char as C

import           Hmus.Types           (PlaylistName)

-- TODO:
-- - can add non-local songs (https://...)
-- - better error messages


type Parser = P.Parsec Void Text

data UserCommand = CmdPlCreate PlaylistName
                 | CmdPlDelete PlaylistName
                 | CmdAdd FilePath
                 | CmdClear
                 | CmdPlayerPause
                 | CmdPlayerNext
                 | CmdPlayerPrev
  deriving (Eq, Show)


showErrorMsg :: (P.Stream s, P.ShowErrorComponent e)
             => P.ParseErrorBundle s e
             -> String
showErrorMsg P.ParseErrorBundle {P.bundleErrors=bundleErrors} =
  format $ foldl f "" bundleErrors
  where
    f :: (P.Stream s, P.ShowErrorComponent e)
      => String
      -> P.ParseError s e
      -> String
    f s e = s <> P.parseErrorTextPretty e
    format :: String -> String
    format = T.unpack . T.replace "\n" ", " . T.strip . T.pack


pCommands :: Parser UserCommand
pCommands = P.choice
  [ P.try $ CmdPlCreate <$> pPlCreate
  , P.try $ CmdPlDelete <$> pPlDelete
  , P.try $ CmdAdd <$> pAdd
  , P.try $ CmdClear <$ pClear
  , P.try $ CmdPlayerPause <$ pPlayerPause
  , P.try $ CmdPlayerNext <$ pPlayerNext
  , P.try $ CmdPlayerPrev <$ pPlayerPrev
  , fail "invalid command"
  ]


pPlName :: Parser PlaylistName
pPlName = P.some C.alphaNumChar <?> "name of playlist"


-- TODO
pFilePath :: Parser FilePath
pFilePath = P.some P.anySingle


pPlCreate :: Parser PlaylistName
pPlCreate = C.string "pl-create" *> C.space1 *> pPlName <* eof


pPlDelete :: Parser PlaylistName
pPlDelete = C.string "pl-delete" *> C.space1 *> pPlName <* eof


pAdd :: Parser FilePath
pAdd = C.string "add" *> C.space1 *> pFilePath <* eof


pClear :: Parser ()
pClear = C.string "clear" *> eof


pPlayerPause :: Parser ()
pPlayerPause = C.string "player-pause" *> eof


pPlayerNext :: Parser ()
pPlayerNext = C.string "player-next" *> eof


pPlayerPrev :: Parser ()
pPlayerPrev = C.string "player-prev" *> eof

-------------------------------------------------------------------------------

runCommandParser :: Parser UserCommand
                 -> Text
                 -> Either String UserCommand
runCommandParser p s =
  case P.runParser p "" s of
    Left err -> Left $ showErrorMsg err
    Right uc -> Right uc


-- TODO: more general type of input
parseCommands :: Text -> Either String UserCommand
parseCommands = runCommandParser pCommands
