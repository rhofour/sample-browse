{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# OPTIONS_GHC -fno-cse #-}
module Main where

import           Brick                      (App (..), AttrMap, BrickEvent (..),
                                             CursorLocation, EventM, Next,
                                             Widget, attrMap, continue,
                                             defaultMain, halt, str)
import           Brick.Widgets.Border       (borderWithLabel, hBorder, vBorder)
import           Brick.Widgets.Border.Style (ascii)
import qualified Brick.Widgets.Core         as C
import           Brick.Widgets.List         (List, handleListEvent, list,
                                             listMoveDown, listMoveUp,
                                             listSelectedElement, renderList)
import           Control.Exception
import           Control.Monad
import qualified Data.List                  as List
import           Data.Maybe                 (isJust)
import           Data.Vector                hiding (mapM, (++))
import qualified Graphics.Vty               as V
import           Lens.Micro                 (over, set, (^.))
import           Lens.Micro.TH              (makeLenses)
import           Prelude                    hiding (map)
import           SDL.Exception              (SDLException (..))
import           SDL.Init                   (InitFlag (InitAudio))
import qualified SDL.Init
import           SDL.Mixer                  (pattern AllChannels, Chunk,
                                             defaultAudio, load, play,
                                             withAudio)
import qualified SDL.Mixer                  (halt)
import           System.Console.CmdArgs
import           System.Directory           (listDirectory)

import           Lib

data ListItem = ListItem
  { filePath :: FilePath
  , chunk    :: Maybe Chunk
  }

data State = State
  { _curPath   :: FilePath
  , _filesList :: List String ListItem
  }
makeLenses ''State

data CliArgs = CliArgs { pathArg :: FilePath, flatten :: Bool }
  deriving (Show, Data, Typeable)

defaultCliArgs = CliArgs
  { pathArg = "." &= args &= typ "DIRECTORY"
  , flatten = False }

renderFilesList :: List String ListItem -> Widget String
renderFilesList listState =
  renderList render True listState
  where
    render selected listItem =
      C.vLimit 1 $ C.hBox [
        C.str (if selected then "+" else (if isJust $ chunk listItem then "*" else "o"))
      , vBorder
      , C.str (filePath listItem)
      ]

draw :: State -> [Widget String]
draw state =
  let
    directoryLabel = C.str ("Directory: " ++ _curPath state)
    files = renderFilesList (_filesList state)
    innerWidget = C.vBox [directoryLabel, hBorder, files]
  in
    [C.withBorderStyle ascii $ borderWithLabel (C.str "sample-browse") innerWidget]

chooseCursor :: State -> [CursorLocation String] -> Maybe (CursorLocation String)
chooseCursor state _ = Nothing

playSample :: Chunk -> EventM String ()
playSample chunk = do
  SDL.Mixer.halt AllChannels
  play chunk

handleEvent :: State -> BrickEvent String () -> EventM String (Next State)
handleEvent state (VtyEvent (V.EvKey V.KEsc [])) = halt state
handleEvent state (VtyEvent (V.EvKey V.KEnter [])) = continue =<< do
  case (listSelectedElement $ state ^. filesList) of
    Just (_, ListItem {chunk=Just chunk}) -> playSample chunk
    _                                     -> return ()
  return state
handleEvent state (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt state
-- Add in vim keys
handleEvent state (VtyEvent (V.EvKey (V.KChar 'j') [])) = continue $ over filesList listMoveDown state
handleEvent state (VtyEvent (V.EvKey (V.KChar 'k') [])) = continue $ over filesList listMoveUp state
-- I can probably do this more simply, but I'm not sure how yet.
handleEvent state (VtyEvent e) = do
  newList <- handleListEvent e (_filesList state)
  let newState = set filesList newList state
  continue newState

startEvent :: State -> EventM String State
startEvent state = return state

filePathToListItem :: FilePath -> IO ListItem
filePathToListItem fp = do
  loadedChunk <- (print fp >> Just <$> load fp) `catches` [
      Handler (\(SDLCallFailed _ _ _) -> return Nothing)
    , Handler (\(ex :: IOException) -> return Nothing)
    ]
  return $ ListItem { filePath = fp , chunk = loadedChunk }

mainWithAudio = do
  cliArgs <- cmdArgs defaultCliArgs
  let dirPath = pathArg cliArgs
  filePaths <- listDirectory dirPath
  listItems <- mapM filePathToListItem filePaths
  let initialState = State {
      _curPath = dirPath
    , _filesList = list "filesList" (fromList listItems) 1
    }
  let app = App {
      appDraw = draw
    , appChooseCursor = chooseCursor
    , appHandleEvent = handleEvent
    , appStartEvent = startEvent
    , appAttrMap = const $ attrMap V.defAttr []
    }
  finalState <- defaultMain app initialState
  print cliArgs

main :: IO ()
main = do
  SDL.Init.initialize [InitAudio]
  withAudio defaultAudio 512 mainWithAudio
