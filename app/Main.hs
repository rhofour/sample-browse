{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
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
                                             renderList)
import           Control.Exception
import           Control.Monad
import qualified Data.List                  as List
import           Data.Vector                hiding (mapM, (++))
import qualified Graphics.Vty               as V
import           Lens.Micro                 (over, set)
import           Lens.Micro.TH              (makeLenses)
import           Prelude                    hiding (map)
import           SDL.Init                   (InitFlag (InitAudio))
import qualified SDL.Init
import           SDL.Mixer                  (Music, defaultAudio, load,
                                             playMusic, withAudio)
import           System.Console.CmdArgs
import           System.Directory           (listDirectory)

import           Lib

data ListItem = ListItem
  { filePath :: FilePath
  , music    :: Either SomeException Music
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
        C.str (if selected then "+" else "*")
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

handleEvent :: State -> BrickEvent String () -> EventM String (Next State)
handleEvent state (VtyEvent (V.EvKey V.KEsc [])) = halt state
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

filePathToListItem fp = do
  -- TODO(rofer): Handle this exception more elegantly
  loadedMusic <- try $ load fp
  return $ ListItem { filePath = fp , music = loadedMusic }

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
