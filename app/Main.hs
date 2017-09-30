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
import qualified Data.Vector                as V
import qualified Graphics.Vty               as Vty
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
import           System.Directory           (doesDirectoryExist, listDirectory)
import           System.FilePath            (takeFileName)
import qualified System.FilePath            as Filepath

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

data CliArgs = CliArgs { pathArg :: FilePath, flatten :: Bool, showAll :: Bool }
  deriving (Show, Data, Typeable)

defaultCliArgs = CliArgs
  { pathArg = "." &= args &= typ "DIRECTORY"
  , flatten = False
  , showAll = False }

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

playSampleIfPossible :: State -> EventM String (Next State)
playSampleIfPossible state = continue =<< do
  case (listSelectedElement $ state ^. filesList) of
    Just (_, ListItem {chunk=Just chunk}) -> playSample chunk
    _                                     -> return ()
  return state


handleEvent :: State -> BrickEvent String () -> EventM String (Next State)
handleEvent state (VtyEvent (Vty.EvKey Vty.KEsc [])) = halt state
handleEvent state (VtyEvent (Vty.EvKey Vty.KEnter [])) = playSampleIfPossible state
handleEvent state (VtyEvent (Vty.EvKey (Vty.KChar ' ') [])) = playSampleIfPossible state
handleEvent state (VtyEvent (Vty.EvKey (Vty.KChar 'q') [])) = halt state
handleEvent state (VtyEvent (Vty.EvKey (Vty.KChar 'c') [Vty.MCtrl])) = halt state
-- Add in vim keys
handleEvent state (VtyEvent (Vty.EvKey (Vty.KChar 'j') [])) = continue $ over filesList listMoveDown state
handleEvent state (VtyEvent (Vty.EvKey (Vty.KChar 'k') [])) = continue $ over filesList listMoveUp state
-- I can probably do this more simply, but I'm not sure how yet.
handleEvent state (VtyEvent e) = do
  newList <- handleListEvent e (_filesList state)
  let newState = set filesList newList state
  continue newState

startEvent :: State -> EventM String State
startEvent state = return state

-- Load a chunk if possible and shorten the filepaths
filePathToListItem :: Int -> FilePath -> IO ListItem
filePathToListItem curDirLen fp = do
  loadedChunk <- (print fp >> Just <$> load fp) `catches` [
      Handler (\(SDLCallFailed _ _ _) -> return Nothing)
    , Handler (\(ex :: IOException) -> return Nothing)
    ]
  return $ ListItem {
     filePath = drop curDirLen fp
   , chunk = loadedChunk }

listFilesRecursively :: Int -> FilePath -> IO [FilePath]
listFilesRecursively 0 baseDir = return [baseDir]
listFilesRecursively n baseDir = do
  isDir <- doesDirectoryExist baseDir
  case isDir of
    False -> return [baseDir]
    True -> do
      newDirs <- (fmap $ Filepath.combine baseDir) <$> listDirectory baseDir
      fmap concat $ mapM (listFilesRecursively (n-1)) newDirs

mainWithAudio = do
  cliArgs <- cmdArgs defaultCliArgs
  let dirPath = pathArg cliArgs
  let recursionDepth = if flatten cliArgs then 100 else 1
  filePaths <- listFilesRecursively recursionDepth dirPath
  listItems <- mapM (filePathToListItem (length dirPath)) filePaths
  let listItems' = if (showAll cliArgs) then listItems else filter (isJust . chunk) listItems
  let initialState = State {
      _curPath = dirPath
    , _filesList = list "filesList" (V.fromList listItems') 1
    }
  let app = App {
      appDraw = draw
    , appChooseCursor = chooseCursor
    , appHandleEvent = handleEvent
    , appStartEvent = startEvent
    , appAttrMap = const $ attrMap Vty.defAttr []
    }
  finalState <- defaultMain app initialState
  print cliArgs

main :: IO ()
main = do
  SDL.Init.initialize [InitAudio]
  withAudio defaultAudio 512 mainWithAudio
