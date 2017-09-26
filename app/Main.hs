{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}
module Main where

import Brick
  ( App(..), AttrMap, BrickEvent(..), EventM, Next, Widget, CursorLocation
  , defaultMain
  , continue, halt
  , str
  , attrMap
  )
import Brick.Widgets.Border ( borderWithLabel, hBorder, vBorder )
import Brick.Widgets.List ( List, list, renderList, handleListEvent )
import Brick.Widgets.Border.Style ( ascii )
import qualified Brick.Widgets.Core as C
import qualified Graphics.Vty as V
import Data.Vector hiding ( (++) )
import Lens.Micro.TH ( makeLenses )
import Lens.Micro ( set, over )
import System.Console.CmdArgs

import Lib

data State = State 
  { _curPath :: FilePath 
  , _filesList :: List String String
  }
makeLenses ''State

data CliArgs = CliArgs { pathArg :: FilePath, flatten :: Bool }
  deriving (Show, Data, Typeable)

defaultCliArgs = CliArgs
  { pathArg = "." &= args &= typ "DIRECTORY"
  , flatten = False }

renderFilesList :: List String String -> Widget String
renderFilesList listState =
  renderList render True listState
  where
    render selected str =
      C.vLimit 1 $ C.hBox [
        C.str (if selected then "+" else "*")
      , vBorder
      , C.str str
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
-- I can probably do this more simply, but I'm not sure how yet.
handleEvent state (VtyEvent e) = do
  newList <- handleListEvent e (_filesList state)
  let newState = set filesList newList state
  continue newState

startEvent :: State -> EventM String State
startEvent state = return state

main :: IO ()
main = do
  cliArgs <- cmdArgs defaultCliArgs
  let initialState = State {
      _curPath = pathArg cliArgs
    , _filesList = list "filesList" (generate 10 show) 1
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
